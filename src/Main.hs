{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TemplateHaskell   #-}
import           ClassyPrelude.Conduit
import qualified Data.ByteString.Builder as BB
import           Data.Conduit.Blaze      (builderToByteString)
import           Data.Conduit.Network    (AppData, appSink, appSource,
                                          clientSettings, runTCPClient,
                                          runTCPServer, serverSettings)
import           Data.Word8              (_0, _9, _colon, _hyphen)
import           System.Directory        (canonicalizePath,
                                          createDirectoryIfMissing,
                                          removeFile, doesFileExist)
import           System.FilePath         (takeDirectory)
import qualified System.FSNotify         as FS
import           System.IO               (IOMode (ReadMode), hFileSize,
                                          openBinaryFile)
import Paths_dumb_file_mirror (version)
import Options.Applicative.Simple (simpleVersion, simpleOptions, argument, auto, str, metavar, addCommand)

desc :: String
desc = unlines
    [ "This program will mirror local file changes to a remote host."
    , "By keeping a persistent TCP connection open between the local"
    , "and remote machines, latency is reduced versus more naive"
    , "solutions, like combining inotify and rsync."
    , "Note that this tool does not perform an initial file copy, if"
    , "needed you should do an explicit scp -r before using this tool."
    ]

main :: IO ()
main = do
    ((), cmd) <- simpleOptions
        $(simpleVersion version)
        "dumb-file-mirror: Mirror file changes to a local host"
        desc
        (pure ()) $ do
            addCommand "remote" "Receive file changes" id $ remote
                <$> portArg
                <*> dirArg
            addCommand "local" "Send file changes" id $ local
                <$> hostArg
                <*> portArg
                <*> dirArg
    cmd
  where
    hostArg = argument str (metavar "HOST")
    portArg = argument auto (metavar "PORT")
    dirArg = argument str (metavar "DIRECTORY")

remote :: Int -- ^ port
       -> FilePath -- ^ root directory
       -> IO ()
remote port dir =
    runTCPServer (serverSettings port "*") (handleAny print . run)
  where
    run :: AppData -> IO ()
    run appData = runResourceT $ appSource appData $$ forever (recvFile dir)

local :: String -- ^ host
      -> Int -- ^ port
      -> FilePath -- ^ root directory
      -> IO ()
local host port dir = runTCPClient (clientSettings port hostBytes) $ \appData -> do
    dir' <- canonicalizePath dir
    FS.withManager $ \man -> do
        chan <- newTChanIO
        void $ FS.watchTree man dir (const True) $ \event -> do
            let fp = FS.eventPath event
                Just suffix = stripPrefix dir' fp >>= stripPrefix "/"
            atomically $ writeTChan chan suffix
        let src = forever $ do
                suffix <- atomically $ readTChan chan
                sendFile dir' suffix
        runResourceT $ src $$ builderToByteString =$ appSink appData
  where
    hostBytes = encodeUtf8 (pack host)

sendInteger :: Monad m => Integer -> Producer m BlazeBuilder
sendInteger i = yield $ BB.integerDec i <> BB.word8 _colon

sendFile :: MonadResource m
         => FilePath -- ^ root
         -> FilePath -- ^ relative
         -> Producer m BlazeBuilder
sendFile root fp = do
    sendInteger $ fromIntegral $ length fpBS
    yield $ toBuilder fpBS

    exists <- liftIO $ doesFileExist fpFull

    if exists
        then bracketP (openBinaryFile fpFull ReadMode) hClose $ \h -> do
            size <- liftIO $ hFileSize h
            sendInteger size
            sourceHandle h =$= mapC BB.byteString
        else sendInteger (-1)
    yield flushBuilder
  where
    fpFull = root </> fp
    fpBS = encodeUtf8 (pack fp :: Text)

recvInteger :: (MonadThrow m, Integral i) => Consumer ByteString m i
recvInteger = do
    (isNeg, x) <- (takeWhileCE (/= _colon) =$= foldMCE addDigit (False, 0))
                  <* getColon
    return $! if isNeg then negate x else x
  where
    addDigit (isNeg, total) w
        | _0 <= w && w <= _9 = return $! (isNeg, total * 10 + fromIntegral (w - _0))
        | w == _hyphen = return $! (True, total)
        | otherwise = throwM (InvalidByte w)

    getColon = do
        mw <- headCE
        unless (mw == Just _colon) (throwM (MissingColon mw))

recvFile :: MonadResource m
         => FilePath -- ^ root
         -> Consumer ByteString m ()
recvFile root = do
    fpLen <- recvInteger
    fpRelText <- takeCE fpLen =$= decodeUtf8C =$= foldC
    let fp = root </> unpack fpRelText
    fileLen <- recvInteger
    if fileLen == (-1)
        then liftIO $ void $ tryIO $ removeFile fp
        else do
            liftIO $ createDirectoryIfMissing True $ takeDirectory fp
            takeCE fileLen =$= sinkFile fp

data RecvIntegerException = InvalidByte Word8
                          | MissingColon (Maybe Word8)
    deriving (Show, Typeable)
instance Exception RecvIntegerException
