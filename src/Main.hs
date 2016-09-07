#!/usr/bin/env stack
{- stack
    --resolver lts-6.15
    --install-ghc
    runghc

    --package classy-prelude-conduit
    --package word8
    --package fsnotify
    --package optparse-simple
    --package hspec
    --package temporary
-}

{-

Usually, a program like this would be broken up into a library, add
test suites, and finally layer an executable on top of the whole
thing. However, one of the goals was to make it easy to review and
play with the code, so I kept it all in one file. This also makes it
possible to run this program as a script instead of compiling it as an
executable, which is what the above Stack information is about.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}
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
import           System.FilePath         (takeDirectory, addTrailingPathSeparator)
import qualified System.FSNotify         as FS
import           System.IO               (IOMode (ReadMode), hFileSize,
                                          openBinaryFile)
import           System.Environment      (withArgs)
import Paths_dumb_file_mirror (version)
import Options.Applicative.Simple (simpleVersion, simpleOptions, argument, auto, str, metavar, addCommand)
import Test.Hspec (hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import System.IO.Temp (withSystemTempDirectory)

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
            addCommand "test" "Run the test suite" id $ pure spec
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
    run appData = runResourceT $ appSource appData $$ foreverCE (recvFile dir)

local :: String -- ^ host
      -> Int -- ^ port
      -> FilePath -- ^ root directory
      -> IO ()
local host port dir = runTCPClient (clientSettings port hostBytes) $ \appData ->
    runResourceT
        $ sourceFileChanges dir
       $$ awaitForever (sendFile dir)
       =$ builderToByteString
       =$ appSink appData
  where
    hostBytes = encodeUtf8 (pack host)

---------------------------------------
-- CONDUIT UTILITY FUNCTIONS
---------------------------------------

sourceFileChanges :: MonadResource m
                  => FilePath
                  -> Producer m FilePath
sourceFileChanges root = bracketP FS.startManager FS.stopManager $ \man -> do
    root' <- liftIO $ canonicalizePath root
    chan <- liftIO newTChanIO
    liftIO $ void $ FS.watchTree man root' (const True) $ \event -> do
        let fp = FS.eventPath event
        case stripPrefix (addTrailingPathSeparator root') fp of
            Nothing -> error $ "sourceFileChanges: prefix not found " ++ show (root', fp)
            Just suffix
                | null suffix -> return ()
                | otherwise -> atomically $ writeTChan chan suffix
    forever $ do
        suffix <- atomically $ readTChan chan
        yield suffix

foreverCE :: Monad m => Sink ByteString m () -> Sink ByteString m ()
foreverCE inner =
    loop
  where
    loop = do
        mnext <- peekCE
        case mnext of
            Nothing -> return ()
            Just _next -> do
                inner
                loop

sendInteger :: Monad m => Integer -> Producer m BlazeBuilder
sendInteger i = yield $ BB.integerDec i <> BB.word8 _colon

sendFile :: MonadResource m
         => FilePath -- ^ root
         -> FilePath -- ^ relative
         -> Producer m BlazeBuilder
sendFile root fp = do
    sendInteger $ fromIntegral $ length fpBS
    yield $ toBuilder fpBS

    let open = tryIO $ openBinaryFile fpFull ReadMode
        close (Left _err) = return ()
        close (Right h) = hClose h

    bracketP open close $ \eh ->
        case eh of
            Left _ex -> sendInteger (-1)
            Right h -> do
                size <- liftIO $ hFileSize h
                sendInteger size
                sourceHandle h =$= mapC BB.byteString
    yield flushBuilder
  where
    fpFull = root </> fp
    fpBS = encodeUtf8 (pack fp :: Text)

recvInteger :: (MonadThrow m, Integral i) => Sink ByteString m i
recvInteger = do
    mnext <- peekCE
    next <-
        case mnext of
            Nothing -> throwM EndOfStream
            Just next -> return next
    isNeg <-
        if next == _hyphen
            then do
                dropCE 1
                return True
            else return False

    x <- (takeWhileCE (/= _colon) =$= foldMCE addDigit 0)

    mw <- headCE
    unless (mw == Just _colon) (throwM (MissingColon mw))

    return $! if isNeg then negate x else x
  where
    addDigit total w
        | _0 <= w && w <= _9 = return (total * 10 + fromIntegral (w - _0))
        | otherwise = throwM (InvalidByte w)

recvFile :: MonadResource m
         => FilePath -- ^ root
         -> Sink ByteString m ()
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
                          | EndOfStream
    deriving (Show, Typeable)
instance Exception RecvIntegerException

---------------------------------------
-- TEST SUITE
---------------------------------------

spec :: IO ()
spec = withArgs [] $ hspec $ do
    prop "sendInteger/recvInteger is idempotent" $ \i -> do
        res <- sendInteger i $$ builderToByteString =$ recvInteger
        res `shouldBe` i
    it "create and delete files" $
      withSystemTempDirectory "src" $ \srcDir ->
      withSystemTempDirectory "dst" $ \dstDir -> do

        let relPath = "somepath.txt"
            content = "This is the content of the file" :: ByteString

        writeFile (srcDir </> relPath) content

        runResourceT
            $ sendFile srcDir relPath
           $$ builderToByteString
           =$ recvFile dstDir

        content' <- readFile (dstDir </> relPath)
        content' `shouldBe` content

        removeFile (srcDir </> relPath)

        runResourceT
            $ sendFile srcDir relPath
           $$ builderToByteString
           =$ recvFile dstDir

        exists <- doesFileExist (dstDir </> relPath)
        exists `shouldBe` False
    it "sourceFileChanges" $ withSystemTempDirectory "source-file-changes" $ \root -> do
        chan <- newTChanIO
        let actions =
                [ ("foo", Just "hello")
                , ("bar", Just "world")
                , ("foo", Just "!")
                , ("bar", Nothing)
                , ("foo", Nothing)
                ]
        runConcurrently $
            Concurrently (runResourceT $ sourceFileChanges root $$ mapM_C (atomically . writeTChan chan)) <|>
            Concurrently (forM_ actions $ \(path, mcontents) -> do
                threadDelay 100000
                case mcontents of
                    Nothing -> removeFile (root </> path)
                    Just contents -> writeFile (root </> path) (contents :: ByteString)) <|>
            Concurrently (forM_ actions $ \(expected, _) -> do
                actual <- atomically $ readTChan chan
                actual `shouldBe` expected)
