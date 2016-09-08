-- Enable various language extensions. We're using some pretty
-- standard Haskell extensions here, nothing too esoteric.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Import some modules. A few things to note:
--
-- * We're using ClassyPrelude.Conduit instead of the standard
-- Prelude, which provides us with quite a bit more out of the box,
-- especially for our conduit usage.
--
-- * In order to make it clear where things are coming from, we're
-- using explicit imports or qualified imports (besides the prelude
-- import). You're free to be lazy and simply do something like
-- `import System.Directory`.
import           ClassyPrelude.Conduit
import           Data.Conduit.FSNotify      (sourceFileChanges, mkFileChangeSettings, eventPath)
import           Data.Conduit.Network       (appSink, appSource, clientSettings,
                                             runTCPClient, runTCPServer,
                                             serverSettings)
import           Data.Word8                 (_0, _9, _colon, _hyphen)
import           Options.Applicative.Simple (addCommand, argument, auto,
                                             metavar, simpleOptions,
                                             simpleVersion, str)
import           Paths_dumb_file_mirror     (version)
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist, removeFile)
import           System.Environment         (withArgs)
import           System.FilePath            (takeDirectory)
import           System.IO                  (IOMode (ReadMode), hFileSize,
                                             openBinaryFile)
import           System.IO.Temp             (withSystemTempDirectory)
import           Test.Hspec                 (hspec, it, shouldBe)
import           Test.Hspec.QuickCheck      (prop)

-- | The main function is the entrypoint to our program. We're going
-- to parse command line arguments and then perform the appropriate
-- action.
main :: IO ()
main = do
    -- Use the simpleOptions function to parse the command line
    -- arguments. We have multiple subcommands, but no flags,
    -- therefore we're using the unit value () here. The cmd will end
    -- up being the function to run next.
    ((), cmd) <- simpleOptions

        -- Version of the executable. This $(...) syntax is Template
        -- Haskell, and the simpleVersion function will look up Git
        -- commit information at compile time, making the
        -- dumb-file-mirror --version output much more useful:
        --
        -- $ dumb-file-mirror --version
        -- Version 0.1.0.0, Git revision 7320af1acc8de1c1cd37f44590f5799f7493cc98 (dirty)
        $(simpleVersion version)

        -- A one-line header for the --help output
        "dumb-file-mirror: Mirror file changes to a local host"

        -- Longer description for the --help output
        desc

        -- No options, so we just use the unit value ()
        (pure ())

        -- Parse the available commands.
        $ do

            -- Remote command, takes port and directory arguments.
            addCommand "remote" "Receive file changes" id

                -- This <$> and <*> syntax allows us to build up a
                -- result. It is known as applicative syntax, and
                -- applies a function to some wrapped-up values. In
                -- this case, the portArg and dirArg values are
                -- instructions on how to parse the command line
                -- arguments, not the actual port and directory.
                (remote <$> portArg <*> dirArg)

            -- Local command, takes host, port, and directory arguments
            addCommand "local" "Send file changes" id
                (local <$> hostArg <*> portArg <*> dirArg)

            -- Run the test suite. Unorthodox to include the test
            -- suite in the main executable, but makes for a
            -- single-file example.
            addCommand "test" "Run the test suite" id $ pure spec

    -- Run the action returned by the command line parser (remote,
    -- local, or spec).
    cmd
  where
    -- This describes how we parse each of the command line
    -- arguments. To see how this affects output:
    --
    -- $ dumb-file-mirror local
    -- Missing: HOST PORT DIRECTORY
    --
    -- Usage: dumb-file-mirror local HOST PORT DIRECTORY
    --   Send file changes
    hostArg = argument str (metavar "HOST")
    portArg = argument auto (metavar "PORT")
    dirArg = argument str (metavar "DIRECTORY")

-- | A longer description of the program, referenced above in main.
desc :: String
desc = unlines
    [ "This program will mirror local file changes to a remote host."
    , "By keeping a persistent TCP connection open between the local"
    , "and remote machines, latency is reduced versus more naive"
    , "solutions, like combining inotify and rsync."
    , "Note that this tool does not perform an initial file copy, if"
    , "needed you should do an explicit scp -r before using this tool."
    ]

-- | Run the remote portion, which listens on the given port and
-- writes files to the given directory.
remote :: Int -- ^ port to listen on
       -> FilePath -- ^ root directory to write files to
       -> IO ()
remote port dir =
    -- Launch a TCP server and use the given run function for each
    -- connection. `handleAny print` is used to display any exceptions
    -- on the console.
    runTCPServer settings (handleAny print . run)
  where
    -- define some server settings that say to listen to all network
    -- interfaces (*) on the given port
    settings = serverSettings port "*"

    -- Our run function is given an `appData` value, which lets us communicate with the peer on the network.
    run appData =
        -- runResourceT creates a scoped area where scarce resources -
        -- like file descriptors - can be allocated, and are
        -- guaranteed to be freed. This allows for easy exception
        -- safety, and complicated control flows to work
        -- seemlessly. In particular, the bracketP function relies on
        -- this function being used.
        runResourceT

        -- This streams out all data from the peer as a stream of
        -- ByteStrings (array of bytes).
      $ appSource appData

        -- We connect (with the $$ operator) our input stream with
        -- this _sink_. Our sink repeatedly runs recvFile until the
        -- stream is closed. Each call to recvFile reads a new file
        -- from the stream and writes it to disk.
     $$ peekForeverE (recvFile dir)

-- | Run the local portion: connect to the remote process, watch for
-- file changes, and on each change send the updated contents to the
-- remote process.
local :: String -- ^ host to connect to
      -> Int -- ^ port to connect to
      -> FilePath -- ^ root directory
      -> IO ()
local host port dir =
    -- Create a connection to the remote process
    runTCPClient settings $ \appData ->
           runResourceT

           -- Get a stream providing the file path of each file
           -- changed on the filesystem.
         $ sourceFileChanges (mkFileChangeSettings dir)

           -- Wait for every new changed file, and then call sendFile
           -- on it to create the binary data to be sent to the
           -- client.
        $$ awaitForever (sendFile dir . eventPath)

           -- Send the data to the remote process
        =$ appSink appData
  where
    -- We receive the hostname as character data on the console, but
    -- the network talks in bytes. Let's convert that String to a
    -- ByteString by assuming a UTF-8 encoding.
    hostBytes = encodeUtf8 (pack host)

    -- And now use that hostBytes to create connection settings for
    -- the requested host/port combo
    settings = clientSettings port hostBytes

---------------------------------------
-- CONDUIT UTILITY FUNCTIONS
---------------------------------------

-- | Send a file over the network, in the format used by this package
-- (see README.md).
sendFile :: MonadResource m
         => FilePath -- ^ root directory
         -> FilePath -- ^ path relative to root
         -> Producer m ByteString
sendFile root fp = do
    -- Send the relative file path, so the other side knows where to
    -- put the contents
    sendFilePath fp

    -- The file may or may not exist. Checking for file existance is
    -- actually open to a race condition: after we check for the file
    -- and before we open it, some other process may delete
    -- it. Therefore, the safe thing to do is open up the file, and
    -- catch any exceptions that occur. For our purposes, we treat all
    -- exceptions as "file does not exist," though something more
    -- fine-grained could definitely be used instead.
    --
    -- The tryIO function will try to run the action, and if any IO
    -- exceptions are thrown, return them as a Left value. Otherwise,
    -- it will return the file handle as a Right value. This is the
    -- Either sum type, and is a powerful feature of Haskell. To learn
    -- more, see:
    -- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/sum-types
    let open = tryIO $ openBinaryFile fpFull ReadMode

        -- If the opening failed, we'll have an error message. So
        -- there's nothing to close, just do nothing!
        close (Left _err) = return ()
        -- Opening succeeded, so close the file handle.
        close (Right h) = hClose h

    -- Grab the file handle...
    bracketP open close $ \eh ->
        case eh of
            -- No file, send a -1 length to indicate file does not
            -- exist
            Left _ex -> sendInteger (-1)

            -- File exists
            Right h -> do
                -- Send the size of the file
                size <- liftIO $ hFileSize h
                sendInteger size

                -- And stream the contents
                sourceHandle h
  where
    fpFull = root </> fp

-- | Send a raw integer. We follow something like the netstring
-- protocol, and print the integer in decimal form followed by a
-- colon.
sendInteger :: Monad m => Integer -> Producer m ByteString
sendInteger i = yield $ encodeUtf8 $ tshow i ++ ":"

-- | Send a filepath.
sendFilePath :: Monad m => FilePath -> Producer m ByteString
sendFilePath fp = do
    -- UTF-8 encode the filepath
    let bs = encodeUtf8 $ pack fp :: ByteString

    -- Send the number of bytes
    sendInteger $ fromIntegral $ length bs

    -- Send the actual path
    yield bs

-- | Receive a file sent by sendFile
recvFile :: MonadResource m
         => FilePath -- ^ directory to store files in
         -> Sink ByteString m ()
recvFile root = do
    -- Get the relative path to store in
    fpRel <- recvFilePath

    -- Prepend with the root directory to get a complete path
    let fp = root </> fpRel

    -- Get the size of the file
    fileLen <- recvInteger

    if fileLen == (-1)
        -- We use -1 to indicate the file should be removed. Go ahead
        -- and call removeFile, but ignore any IO exceptions that
        -- occur when doing so (in case the file doesn't exist
        -- locally, for example)
        then liftIO $ void $ tryIO $ removeFile fp
        else do
            -- Create the containing directory
            liftIO $ createDirectoryIfMissing True $ takeDirectory fp

            -- Stream out the specified number of bytes and write them
            -- into the file
            takeCE fileLen =$= sinkFile fp

-- | Receive an integer sent by sendInteger.
recvInteger :: (MonadThrow m, Integral i) => Sink ByteString m i
recvInteger = do
    -- Check for a hyphen (indicating a negative number). First we
    -- peek at the next byte, leaving it on the stream in case it's
    -- not a hyphen.
    mnext <- peekCE

    -- We need to have some data. If we got a Nothing, we have an end
    -- of stream, so we should throw that exception.
    next <-
        case mnext of
            Nothing -> throw EndOfStream
            Just next -> return next

    isNeg <-
        if next == _hyphen
            then do
                -- We did get a hyphen, so drop that byte from the
                -- stream and return True
                dropCE 1
                return True

                -- Not a hyphen, so return False and don't drop any
                -- data
            else return False

    -- Take all bytes up until the terminating colon, and fold over
    -- them with addDigit to sum up the result
    x <- takeWhileCE (/= _colon) =$= foldMCE addDigit 0

    -- Make sure we actually have a colon at the end
    mw <- headCE
    unless (mw == Just _colon) (throwM (MissingColon mw))

    -- Negate the answer if necessary
    return $! if isNeg then negate x else x
  where
    addDigit total w
        | _0 <= w && w <= _9 = return (total * 10 + fromIntegral (w - _0))
        | otherwise = throwM (InvalidByte w)

-- | Receive a file path sent with sendFilePath
recvFilePath :: MonadThrow m => Sink ByteString m FilePath
recvFilePath = do
    -- Get the byte count
    fpLen <- recvInteger

    -- Read in the given number of bytes, decode as UTF-8 text, and
    -- then fold all of the chunks into a single Text value
    fpRelText <- takeCE fpLen =$= decodeUtf8C =$= foldC

    -- Unpack the text value into a FilePath
    return $ unpack fpRelText

-- | Define an exception type for when something goes wrong
data RecvIntegerException = InvalidByte Word8
                          | MissingColon (Maybe Word8)
                          | EndOfStream
    deriving (Show, Typeable)
instance Exception RecvIntegerException

---------------------------------------
-- TEST SUITE
---------------------------------------

-- | Run the test suite. We start off with @withArgs []@ since the
-- @test@ argument our options parser above expects would confuse
-- hspec. This isn't normally a problem, when the test suite is in its
-- own executable, it's only because we're being overly clever with
-- including the test suite with the executable.
spec :: IO ()
spec = withArgs [] $ hspec $ do
    -- Ensure that sending a value through sendInteger and recvInteger
    -- comes back with the same result. This will generate random data
    -- to test against.
    prop "sendInteger/recvInteger is idempotent" $ \i -> do
        res <- sendInteger i $$ recvInteger
        res `shouldBe` i

    -- Ensure that sending a value through sendFilePath and
    -- recvFilePath comes back with the same result. This also works
    -- on random data.
    prop "sendFilePath/recvFilePath is idempotent" $ \fp -> do
        res <- sendFilePath fp $$ recvFilePath
        res `shouldBe` fp

    -- A more standard unit test, checking that sending and receiving
    -- a file does what is expected.
    it "create and delete files" $
      -- Get temporary source and destination directories
      withSystemTempDirectory "src" $ \srcDir ->
      withSystemTempDirectory "dst" $ \dstDir -> do

        let relPath = "somepath.txt"
            content = "This is the content of the file" :: ByteString

        -- Ensure that sending a file that exists makes it appear in
        -- the destination
        writeFile (srcDir </> relPath) content

        runResourceT $ sendFile srcDir relPath $$ recvFile dstDir

        content' <- readFile (dstDir </> relPath)
        content' `shouldBe` content

        -- Ensure that sending a file that doesn't exist makes it
        -- disappear in the destination
        removeFile (srcDir </> relPath)

        runResourceT $ sendFile srcDir relPath $$ recvFile dstDir

        exists <- doesFileExist (dstDir </> relPath)
        exists `shouldBe` False
