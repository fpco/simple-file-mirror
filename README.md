# dumb-file-mirror

[![Build Status](https://travis-ci.org/fpco/dumb-file-mirror.svg?branch=master)](https://travis-ci.org/fpco/dumb-file-mirror)
[![Build status](https://ci.appveyor.com/api/projects/status/19mblbxaig48i26p/branch/master?svg=true)](https://ci.appveyor.com/project/snoyberg/dumb-file-mirror/branch/master)

## What it is

This is a dumb tool to mirror file changes made on one machine onto
another machine. In contrast with more naive approaches (like
combining `inotify` and `rsync`), this approach keeps a persistent TCP
connection open to decrease latency between writes. It is also a fully
cross-platform tool, and uses OS-specific file watching APIs when
available.

I initially wrote this tool to make our lives a little bit nicer when
doing some coding on remote build machines. However, it turned out to
be a good demonstration of some practical Haskell, as well as using
the [conduit library](https://github.com/snoyberg/conduit#readme) for
non-trivial network operations.

## Get started quickly

* Clone the repo: `git clone https://github.com/fpco/dumb-file-mirror`
* Get the
  [Haskell Stack build tool](https://haskell-lang.org/get-started). On
  most POSIX systems, just run `curl -sSL
  https://get.haskellstack.org/ | sh`
* Inside the `dumb-file-mirror` directory, run `stack install
  --install-ghc`. (This will take a while, it's going to set up an
  entire toolchain and build a bunch of dependencies.)
* Run `dumb-file-mirror remote 1234 dest-dir` on the remote machine
* Run `dumb-file-mirror local remote-host-name 1234 src-dir` on the local machine
* Edit away!

You can of course get a lot more inventive with this, especially with
SSH tunneling. For example, try running these in two different
terminals:

```shell
$ ssh user@host -L 12345:localhost:12345 /path/to/bin/dumb-file-mirror remote 12345 /some/dest/dir
$ dumb-file-mirror local localhost 12345 /some/source/dir
```

## How it works

This program uses a very simple network protocol for communicating
file changes. Since file changes are only sent in one direction, each
side of the connection need only send or receive, not both. (In other
words, it's a half-duplex protocol.) The format for sending data is
borrowed heavily from
[netstrings](https://en.wikipedia.org/wiki/Netstring), though for
simplicity I left off the trailing comma. This format is very amenable
to streaming data, which is something we want to leverage here to
avoid allocating large blocks of memory for file contents.

The basic protocol is:

* Send the length of a file path, as a decimal, followed by a colon
* Send the payload of the filepath
* Send the length of the file contents, or -1 if the file has been
  removed. Like the filepath, follow with a colon
* If the file is present, send the payload of the file itself

As an example, if I wrote the words "Hello World" to the file
"hello.txt", the network data sent would be:

```
9:hello.txt11:Hello World
```

Our program needs to implement a TCP client that can watch a directory
tree for file changes, and send appropriate content over the
connection. It also needs to provide a TCP server that can receive
this network protocol and write changes to disk. We also want to do
some command line parsing to determine which mode to run in, plus the
hostname, port, and directory. And finally, we're going to include a
test suite in the program itself (though, in a more realistic
application, we'd separate our code into a library, executable, and
test suite).

* * *

__NOTE__ Just ignore all of the content below here, I'll likely delete
it shortly. I'm writing up a blog post series to properly introduce
the concepts in this program.

## Quick intro to conduit

This program is going to make heavy usage of the
[conduit streaming data library](https://github.com/snoyberg/conduit#readme). You
won't need to know much about conduit to follow. The important terms
are *upstream* (where data is coming from into the current function)
and *downstream* (where the current function is sending data). This
will make more sense when we look at some of our test cases.

**FIXME**

## Sending and receiving integers

We'll work this blog post by starting at the bottom and building up to
the high level functions. The most basic function we have is
`sendInt`, which sends an integer value downstream as binary data:

``` haskell
sendInt :: Monad m => Int -> Producer m ByteString
sendInt i = yield $ encodeUtf8 $ tshow i ++ ":"
```

That first line is the *type signature*, and tells you the inputs and
outputs of this function. Type signatures like this are optional in
Haskell, since we have *type inference*, but it's common practice to
add top-level signatures to make the program easier to follow, and
protect against accidental mistakes in the code. In our case, the
signature is saying:

* I'm going to take in an `Int`
* I'm going to give you a producer of `ByteString`s (binary data)
* And I run in any monad

You can safely ignore the monad bits here, they're not necessary for
following the code, or writing a lot of practical Haskell programs. If
you are interested in learning more, I recommend following up with
*this excellent tutorial FIXME*.

OK, that'll be enough for focusing on types for now, let's instead
focus on the code. `yield` is how we send data downstream (Python
users are already used to this idiom from generators). Our `tshow i ++
":"` converts our integer into textual format and appends a colon. And
our `encodeUtf8` function encodes from text to bytes.

This is an important point worth focusing on: Haskell does a lot to
protect you from making common mistakes, such as treating binary data
as text or vice versa. We use totally different types with explicit
conversions between the two. `ByteString` and `Text` are two of the
most important types for most practical Haskell applications.

Receiving an integer is just a bit more complicated, but not too bad:

``` haskell
recvInt :: MonadThrow m => Sink ByteString m Int
recvInt = do
    -- Get all of the bytes up to the colon, convert to text, and fold
    -- (aka concatenate) into a single chunk
    intText <- takeWhileCE (/= _colon) =$= decodeUtf8C =$= foldC

    -- Drop the colon character.
    dropCE 1

    -- Convert to a String and then use the read function.
    case readMay $ unpack intText of
        Nothing -> error $ "Invalid integer: " ++ show intText
        Just i -> return i
```

We're beginning to see our first bits of conduit code shine through
here. We've composed a few simpler functions - `takeWhileCE` to take
all bytes up to the colon, `decodeUtf8C` to convert to text, and
`foldC` to combine multiple textual chunks into a single chunk - and
created one larger function. This is generally speaking a great power
of functional programming: the ability to piece together smaller
pieces to make up a more powerful application.

### Testing

Whenever we have the chance in Haskell, we like to define properties
of a program instead of giving explicit test cases. The reason for
this is the QuickCheck library, which allows us to get test cases
automatically generated for properties. So what property should our
two functions above follow? An obvious one is: if you send any
integer, and then receive that integer, you should get back the same
result. Let's see how we express this in our test suite:

```haskell
prop "sendInt/recvInt is idempotent" $ \i -> do
    res <- sendInt i $$ recvInt
    res `shouldBe` i
```

We've stated that for any integer `i`, if we connect `sendInt` with
`recvInt` and take the result, it should be the same as the original
`i`. This ensures that forgotten test cases - like negative numbers,
large numbers, 0, etc - are explored.

## Sending and receiving file paths

Building on top of integer sending and receiving, we can do the same
for filepaths. The only tricky bit, once again, is getting the rules
around character encoding down right. When working on this code, I
found the explicit separation between binary and textual data to be
vital for writing the code correctly.

``` haskell
sendFilePath :: Monad m => FilePath -> Producer m ByteString
sendFilePath fp = do
    -- UTF-8 encode the filepath
    let bs = encodeUtf8 $ pack fp :: ByteString

    -- Send the number of bytes
    sendInt $ length bs

    -- Send the actual path
    yield bs
```

Importantly, when we `sendInt` the length, we send the size of the
binary representation, _not_ the textual representation. Remember that
for non-ASCII file paths, the UTF-8 encoded representation will always
have more bytes than the original has characters, so this is
absolutely vital to get right.

What's really nice is how similar the reverse operation (receiving the
filepath) is:

``` haskell
recvFilePath :: MonadThrow m => Sink ByteString m FilePath
recvFilePath = do
    -- Get the byte count
    fpLen <- recvInt

    -- Read in the given number of bytes, decode as UTF-8 text, and
    -- then fold all of the chunks into a single Text value
    fpRelText <- takeCE fpLen =$= decodeUtf8C =$= foldC

    -- Unpack the text value into a FilePath
    return $ unpack fpRelText
```

And like integers, writing our test case is trivial:

``` haskell
prop "sendFilePath/recvFilePath is idempotent" $ \fp -> do
    res <- sendFilePath fp $$ recvFilePath
    res `shouldBe` fp
```

## Sending the file

We'll break down the sending of the file itself into a few
pieces. What's important to note here is that, until we actually try
and open the file, we don't know if the file exists.\* So what we're
going to do is:

* Send the file path
* Try to open the file
* If the open worked, send the file contents
* If the open failed, send a -1 to indicate the file is gone

\* You might think that you could check for file existence and then
open it, but that leaves you open to a race condition of the file
being deleted after checking for existence.

Our `sendFile` function will take two arguments: the root directory,
and the relative path. We want to send only the relative path to the
other side so it can place the file in any directory it wants. So an
example for calling this function may be `sendFile "/home/user/work"
"blogs/haskell.md"`.

``` haskell
sendFile root fp = do
    -- Get the full path to the file, for reading from it
    let fpFull = root </> fp

    -- Send the relative path
    sendFilePath fp
```

Next we're going to define our open and close functions. We're going
to wrap our standard `openBinaryFile` function with a `tryIO`, which
will catch any IO exception and return it as a `Left` value (see
[this great sum types tutorial for more about `Either`](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/sum-types)). Our
close function will need to check if the file was opened successfully
or not by *pattern matching*. This is all easier to see in action:

``` haskell
    let open = tryIO $ openBinaryFile fpFull ReadMode

        -- If the opening failed, we'll have an error message. So
        -- there's nothing to close, just do nothing!
        close (Left _err) = return ()
        -- Opening succeeded, so close the file handle.
        close (Right h) = hClose h
```

Now we can use the `bracketP` function to open our file and guarantee
it will be closed in the case of an exception. This is a common
pattern in Haskell, and is similar to using `with` in Python or RAII
in C++.

``` haskell
    bracketP open close $ \eh ->
```

Once we've opened our file, 
