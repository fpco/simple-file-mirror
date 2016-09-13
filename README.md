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

## The code

The entire source code is in [a single Haskell source
file](https://github.com/fpco/dumb-file-mirror/blob/master/src/Main.hs) which
is heavily commented. In addition, this project is the topic of a blog post
series on the [FP Complete blog](https://www.fpcomplete.com/blog):

* [Practical Haskell: Dumb File Mirror (Part
  1)](https://www.fpcomplete.com/blog/2016/09/practical-haskell-dumb-file-mirror-1)
