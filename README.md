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

This is also a good demonstration of using Haskell and the
[conduit library](https://github.com/snoyberg/conduit#readme) for
non-trivial network operations.

## Get started quickly

* Clone the repo: `git clone https://github.com/fpco/dumb-file-mirror`
* Get the [Haskell Stack build tool](https://haskell-lang.org/get-started)
* Inside the `dumb-file-mirror` directory, run `stack install --install-ghc`
* Run `dumb-file-mirror remote 1234 dest-dir` on the remote machine
* Run `dumb-file-mirror local remote-host-name 1234 src-dir` on the local machine
* Edit away!

You can of course get a lot more inventive with this, especially with SSH tunneling.

## How it works
