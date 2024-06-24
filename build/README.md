This directory contains Dockerfiles for all builds.

A build image will:

* Run on Linux x86\_64 with vanilla Docker (no exceptions)
* Not contain any software that would restrict easy modification or copying
* Take a `cabal sdist` style tar.gz of the ShellCheck directory on stdin
* Output a tar.gz of artifacts on stdout, in a directory named for the arch

This makes it simple to build any release without exotic hardware or software.

An image can be built and tagged using `build_builder`,
and run on a source tarball using `run_builder`.

Tip: Are you developing an image that relies on QEmu usermode emulation?
     It's easy to accidentally depend on binfmt\_misc on the host OS.
     Do a `echo 0 | sudo tee /proc/sys/fs/binfmt_misc/status` before testing.
