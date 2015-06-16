Installation (version 0.2)
=========================

Binary installation
-------------------

### On x86 Ubuntu 12.04

1.  Ensure the prerequisite packages are installed: libgtk2.0-0
2.  Download one of the following executables and place is somewhere on
    your PATH.

    -   [polypaver-0.2-linux-i386-glibc215](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.2-linux-i386-glibc215)
    -   [polypaver-0.2-linux-x86\_64-glibc215](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.2-linux-x86_64-glibc215)

Similar steps should work on newer versions of Ubuntu and on other Linux
systems with `libc6` version 2.15.

### On Windows 7/8

1.  Download [gtk-win
    redistributable](http://downloads.sourceforge.net/gtk-win/gtk2-runtime-2.24.10-2012-10-10-ash.exe?download)
2.  Run the gtk-win installer, selecting "Compatibility DLLs" for
    installation.
3.  Download one of the following executable and place is somewhere on
    your PATH.

    -   [polypaver-0.2-win7-32bit.exe](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.2-win7-32bit.exe)
    -   [polypaver-0.2-win8-64bit.exe](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.2-win8-64bit.exe)

Source installation
-------------------

Note that by installing from source, you also install
the PolyPaver Haskell library, which allows one to
define PolyPaver problems using Haskell.
An example of such a problem is in the folder
[examples/haskell](http://code.google.com/p/polypaver/source/browse/#hg%2Fexamples%2Fhaskell).

### On Ubuntu 12.04

**Warning** These instructions are for a vanilla installation of the
operating system.
You need to modify the steps accordingly if ghc or the haskell-platform
is already installed.
The package has been tested with ghc versions 7.4.1 and 7.4.2.

```sh
sudo apt-get install libgtk2.0-dev
sudo apt-get install ghc
sudo apt-get install cabal-install
cabal update
export PATH=\~/.cabal/bin:$$PATH
cabal install alex
cabal install happy
cabal install gtk2hs-buildtools
wget http://polypaver.googlecode.com/files/polypaver-0.2.tar.gz
tar xvf polypaver-0.2.tar.gz
cd polypaver-0.2
cabal install
```

If you replace the last command with:

```sh
cabal install -f DynamicLoading
```

you will be able to apply PolyPaver on problems defined in Haskell as
follows:

```sh
polypaver examples/haskell/sqrtexp.hs
```

### On Windows 7/8

1.  Install the [Haskell
    Platform](http://lambda.haskell.org/platform/download/2012.4.0.0/HaskellPlatform-2012.4.0.0-setup.exe)
    or ensure that the corresponding Haskell build environment (with GHC
    7.4.2 or later) is installed.
2.  Download and unpack [GTK+ 2.24.10
    32bit](http://ftp.gnome.org/pub/gnome/binaries/win32/gtk+/2.24/gtk+-bundle_2.24.10-20120208_win32.zip)
    into some directory. If this directory is `c:\opt\gtk` then prepend
    `c:\opt\gtk\bin` to your `PATH` environment variable.
3.  Run the following commands to install the `gtk2hs` build tools:

```sh
cabal update
cabal install gtk2hs-buildtools
``` 

4. Download the [PolyPaver 0.2 source code](http://polypaver.googlecode.com/files/polypaver-0.2.tar.gz) 
archive and extract it to a directory, e.g. `c:\opt\polypaver-0.2`. 
You can use a program such as [7zip](http://www.7-zip.org/) to extract the archive. 
Run the following command to build and install: 

```sh
cd c:\opt\polypaver-0.2
cabal install
```

* * * * *

Installation (version 0.1)
==========================

Linux executable
----------------

We provide a 32-bit Ubuntu 11.04 Linux binary of the main [polypaver
executable](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.1).
Downloading and running it may be the easiest way to get started.
Nevertheless, the binary currently requires problems to be in the SPARK
vcg/siv format. The source code archive
[polypaver-0.1.tar.gz](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.1.tar.gz)
contains several siv files in `examples/SPARK/*/out/**.siv`.

polypaver Haskell library
-------------------------

The easiest way to apply PolyPaver on specific numerical conjectures is
to encode them in a simple Haskell file, based on the examples
provided,
and as explained in the Tutorial. This method requires installation of
the polypaver Haskell library.

1.  Prerequisites: [ghc
    6.12.3](http://www.haskell.org/ghc/download_ghc_6_12_3) and
    [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install),
    both parts of [Haskell
    Platform](http://hackage.haskell.org/platform/) 2010.2.0.0 (beware,
    **not** the latest version)
2.  Download and extract the archive
    [polypaver-0.1.tar.gz](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.1.tar.gz).
3.  In the extracted folder `polypaver-0.1` execute: `cabal install`
4.  If all went well, you are ready to compile and execute a PolyPaver
    Haskell main module, such as the provided `examples/mini.hs`.
    Beware, `polypaver` command line arguments do not work reliably when
    executed in `ghci`.
5.  The above installation steps will automatically compile and install
    the `polypaver` executable besides the library.

