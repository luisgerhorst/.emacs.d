# .emacs.d

My `.emacs.d` for macOS, Debian and Ubuntu. Originally derived from [flyingmachine/emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure).

## Installation

First clone the repository including submodules:

```sh
git clone --recursive git@github.com:luisgerhorst/.emacs.d.git ~/.emacs.d
```

Then read the following:

-   __Local Config__

    Some files contain user or machine-spcific options that will not be included in your git repository, remove the `.template` extension and edit them to contain all desired values.

-   __Python Autocompletion & Syntax Checking__

    The config is using the environments from [`pyenv`](https://github.com/pyenv/pyenv) (usually located in `~/.pyenv/versions`). Ensure zlib is available (do `xcode-select --install` if not), then install pyenv on macOS using [`brew install pyenv`](https://github.com/pyenv/pyenv#homebrew-on-mac-os-x).

    See customizeable variable `jedi:environment-root` for current version used. Run `pyenv install VERSION` to install a specific version (preferably the one `jedi:environment-root` is set to) before opening any Python files.

    For detailed syntax checking with [Flycheck's `python-pylint` checker](http://www.flycheck.org/en/latest/languages.html#syntax-checker-python-pylint) install [pylint](https://pylint.org/#install).

-   __Dired__

    On macOS `gls` (GNU-Version of `ls`) has to be in your `$PATH`. Do `brew install coreutils`. If you want to use the default `ls` change `insert-directory-program` (is set in `lisp/luis-files.el`).

-   __Java__

    Install the [JDK](http://www.oracle.com/technetwork/java/index.html).

-   __Scala__

    Make sure you have the JDK and `sbt` installed (see Java section). Make sure [sbt is configured for use with ensime](http://ensime.github.io/build_tools/sbt/). When in a project generate the `.ensime` file using [`M-x sbt-command ensimeConfig`](http://ensime.github.io/build_tools/sbt/#core-commands) and start ensime with [`M-x ensime`](http://ensime.github.io/editors/emacs/install/#starting) to get syntax checking, autocompletion etc.. When you experience problems you may try [`M-x ensime-server-update`](http://ensime.github.io/editors/emacs/install/#updating).

-   __TeX__

    Can be installed using the latest version of [MacTeX](http://www.tug.org/mactex/index.html).

-   __Interactive Spell on Mac OS X__

    Install `aspell` with `brew install aspell`. Then download the dictionaries for the languages you need from `ftp://ftp.gnu.org/gnu/aspell/dict/0index.html` (see README in downloaded folder for install instruction, you may need `sudo` when doing `make install`).

    Then `customize` the variable `ispell-dictionary`, set it to the dictionary you want to use (e.g. `de_DE` for German, run `ispell-change-dictionary` to see a list of available dictionaries and change the used one temporarily).

-   __Benchmark Init__

    ```sh
    cd site-lisp/benchmark-init-el
    make
    ```

Now open Emacs, all required packages will be installed automatically which may take a while.
