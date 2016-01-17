# .emacs.d

My `.emacs.d`. Originally derived from
[flyingmachine/emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure).

## Installation

First do `git clone https://github.com/luisgerhorst/.emacs.d.git
~/.emacs.d`. Then read the following, you may skip some steps if you don't want to use this feature.

-   __Local Config__

    Rename the file `customizations/my-private.el.template` to
    `customizations/my-private.el`. You may also edit it to contain all
    desired values. The file will not be included in your git
    repository.

    Do the same for `customizations/my-machine-local.el.template`. Its
    for settings specific to your machine (e.g. settings that depend on
    your screen size). Git also ignores this file.

-   __Python Autocompletion__

    The config is using the environments from `pyenv` (usually located
    in `~/.pyenv/versions`). See customizeable variable
    `jedi:environment-root` for current version used. Run `pyenv install
    VERSION` to install a specific version (preferably the one
    `jedi:environment-root` is set to) before opening any Python files.

-   __Dired__

    `gls` (GNU-Version of `ls`) has to be in your `$PATH`. Do `brew
    install coreutils`. If you want to use the default `ls` change
    `insert-directory-program` (is set in
    `customizations/my-integration.el`).

-   __Erlang__

    `erl` has to be in your `$PATH`. Do `brew install erlang`.

-   __Interactive Spell on Mac OS X__

    Install `aspell` with `brew install aspell`. Then download the
    dictionaries for the languages you need from
    `ftp://ftp.gnu.org/gnu/aspell/dict/0index.html` (see README in
    downloaded folder for install instruction, you may need `sudo` when
    doing `make install`).

    Then `customize` the variable `ispell-dictionary`, set it to the
    dictionary you want to use (e.g. `de_DE` for German, run
    `ispell-change-dictionary` to see a list of available dictionaries
    and change the used on temporarily).

Now open Emacs, all required packages will be installed automatically
which may take a while.
