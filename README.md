# .emacs.d

My `.emacs.d` for Mac OS X. Originally derived from
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

-   __Sending Mail__

    For connecting to a SMTP server that requires authentication
    `gnutls` is required, install it with

    ```sh
    brew install gnutls
    ```

    The credentials for the server are stored in `~/.authinfo`, run

    ```sh
    touch ~/.authinfo
    chmod 600 ~/.authinfo
    ```

    to create it and set it's permissions properly. Then insert the credentials in the following format:

    ```
    machine YOUR_SMTP_SERVER port YOUR_SMTP_SERVER_PORT login YOUR_SERVER_LOGIN password YOUR_PASSWORD
    ```

    `YOUR_SMTP_SERVER_PORT` is probably `587`, `YOUR_SERVER_LOGIN` is
    your E-Mail adress. Now make sure you edit
    `customizations/my-private.el` to contain all variables required for
    sending mails. You may also have a look at `customizations/my-apps.el`
    for more options.

    Source: http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html

-   __Receiving Mail__

    Here's a tutorial: http://www.kirang.in/2014/11/13/emacs-as-email-client-with-offlineimap-and-mu4e-on-osx/

    Follow the tutorial, also have a look at the comments. The files
    `customizations/my-apps.el`,
    `customizations/my-machine-local.el.template` and
    `customizations/my-private.el.template` already contain the options
    you need, just modify them according to your mail provider.

    To install `mu4e` I only had to run

    ```sh
    EMACS=$(which emacs) brew install mu --with-emacs --HEAD
    ```

    and make sure `/usr/local/share/emacs/site-lisp/mu/mu4e` (probably
    also `.../site-lisp/mu4e`) is in my Emacs load path to make it
    work. Renaming `/usr/bin/emacs` was not required (I have Emacs
    installed with Homebrew and in my PATH `/usr/local/bin` comes before
    `/usr/bin`, also because of some limitations of OS X 10.11.2 I could
    not modify `/usr/bin` anyway, even with `sudo`).

Now open Emacs, all required packages will be installed automatically
which may take a while.
