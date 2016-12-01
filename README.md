# .emacs.d

My `.emacs.d` for Mac OS X. Originally derived from [flyingmachine/emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure).

## Installation

First clone the repository including submodules:

```sh
git clone --recursive git@github.com:luisgerhorst/.emacs.d.git ~/.emacs.d
```

Then read the following:

-   __Local Config__

    The following files contain user or machine-spcific options that will not be included in your git repository, remove the `.template` extension and edit them to contain all desired values.

    ```
    lisp/luis-machine-local.el.template
    lisp/luis-private.el.template
    ```

-   __Python Autocompletion__

    The config is using the environments from `pyenv` (usually located in `~/.pyenv/versions`). See customizeable variable `jedi:environment-root` for current version used. Run `pyenv install VERSION` to install a specific version (preferably the one `jedi:environment-root` is set to) before opening any Python files.

-   __Dired__

    `gls` (GNU-Version of `ls`) has to be in your `$PATH`. Do `brew install coreutils`. If you want to use the default `ls` change `insert-directory-program` (is set in `lisp/luis-files.el`).

-   __Java__

    Install the [JDK](http://www.oracle.com/technetwork/java/index.html).
    
    Install [emacs-eclim](https://github.com/emacs-eclim/emacs-eclim#installation). The configuration is located in `lisp/lib/luis-java.el`. When opening a project for the first time do `M-x eclim-project-create`, `eclimd` is started automatically the first time a Java file is opened.

    Install [SBT](http://www.scala-sbt.org) with `brew install sbt` to build Java code. See `lisp/lib/luis-java.el` for keybindings to run `sbt` commands.

-   __Scala__

    Make sure you have the JDK and `sbt` installed (see Java section). Make sure [sbt is configured for use with ensime](http://ensime.github.io/build_tools/sbt/). When in a project generate the `.ensime` file using [`M-x sbt-command ensimeConfig`](http://ensime.github.io/build_tools/sbt/#core-commands) and start ensime with [`M-x ensime`](http://ensime.github.io/editors/emacs/install/#starting) to get syntax checking, autocompletion etc.. When you experience problems you may try [`M-x ensime-server-update`](http://ensime.github.io/editors/emacs/install/#updating).

-   __TeX__

    Can be installed using the latest version of [MacTeX](http://www.tug.org/mactex/index.html).

-   __Interactive Spell on Mac OS X__

    Install `aspell` with `brew install aspell`. Then download the dictionaries for the languages you need from `ftp://ftp.gnu.org/gnu/aspell/dict/0index.html` (see README in downloaded folder for install instruction, you may need `sudo` when doing `make install`).

    Then `customize` the variable `ispell-dictionary`, set it to the dictionary you want to use (e.g. `de_DE` for German, run `ispell-change-dictionary` to see a list of available dictionaries and change the used on temporarily).

-   __Sending Mail__

    Here's the [turorial](http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html) that helped me when I set it up.

    For connecting to a SMTP server that requires authentication `gnutls` is required, install it with

    ```sh
    brew install gnutls
    ```

    The credentials for the server are stored in `~/.authinfo`, run

    ```sh
    touch ~/.netrc
    chmod 600 ~/.netrc
    ```

    to create it and set it's permissions properly. Then insert the credentials in the following format:

    ```
    machine YOUR_SMTP_SERVER port YOUR_SMTP_SERVER_PORT login YOUR_SERVER_LOGIN password YOUR_PASSWORD
    ```

    `YOUR_SMTP_SERVER_PORT` is probably `587`, `YOUR_SERVER_LOGIN` is your E-Mail adress. Now make sure you edit `lisp/lib/luis-mail-private.el` to contain all variables required for sending mails.

-   __Receiving Mail__

    We'll use `mbsync` to fetch mail from the server, install it with

    ```sh
    brew install isync
    ```

    Now __configure it__, you can find my config in my [dotfiles](https://github.com/luisgerhorst/dotfiles/blob/master/.mbsyncrc). Here's a [turorial for switching from `offlineimap` to `mbsync`](http://pragmaticemacs.com/emacs/migrating-from-offlineimap-to-mbsync-for-mu4e/). Here's the [man page](http://isync.sourceforge.net/mbsync.html).

    Now run `mbsync -a` once from the terminal to fill your maildir with messages.

    When done, install `mu4e` (if `mu` is already installed reinstall it since `EMACS` has to be set, make sure `brew` is not wrapped by `brew-file` or something) and index your maildir using the following commands:.

    ```sh
    EMACS=$(which emacs) brew install mu --with-emacs --build-from-source --HEAD
    mu index --maildir=~/.maildir
    ```

    Then configure `mu4e` according to your mail provider and machine. The files `lisp/lib/luis-mail-private.el.template` and `lisp/luis-machine-local.el.template` already contain the options you need.

    Finally install `terminal-notifier` using `brew install terminal-notifier` for desktop notifications by [`mu4e-alert`](https://github.com/iqbalansari/mu4e-alert).

Now open Emacs, all required packages will be installed automatically which may take a while.
