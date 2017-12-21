# .emacs.d

My `.emacs.d` for Mac OS X. Originally derived from [flyingmachine/emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure).

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

    Install Eclipse with `brew cask install eclipse-java` and [emacs-eclim](https://github.com/emacs-eclim/emacs-eclim#installation). The configuration is located in `lisp/lib/luis-java.el`. When opening a project for the first time do `M-x eclim-project-create`, `eclimd` is started automatically the first time a Java file is opened.

-   __Scala__

    Make sure you have the JDK and `sbt` installed (see Java section). Make sure [sbt is configured for use with ensime](http://ensime.github.io/build_tools/sbt/). When in a project generate the `.ensime` file using [`M-x sbt-command ensimeConfig`](http://ensime.github.io/build_tools/sbt/#core-commands) and start ensime with [`M-x ensime`](http://ensime.github.io/editors/emacs/install/#starting) to get syntax checking, autocompletion etc.. When you experience problems you may try [`M-x ensime-server-update`](http://ensime.github.io/editors/emacs/install/#updating).

-   __TeX__

    Can be installed using the latest version of [MacTeX](http://www.tug.org/mactex/index.html).

-   __Interactive Spell on Mac OS X__

    Install `aspell` with `brew install aspell`. Then download the dictionaries for the languages you need from `ftp://ftp.gnu.org/gnu/aspell/dict/0index.html` (see README in downloaded folder for install instruction, you may need `sudo` when doing `make install`).

    Then `customize` the variable `ispell-dictionary`, set it to the dictionary you want to use (e.g. `de_DE` for German, run `ispell-change-dictionary` to see a list of available dictionaries and change the used one temporarily).

-   __Sending Mail__

    Here's the [turorial](http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html) that helped me when I set it up.

    For connecting to a SMTP server that requires authentication `gnutls` is required, install it with

    ```sh
    brew install gnutls
    ```

    Install GNUPG to encrypt the file in which we'll store our login credentials and generate a key if you don't already have one.

    ```sh
    brew install gnupg pinentry-mac
    gpg --full-gen-key
    ```

    You can also import an existing key using `gpg --import private.asc`. `pinentry-mac` or a similar program is required.

    The credentials for the server are stored in `~/.authinfo.gpg`, run

    ```sh
    touch ~/.authinfo.gpg
    chmod 600 ~/.authinfo.gpg
    ```

    to create it and set it's permissions properly. Open it in an editor that allows automatic encryption of `.gpg` files (e.g. Emacs). Then insert the credentials in the following format:

    ```
    machine YOUR_SMTP_SERVER port YOUR_SMTP_SERVER_PORT login YOUR_SERVER_LOGIN password YOUR_PASSWORD
    ```

    `YOUR_SMTP_SERVER_PORT` is probably `587`, `YOUR_SERVER_LOGIN` is your E-Mail adress. Now make sure you edit `lisp/lib/luis-mail-private.el` to contain all variables required for sending mails.

-   __Receiving Mail__

    We'll use `mbsync` to fetch mail from the server, install it with

    ```sh
    brew install isync
    ```

    Now __configure it__, you can find my config in my dotfiles. Here's a [turorial for switching from `offlineimap` to `mbsync`](http://pragmaticemacs.com/emacs/migrating-from-offlineimap-to-mbsync-for-mu4e/). Here's the [man page](http://isync.sourceforge.net/mbsync.html).

    Now run `mbsync -a` once from the terminal to fill your maildir with messages.

    When done, install `mu4e` (if `mu` is already installed reinstall it since `EMACS` has to be set, make sure `brew` is not wrapped by `brew-file` or something, you can do so by e.g. temporarily switching to `bash` if you use `zsh`) and index your maildir using the following commands:

    ```sh
    EMACS=$(which emacs) brew install mu --with-emacs
    mu index --maildir=~/.maildir
    ```

    Ensure `mu4e` (contained in `mu`) is loaded when Emacs starts. You can check for it using `M-x mu4e`. If Emacs was installed using Cask, symlink `mu4e` to `.emacs.d/site-lisp`

    ```sh
    ln -s /usr/local/share/emacs/site-lisp/mu/mu4e ~/.emacs.d/site-lisp/mu4e
    ```

    Then configure `mu4e` according to your mail provider and machine. The files `lisp/lib/luis-mail-private.el.template` and `lisp/luis-machine-local.el.template` already contain the options you need.

    Optionally install `terminal-notifier` using `brew install terminal-notifier` for desktop notifications by [`mu4e-alert`](https://github.com/iqbalansari/mu4e-alert).

-   __Benchmark Init__

    ```sh
    cd site-lisp/benchmark-init-el
    make
    ```

Now open Emacs, all required packages will be installed automatically which may take a while.
