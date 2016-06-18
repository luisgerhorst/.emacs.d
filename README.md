# .emacs.d

My `.emacs.d` for Mac OS X. Originally derived from
[flyingmachine/emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure).

## Installation

First clone the repository including submodules:

```git clone --recursive git@github.com:luisgerhorst/.emacs.d.git ~/.emacs.d```

Then read the following, you may skip some steps if you don't want to use this feature.

-   __Local Config__

    Rename the file `lisp/luis-private.el.template` to
    `lisp/luis-private.el`. You may also edit it to contain all
    desired values. The file will not be included in your git
    repository.

    Do the same for `lisp/luis-machine-local.el.template`. Its
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
    `lisp/luis-files.el`).

-   __Java__

    Install the [JDK](http://www.oracle.com/technetwork/java/index.html), then [build jdee-server](https://github.com/jdee-emacs/jdee-server#building). Now you can set `jdee-server-dir` (template in `luis-machine-local.el.template`).

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
    `lisp/luis-private.el` to contain all variables required for
    sending mails. You may also have a look at `lisp/luis-apps.el`
    for more options.

    Source: http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html

-   __Receiving Mail__

    We'll use `offlineimap` to fetch mail from the server, install it
    with

    ```sh
    brew install offlineimap
    ```

    Now configure it, here's the config when you're using FastMail:

    ```
    [general]
    accounts = FastMail

    [Account FastMail]
    localrepository = FastMailLocal
    remoterepository = FastMailRemote

    [Repository FastMailLocal]
    type = Maildir
    localfolders = ~/.maildir
    # nametrans = lambda folder: "INBOX" if folder == "Inbox" else "INBOX." + folder

    [Repository FastMailRemote]
    type = IMAP
    remotehost = mail.messagingengine.com
    remoteuser = YOUR_MAIL@fastmail.fm
    remotepass = YOUR_PASSWORD
    cert_fingerprint = 958b1601563aef92607a41eb5bad22e3cace8431
    # folderfilter = lambda folder: folder not in ['INBOX.Inbox']
    # nametrans = lambda folder: "Inbox" if folder == "INBOX" else re.sub('^INBOX\.', '', folder)
    ```

    Do not configure it to run indefinitely, automatically syncing your
    mail periodically (with `autorefresh`). Also do not make your OS
    start it as a deamon on login (e.g. by copying some plists to
    `~/Library/LaunchAgents` as the homebrew formula suggests). `mu4e`
    will take care of everything. Read the
    [`offlineimap` docs](http://docs.offlineimap.org/en/latest/nametrans.html)
    to set up nametrans properly. You can ommit this if you don't want
    nice folder names.

    Now run `offlineimap` once from the terminal to fill your Maildir
    with messages.

    When done, install `mu4e` using

    ```sh
    EMACS=$(which emacs) brew install mu --with-emacs --HEAD
    ```

    Then `mu index --maildir=~/.maildir` to index you Maildir.

    Then configure `mu4e` according to your mail provider. The files
    `lisp/luis-mail.el`, `lisp/luis-machine-local.el.template` and
    `lisp/lib/luis-mail-private.el.template` already contain the options
    you need, just modify them.

    Here's a tutorial that may also help you:
http://www.kirang.in/2014/11/13/emacs-as-email-client-with-offlineimap-and-mu4e-on-osx/

    Install `terminal-notifier` using `brew install terminal-notifier`
    for desktop notifications by
    [`mu4e-alert`](https://github.com/iqbalansari/mu4e-alert).

Now open Emacs, all required packages will be installed automatically
which may take a while.
