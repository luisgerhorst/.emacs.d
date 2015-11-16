# .emacs.d

My `.emacs.d`. Originally derived from
[flyingmachine/emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure).

## Installation

First do `git clone https://github.com/luisgerhorst/.emacs.d.git
~/.emacs.d`. Then read the following before opening Emacs.

### Local config

Rename the file `customizations/my-private.el.template` to
`customizations/my-private.el`. You may also edit it to contain all
desired values. The file will not be included in your git repository.

Do the same for `customizations/my-machine-local.el.template`. Its for
settings specific to your machine (e.g. settings that depend on your
screen size). Git also ignores this file.

### Python autocompletion

The config is using the environments from `pyenv` (usually located in
`~/.pyenv/versions`). See customizeable variable `jedi:environment-root`
for current version used. Run `pyenv install VERSION` to install a
specific version (preferably the one `jedi:environment-root` is set to)
before opening any Python files.

### Dired

`gls` (GNU-Version of `ls`) has to be in your `$PATH`. Do `brew install coreutils`.

### Erlang

`erl` has to be in your `$PATH`. Do `brew install erlang`.

### Done?

Open Emacs, all required packages will be installed automatically which
may take a while.
