# .emacs.d

My `.emacs.d`. Originally derived from
[flyingmachine/emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure).

## Installation

1. `git clone https://github.com/luisgerhorst/.emacs.d.git ~/.emacs.d`
2. Rename the file `customizations/my-private.sample.el` to
   `customizations/my-private.el`. You may also edit it to contain all
   desired values. The file will not be included in your git repository.
3. Open Emacs. All required elpa packages will be installed
   automatically, this may take a while.

### Python autocompletion

The config is using the environments from `pyenv` (usually located in
`~/.pyenv/versions`). See customizeable variable `jedi:environment-root`
for current version used. Run `pyenv install VERSION` to install a
specific version (preferably the one `jedi:environment-root` is set to)
before opening any Python files.
