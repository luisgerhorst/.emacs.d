(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "lisp/luis-custom.el" user-emacs-directory))

;; General Settings required by other specific configuration subsets.
(require 'luis-elpa)
(require 'luis-site-lisp)
(require 'luis-utils)
(require 'luis-startup)

;; Feature specific settings.
(require 'luis-osx-integration)
(require 'luis-interactive)
(require 'luis-user-interaction)
(require 'luis-look)
(require 'luis-files)
(require 'luis-editing)
(require 'luis-movement)
(require 'luis-languages)
(require 'luis-mail)
(require 'luis-apps)
(require 'luis-misc)

;; Excluded from Git.
(require 'luis-machine-local)
(require 'luis-private)

(when (file-exists-p custom-file)
  (load custom-file))
