(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lib" user-emacs-directory))

(setq custom-file (expand-file-name "lisp/luis-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; General Settings required by other specific configuration subsets.
(require 'luis-elpa)
(require 'luis-site-lisp)

;; Feature specific settings.
(require 'luis-integration)
(require 'luis-interactive)
(require 'luis-files)
(require 'luis-editing)
(require 'luis-movement)
(require 'luis-misc)

(require 'luis-user-interaction)
(require 'luis-look)

(require 'luis-modes)
(require 'luis-apps)

;; Excluded from Git.
(require 'luis-machine-local)
(require 'luis-private)
