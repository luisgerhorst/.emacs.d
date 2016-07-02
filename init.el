(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lib" user-emacs-directory))

(setq custom-file
      (expand-file-name "lisp/luis-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; General

(require 'luis-packages)
(require 'use-package)

;;; Features

(require 'luis-integration)
(require 'luis-files)
(require 'luis-interactive)
(require 'luis-modification)
(require 'luis-movement)
(require 'luis-misc)

(require 'luis-user-interaction)
(require 'luis-look)

(require 'luis-modes)
(require 'luis-apps)

;; Private

(require 'luis-machine-local)
(require 'luis-private)
