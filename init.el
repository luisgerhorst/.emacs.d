;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "lisp/lib"))

;;; Variables

(setq custom-file (locate-user-emacs-file "lisp/luis-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(require 'luis-machine-local)
(require 'luis-private)

;;; Load Path

(require 'luis-packages)
(require 'use-package)
(require 'diminish)

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
