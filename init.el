
;; Is being called in luis-packages.
;; (package-initialize)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "lisp/lib"))

(setq custom-file (locate-user-emacs-file "lisp/luis-custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Load Path

(require 'luis-packages)

;;; Libraries

(require 'use-package)
(use-package diminish
  :ensure t)

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
