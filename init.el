;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; You can access the stats using `benchmark-init/show-durations-tree' and
;; `benchmark-init/show-durations-tabulated'.
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/benchmark-init-el"))
(when (require 'benchmark-init-loaddefs nil t)
  (benchmark-init/activate))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "lisp/lib"))

;;; Variables

(setq custom-file (locate-user-emacs-file "lisp/luis-custom.el"))
(load custom-file t)

(require 'luis-private nil t)

;;; Load Path

(require 'luis-packages)
(straight-use-package 'use-package)
(require 'diminish)

;;; Features

(require 'luis-integration)
(require 'luis-files)
(require 'luis-interactive)
(require 'luis-modification)
(require 'luis-movement)
(require 'luis-compile)

(require 'luis-user-interaction)
(require 'luis-look)

(require 'luis-modes)
(require 'luis-apps)

(require 'luis-iterm)
