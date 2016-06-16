(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "lisp/lib" user-emacs-directory) load-path)

;; General Settings required by other specific configuration subsets.
(require 'luis-elpa)
(require 'luis-site-lisp)

(progn
  (setq custom-file
        (expand-file-name "lisp/luis-custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(require 'use-package)


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
