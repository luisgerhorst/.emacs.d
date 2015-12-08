;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(ido-mode 1)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Discover Emacs with popup buffers.
(require-package 'discover)
(require 'discover)

(add-to-list 'load-path (expand-file-name "vendor/async" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/helm" user-emacs-directory))
(require 'helm-config)
(global-set-key [remap execute-extended-command] 'helm-M-x)
