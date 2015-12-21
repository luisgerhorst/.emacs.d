;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

;; See customize for options.
(ido-mode 1)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Discover Emacs with popup buffers.
(require-package 'discover)
(require 'discover)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(require-package 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
