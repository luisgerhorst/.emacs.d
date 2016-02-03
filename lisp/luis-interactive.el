;;; Interactively do things

;; See customize for options.
(ido-mode 1)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(require-package 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;; Discover Emacs with popup buffers.

(require-package 'discover)
(require 'discover)

(discover-add-context-menu
 ;; See discover.el for key.
 :context-menu (assq 'rectangles discover-context-menus)
 :bind "H-r")                           ; More convenient shortcut.


(provide 'luis-interactive)
