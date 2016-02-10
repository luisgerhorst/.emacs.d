;;; Interactively do things

(setq ido-use-filename-at-point nil)
(setq ido-enable-flex-matching t)

(ido-mode 1)
(ido-everywhere 1)

(require-package 'ido-ubiquitous)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; https://github.com/DarwinAwardWinner/ido-ubiquitous/tree/4cc53187bf37fedabbc5b6dea41e041c80982552#ido-for-describe-face-and-certain-other-commands
(require-package 'crm-custom)
(require 'crm-custom)
(crm-custom-mode 1)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(require-package 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;; Buffers

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; Discover Emacs commands and shortcuts with popup buffers.

(require-package 'which-key)
(require 'which-key)
(which-key-setup-side-window-right-bottom)
(which-key-mode)

(require-package 'discover)
(require 'discover)

(discover-add-context-menu
 ;; See discover.el for key.
 :context-menu (assq 'rectangles discover-context-menus)
 :bind "H-r") ; More convenient shortcut.

;; Display popup with keybingings defined by current major mode.
(require-package 'discover-my-major)
(global-set-key (kbd "C-h C-m") 'discover-my-major)

(provide 'luis-interactive)
