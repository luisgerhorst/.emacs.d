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
 :context-menu
 ;; Copied from discover.el with minor changes.
 '(rectangles
  (description "Rectangles, register and bookmarks")
  (actions
   ("Rectangle"
    ("M-w" "copy rectangle as kill" copy-rectangle-as-kill)
    ("N" "rectangle number lines" rectangle-number-lines)
    ("c" "clear rectangle" clear-rectangle)
    ("d" "delete rectangle" delete-rectangle)
    ("k" "kill rectangle" kill-rectangle)
    ("o" "open rectangle" open-rectangle)
    ("r" "copy rectangle to register" copy-rectangle-to-register)
    ("t" "string rectangle" string-rectangle)
    ("y" "yank rectangle" yank-rectangle))

   ("Bookmark"
    ("b" "helm filtered bookmarks" helm-filtered-bookmarks) ; Use helm.
    ("l" "bookmark bmenu list" bookmark-bmenu-list)
    ("m" "bookmark set" bookmark-set))

   ("Register"
    ("+" "increment register" increment-register)
    ("C-@" "point to register" point-to-register)
    ("C-SPC" "point to register" point-to-register)
    ("SPC" "point to register" point-to-register)
    ("f" "frame configuration to register" frame-configuration-to-register)
    ("g" "insert register" insert-register)
    ("i" "insert register" insert-register)
    ;; this is technically not bound to a key but it's just too darn
    ;; useful to leave unbound.
    ("A" "append to register" append-to-register)
    ("j" "jump to register" jump-to-register)
    ("n" "number to register" number-to-register)
    ("s" "copy to register" copy-to-register)
    ("w" "window configuration to register" window-configuration-to-register)
    ("x" "copy to register" copy-to-register))))
 :bind "H-r")                           ; More convenient shortcut.


(provide 'luis-interactive)
