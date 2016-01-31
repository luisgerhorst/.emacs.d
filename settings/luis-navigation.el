;;; Helm

(add-to-list 'load-path
             (expand-file-name "vendor/async" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "vendor/helm" user-emacs-directory))
(require 'helm-config)

(helm-mode 1)

(setq helm-M-x-fuzzy-match t
      helm-M-x-always-save-history t
      helm-autoresize-mode t
      helm-autoresize-max-height 30
      helm-display-buffer-default-size 30
      helm-candidate-number-limit 100)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-recentf)

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
