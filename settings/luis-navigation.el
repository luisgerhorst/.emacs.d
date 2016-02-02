;;; Helm

(add-to-list 'load-path
             (expand-file-name "vendor/async" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "vendor/helm" user-emacs-directory))
(require 'helm-config)

;; Basic helm with fuzzy-matching everywhere.
(helm-mode 1)
(setq helm-mode-fuzzy-match t)

;; Always fuzzy-match.
(setq helm-M-x-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-file-cache-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-lisp-fuzzy-completion t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-buffers-fuzzy-matching t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-recentf)

(setq helm-autoresize-mode t
      helm-autoresize-max-height 30)

;; Show input at the top.
(setq helm-echo-input-in-header-line t)
;; Hide minibuffer in Helm session, since we use the header line
;; already.
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

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
