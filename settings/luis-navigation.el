;;; Helm

(require-package 'popwin)
(require 'popwin)

;; Helm enhancements stolen from Spacemacs
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/+distribution/spacemacs-base/packages.el
(progn
  (setq helm-prevent-escaping-from-minibuffer t
        helm-bookmark-show-location t
        helm-display-header-line nil
        helm-split-window-in-side-p t
        helm-always-two-windows t
        helm-echo-input-in-header-line t
        helm-imenu-execute-action-at-once-if-one nil
        helm-org-format-outline-path t)

  ;; hide minibuffer in Helm session, since we use the header line already
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  ;; fuzzy matching setting
  (setq helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-file-cache-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-buffers-fuzzy-matching t)

  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'spacemacs//hide-cursor-in-helm-buffer)

  (defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
  (defvar spacemacs-helm-display-buffer-regexp `("*.*helm.**"
                                                 (display-buffer-in-side-window)
                                                 (inhibit-same-window . t)
                                                 (side . bottom)
                                                 (window-width . 0.6)
                                                 (window-height . 0.4)))
  (defvar spacemacs-display-buffer-alist nil)
  (defun spacemacs//helm-prepare-display ()
    "Prepare necessary settings to make Helm display properly."
    ;; avoid Helm buffer being diplaye twice when user
    ;; sets this variable to some function that pop buffer to
    ;; a window. See https://github.com/syl20bnr/spacemacs/issues/1396
    (let ((display-buffer-base-action '(nil)))
      (setq spacemacs-display-buffer-alist display-buffer-alist)
      ;; the only buffer to display is Helm, nothing else we must set this
      ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
      ;; etc... because of existing popwin buffers in the alist
      (setq display-buffer-alist nil)
      (popwin-mode -1)))

  (defun spacemacs//display-helm-window (buffer)
    (let ((display-buffer-alist (list spacemacs-helm-display-help-buffer-regexp
                                      ;; this or any specialized case of Helm buffer must be added AFTER
                                      ;; `spacemacs-helm-display-buffer-regexp'. Otherwise,
                                      ;; `spacemacs-helm-display-buffer-regexp' will be used before
                                      ;; `spacemacs-helm-display-help-buffer-regexp' and display
                                      ;; configuration for normal Helm buffer is applied for helm help
                                      ;; buffer, making the help buffer unable to be displayed.
                                      spacemacs-helm-display-buffer-regexp)))
      (helm-default-display-buffer buffer)))
  (setq helm-display-function 'spacemacs//display-helm-window)

  (defun spacemacs//restore-previous-display-config ()
    (popwin-mode 1)
    ;; we must enable popwin-mode first then restore `display-buffer-alist'
    ;; Otherwise, popwin keeps adding up its own buffers to `display-buffer-alist'
    ;; and could slow down Emacs as the list grows
    (setq display-buffer-alist spacemacs-display-buffer-alist))

  (add-hook 'helm-after-initialize-hook 'spacemacs//helm-prepare-display)
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook 'spacemacs//restore-previous-display-config)

  ;; Add minibuffer history with `helm-minibuffer-history'
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

  (defun spacemacs//helm-cleanup ()
    "Cleanup some helm related states when quitting."
    ;; deactivate any running transient map (micro-state)
    (setq overriding-terminal-local-map nil))
  (add-hook 'helm-cleanup-hook 'spacemacs//helm-cleanup))

(add-to-list 'load-path
             (expand-file-name "vendor/async" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "vendor/helm" user-emacs-directory))
(require 'helm-config)

(helm-mode 1)
(setq helm-mode-fuzzy-match t)

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
