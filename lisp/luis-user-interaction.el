;; Scroll in smaller steps.
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))

;; Disable Mouse
;; (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
;;              [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
;;              [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
;;              [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
;;              [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
;;   (global-unset-key k))

;; Better shortcut for switching buffers (normally C-x o).
(global-set-key (kbd "H-w") 'other-window)

(global-set-key (kbd "H-z") 'undo)

(require-package 'god-mode)
(require 'god-mode)
(global-set-key (kbd "<f7>") 'god-local-mode)

(defun my/god-mode-update-cursor ()
  "Cursor is vertical bar if in god-mode."
  (setq cursor-type (if god-local-mode 'bar 'box)))
(add-hook 'god-mode-enabled-hook 'my/god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'my/god-mode-update-cursor)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


(provide 'luis-user-interaction)
