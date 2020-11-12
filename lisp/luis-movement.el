;; -*- lexical-binding: t; -*-

;;; Scrolling

(setq scroll-preserve-screen-position t
      scroll-conservatively 50
      scroll-margin 2
      ;; Scroll in smaller steps when mouse is used.
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;; I used to bind these to M-N/P (== Option-Shift-n/p) but for some reason
;;; these key bindings no longer work on macOS Catalina. Maybe recheck this
;;; later. For now they only work if not overlayed by another keymap.
(global-set-key (kbd "M-n") #'scroll-up-line)
(global-set-key (kbd "M-p") #'scroll-down-line)

;;; Windows

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init
  (setq aw-dispatch-when-more-than 3
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

;;; Misc

(use-package subword
  :defer t
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  :config
  (diminish 'subword-mode))

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
(save-place-mode 1)

(provide 'luis-movement)
