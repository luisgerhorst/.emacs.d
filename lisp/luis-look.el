;;;; For things concerning the way it looks and what is highlighted.

;;; Wrapping

(setq-default truncate-lines t)

;; Prefix wrapped lines like filling does but don't change the buffer. Does not
;; work with tabs.
(use-package adaptive-wrap
  :ensure t
  :commands (adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 2))

(defun luis-toggle-wrapping ()
  (interactive)
  (if truncate-lines
      (progn
        (adaptive-wrap-prefix-mode 1)
        ;; This also disables truncate-lines:
        (visual-line-mode 1))
    (adaptive-wrap-prefix-mode -1)
    (visual-line-mode -1)
    (kill-local-variable 'truncate-lines)))

;; Enable in buffer with unfilled lines (that you can't edit).
(global-set-key (kbd "C-c w") #'luis-toggle-wrapping)

;;; Theme

(use-package solarized-theme
  :ensure t
  :defer t)

;; See customize group Solarized for options.
(load-theme 'solarized-dark t)

;;; Whitespaces

(setq-default indicate-empty-lines t)

;;; Misc

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(blink-cursor-mode -1)
(show-paren-mode 1)
(setq ring-bell-function 'ignore)


(provide 'luis-look)
