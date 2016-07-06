;;;; For things concerning the way it looks and what is highlighted.

;;; Wrapping

;; Prefix wrapped lines like filling does but don't change the buffer. Does not
;; work with tabs.
(use-package adaptive-wrap
  :ensure t
  :commands (adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 2))

(defun luis-toggle-wrapping (&optional prefix)
  "Enable nice line wrapping if prefix argument is > 0, disable it otherwise.
Toggle nice line wrapping if prefix argument is not set."
  (interactive "P")
  (if (or (and (not prefix) truncate-lines)
          (> prefix 0))
      (progn
        (adaptive-wrap-prefix-mode 1)
        ;; This also disables truncate-lines:
        (visual-line-mode 1))
    (adaptive-wrap-prefix-mode -1)
    (visual-line-mode -1)
    (setq-local truncate-lines t)))

(add-hook 'prog-mode-hook
          (lambda ()
            (luis-toggle-wrapping 0)))

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

;; Display column number in mode line.
(column-number-mode 1)


(provide 'luis-look)
