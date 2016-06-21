;;;; For things concerning the way it looks and what is highlighted.

;;; Wrapping

;; Prefix wrapped lines like filling does but don't change the buffer. Does not
;; work with tabs.
(use-package adaptive-wrap
  :ensure t
  :commands (adaptive-wrap-prefix-mode)
  :init
  (add-hook 'prog-mode-hook #'adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 2)
  (add-hook 'adaptive-wrap-prefix-mode-hook
            (lambda ()
              (setq-local word-wrap t)
              (setq-local line-move-visual nil))))

;;; Theme

(use-package solarized-theme
  :ensure t
  :defer t)

;; See customize group Solarized for options.
(load-theme 'solarized-dark t)

;;; Misc

;; Diable menu, tool and scroll bar.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(blink-cursor-mode -1)
(show-paren-mode 1)
(setq ring-bell-function 'ignore)
(setq-default indicate-empty-lines t)


(provide 'luis-look)
