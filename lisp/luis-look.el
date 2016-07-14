;;;; For things concerning the way it looks and what is highlighted.

;;; Wrapping

;; Prefix wrapped lines like filling does but don't change the buffer. Does not
;; work with tabs.
(use-package adaptive-wrap
  :ensure t
  :commands (adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 0))

(setq-default truncate-lines t)
(setq-default word-wrap t)
(setq line-move-visual nil)

;; Enable in files with long lines that can not be modified.
(use-package luis-code-wrap
  :bind ("C-c w" . luis-code-wrap-mode))

(use-package luis-text-wrap
  :commands luis-text-wrap-mode)

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

;; Does not look good and sometimes causes display issues on my Mac.
(setq overflow-newline-into-fringe nil)

;; Display column number in mode line.
(column-number-mode 1)


(provide 'luis-look)
