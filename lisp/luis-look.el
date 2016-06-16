;;;; For things concerning the way it looks and what is highlighted.

;; Diable menu, tool and scroll bar.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(use-package solarized-theme
  :ensure t
  :defer t)

;; See customize group Solarized for options.
(load-theme 'solarized-dark t)

;; Highlight current line
(global-hl-line-mode t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; No bell.
(setq ring-bell-function 'ignore)

;; Highlight matching parenthesis.
(show-paren-mode t)

(setq-default indicate-empty-lines t)


(provide 'luis-look)
