;;;; For things concerning the way it looks and what is highlighted.

;; Diable menu, tool and scroll bar.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require-package 'solarized-theme)

;; See customize group Solarized for options.
(load-theme 'solarized-dark t)

;; Highlight current line
(global-hl-line-mode t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; no bell
(setq ring-bell-function 'ignore)

;; Highlights matching parenthesis
(show-paren-mode t)

(setq default-indicate-empty-lines t)


(provide 'luis-look)
