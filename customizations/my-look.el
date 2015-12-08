;; For things concerning the way it looks and what is highlighted.

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

