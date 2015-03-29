;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.

(setq solarized-high-contrast-mode-line t)
(setq solarized-use-more-italic t)
(setq solarized-distinct-fringe-background t)

(load-theme 'solarized-dark t)

;; Font Size
(set-face-attribute 'default nil :height 130)
;; Font Family
(set-face-attribute 'default nil :family "Input Mono")
