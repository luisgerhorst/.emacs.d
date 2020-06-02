;; -*- lexical-binding: t -*-
;;;; For things concerning the way it looks and what is highlighted.

;;; Mode Line

(column-number-mode 1)
(size-indication-mode 1)

;;; Wrapping

;; Prefix wrapped lines like filling does but don't change the buffer. Does not
;; work with tabs.
(use-package adaptive-wrap
  :commands (adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 0))

(setq-default truncate-lines t)
(setq-default word-wrap t)

(use-package visual-fill-column)

;; Enable in files with long lines that can not be modified.
(use-package luis-code-wrap
  :straight nil
  :commands (luis-code-wrap-mode))

(use-package virtual-auto-fill
  :straight (virtual-auto-fill
             :type git
             :host github
             :repo "luisgerhorst/virtual-auto-fill")
  :init
  (setq virtual-auto-fill-mode-visual-fill-column-mode-warning-in-emacs-pre-26-1 nil))

;;; Theme

(use-package solarized
  :defer t
  :straight (solarized :host github :repo "bbatsov/solarized-emacs"
                       :fork (:host github :repo "luisgerhorst/solarized-emacs" :branch "terminal_workaround"))
  :init
  (load-theme 'solarized-dark t)
  :config
  (when solarized-iterm
    (set-face-attribute 'vertical-border nil
                        :background "brightgreen"
                        :foreground "brightgreen")))

;;; Font

;; First check whether the font is installed.
(when (member "Input Mono" (font-family-list))
  ;; On macOS you have to install theses manually (e.g. by opening them, they are
  ;; in dotfiles/home/.fonts).
  (set-frame-font "Input Mono" t t))

;; In GNU/Linux Gnome just open the Tweaks application and set the default
;; monospace font to Input Mono (size 10.0 or 11.0 depending on display).
(when (string-equal system-type "darwin")
  ;; For some reason macOS and GNU/Linux Gnome do not agree about font sizes. In
  ;; Gnome, 10.0 is as large as 13.0 in macOS.
  (set-face-attribute 'default nil :height 130))

;;; Whitespaces

(setq-default indicate-empty-lines t)

;;; Cursor

(show-paren-mode 1)
(blink-cursor-mode -1)

;;; Misc

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(unless (and (display-graphic-p) (string-equal system-type "darwin"))
  ;; I don't use the menubar usually. In macOS Emacs.app however the system
  ;; menubar is visible anyway, thus leave it enabled there.
  (menu-bar-mode -1))

(setq ring-bell-function 'ignore)

;; Does not look good and sometimes causes display issues on my Mac but
;; otherwise eshell always scrolls to the right when a process (like homebrew)
;; shows a progress bar.
(setq overflow-newline-into-fringe t)

;; Highlight FIXME/TODO in comments.
(use-package fic-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook #'fic-mode))

(provide 'luis-look)
;;; luis-look.el ends here
