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

;; Enable in files with long lines that can not be modified.
(use-package luis-code-wrap
  :commands (luis-code-wrap-mode))

(use-package luis-text-wrap
  :commands (luis-text-wrap-mode)
  :init
  (setq luis-text-wrap-mode-visual-fill-column-mode-warning-in-emacs-pre-26-1 nil))

;;; Theme

(defun luis-style-set (new background-mode powerline-seperator)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme new t)

  (setq frame-background-mode background-mode)
  (frame-set-background-mode (selected-frame)))

(defun luis-style-dark ()
  (require 'solarized)
  (luis-style-set 'solarized-dark 'dark 'utf-8)
  (when solarized-iterm
    (set-face-attribute 'vertical-border nil
                        :background "brightgreen"
                        :foreground "brightgreen")))

(defun luis-style-light ()
  (luis-style-set 'adwaita 'light nil))

(defvar luis-style-toggle-state t)

(defun luis-theme-toggle ()
  (interactive)
  (if luis-style-toggle-state
      (luis-style-light)
    (luis-style-dark))
  (setq luis-style-toggle-state (not luis-style-toggle-state)))

(luis-style-dark)


;;; Whitespaces

(setq-default indicate-empty-lines t)

;;; Cursor

(blink-cursor-mode -1)
(show-paren-mode 1)

;;; Misc

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq ring-bell-function 'ignore)

;; Does not look good and sometimes causes display issues on my Mac but
;; otherwise eshell always scrolls to the right when a process (like homebrew)
;; shows a progress bar.
(setq overflow-newline-into-fringe t)

;; Highlight FIXME/TODO in comments.
(add-hook 'prog-mode-hook #'fic-mode)

(when (not (display-graphic-p))
  (menu-bar-mode -1))

(provide 'luis-look)
;;; luis-look.el ends here
