;; -*- lexical-binding: t -*-
;;;; For things concerning the way it looks and what is highlighted.

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
  :commands (luis-text-wrap-mode))

;;; Mode Line and Theme

(when (display-graphic-p)
  (use-package powerline
    :config
    (set-face-underline 'mode-line nil)
    (set-face-underline 'mode-line-inactive nil)
    ;; Causes incorrect display of active mode line when Emacs is not focused.
    (remove-hook 'focus-out-hook 'powerline-unset-selected-window)))

(defun luis-theme-set (new background-mode powerline-seperator)
  (when (display-graphic-p)
    (setq powerline-default-separator powerline-seperator)
    (powerline-default-theme))

  (mapc #'disable-theme custom-enabled-themes)
  (load-theme new t)

  (setq frame-background-mode background-mode)
  (frame-set-background-mode (selected-frame)))

(defun luis-theme-zenburn ()
  (luis-theme-set 'zenburn 'dark nil))

(defun luis-theme-adwaita ()
  (luis-theme-set 'adwaita  'light nil))

(defun luis-theme-toggle ()
  (interactive)
  (if (custom-theme-enabled-p 'zenburn)
      (luis-theme-adwaita)
    (luis-theme-zenburn)))

(luis-theme-zenburn)


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

;; Display column number in mode line.
(column-number-mode 1)

;; Highlight FIXME/TODO in comments.
(add-hook 'prog-mode-hook #'fic-mode)

(when (not (display-graphic-p))
  (menu-bar-mode -1))


(provide 'luis-look)
;;; luis-look.el ends here
