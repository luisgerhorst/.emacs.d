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
  :commands (luis-code-wrap-mode)
  :init
  (add-hook 'compilation-mode-hook #'luis-code-wrap-mode))

(use-package luis-text-wrap
  :commands luis-text-wrap-mode)

;;; Theme

(use-package solarized-theme
  :defer t
  :init
  ;; Looks much better when using Powerline.
  (setq solarized-high-contrast-mode-line t))

;; See customize group Solarized for options.
(load-theme 'solarized-dark t)

;;; Syntax Checking

(setq flymake-gui-warnings-enabled nil
      flymake-log-level 0)

(use-package flycheck
  :bind ("H-s f" . flycheck-mode)
  :bind-keymap ("H-f" . flycheck-command-map))

;;; Mode Line

(use-package powerline
  :config
  (setq powerline-default-separator 'utf-8)
  (powerline-default-theme)
  (set-face-underline 'mode-line nil)
  (set-face-underline 'mode-line-inactive nil)
  ;; Causes incorrect display of active mode line when Emacs is not focused.
  (remove-hook 'focus-out-hook 'powerline-unset-selected-window))

;;; Whitespaces

(setq-default indicate-empty-lines t)

;;; Cursor

(blink-cursor-mode -1)
(show-paren-mode 1)

;;; Misc

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq ring-bell-function 'ignore)

;; Does not look good and sometimes causes display issues on my Mac.
(setq overflow-newline-into-fringe nil)

;; Display column number in mode line.
(column-number-mode 1)

(use-package fic-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook #'fic-mode))


(provide 'luis-look)
;;; luis-look.el ends here
