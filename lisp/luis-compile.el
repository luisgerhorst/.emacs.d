;; -*- lexical-binding: t; -*-

;;; Compilation

(setq compilation-ask-about-save nil)
(global-set-key (kbd "C-c c") #'compile)

;;; Language Server Protocol

;; To enable this in a specific language, add
;;
;; (use-package lang-mode
;;   :config
;;   (add-hook 'lang-mode-hook #'lsp))
;;
;; to 'lib/luis-lang.el'.
(use-package lsp-mode
  :defer t)

;; For configuration of `lsp-ui', see 'M-x customize-group lsp-ui'.

;;; Syntax Checking

(setq flymake-gui-warnings-enabled nil
      flymake-log-level 0)

(use-package flycheck
  :commands (flycheck-mode)
  :bind-keymap ("C-c !" . flycheck-command-map))

(defun luis-flycheck-unless-file-remote ()
  (let ((current-file (buffer-file-name (current-buffer))))
    (unless (and current-file (file-remote-p current-file))
      (flycheck-mode 1))))

;;; Misc

(setq load-prefer-newer t)

(provide 'luis-compile)
