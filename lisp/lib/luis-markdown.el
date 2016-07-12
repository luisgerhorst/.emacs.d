(use-package markdown-mode
  :ensure t
  :init
  (defun luis-markdown-mode-hook ()
    ;; Can be enabled in files with unfilled lines.
    (setq-local buffer-face-mode-face '(:family "Input Serif"))
    (buffer-face-mode -1)
    (local-set-key (kbd "C-c f") 'buffer-face-mode))
  (add-hook 'markdown-mode-hook #'luis-markdown-mode-hook)
  (add-hook 'markdown-mode-hook #'luis-text-wrap-mode)
  :mode ("\\.md\\'"
         "\\.markdown\\'"))


(provide 'luis-markdown)
