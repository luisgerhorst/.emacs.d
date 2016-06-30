(use-package markdown-mode
  :ensure t
  :init
  (defun luis-markdown-mode-hook ()
    ;; Can be enabled in files with unfilled lines.
    (setq-local buffer-face-mode-face '(:family "Input Serif"))
    (buffer-face-mode -1)
    (local-set-key (kbd "C-c f") 'buffer-face-mode)

    (turn-on-company-ngram)
    (luis-company-configure-completion 0.2 0)

    (subword-mode -1))

  (add-hook 'markdown-mode-hook #'luis-markdown-mode-hook)
  ;; Proper line wrapping for text.
  (add-hook 'markdown-mode-hook #'visual-fill-column-mode)
  :mode ("\\.md\\'"
         "\\.markdown\\'"))


(provide 'luis-markdown)
