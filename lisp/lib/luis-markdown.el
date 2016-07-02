(use-package markdown-mode
  :ensure t
  :init
  (defun luis-markdown-mode-hook ()
    ;; Can be enabled in files with unfilled lines.
    (setq-local buffer-face-mode-face '(:family "Input Serif"))
    (buffer-face-mode -1)
    (local-set-key (kbd "C-c f") 'buffer-face-mode)

    (subword-mode -1)

    ;; Together with visual-fill-column-mode:
    (setq-local truncate-lines nil))

  (add-hook 'markdown-mode-hook #'luis-markdown-mode-hook)
  ;; Proper line wrapping for text.
  (add-hook 'markdown-mode-hook #'visual-fill-column-mode)
  :mode ("\\.md\\'"
         "\\.markdown\\'"))


(provide 'luis-markdown)
