(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'"
         "\\.markdown\\'"))

(defun luis-markdown-mode-hook ()
  ;; Can be enabled in files with unfilled lines.
  (setq-local buffer-face-mode-face '(:family "Input Serif"))
  (buffer-face-mode -1)
  (local-set-key (kbd "C-c f") 'buffer-face-mode)

  ;; Disable modes usefull for programming.
  (show-paren-mode -1)
  (subword-mode -1)

  ;; Proper line wrapping for text.
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(add-hook 'markdown-mode-hook #'luis-markdown-mode-hook)


(provide 'luis-markdown)
