;; Markdown
(add-hook 'markdown-mode-hook 'luis/markdown-mode-hook)
(defun luis/markdown-mode-hook ()
  (setq buffer-face-mode-face '(:family "Input Serif"))
  (buffer-face-mode))
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
