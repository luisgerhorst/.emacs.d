(defun my/text-mode-hook ()
  (setq buffer-face-mode-face '(:family "Input Serif"))
  (buffer-face-mode)
  (show-paren-mode -1))

;; Markdown

(require-package 'markdown-mode)

(add-hook 'markdown-mode-hook 'my/text-mode-hook)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; reStructuredText

(add-hook 'rst-mode-hook 'my/text-mode-hook)
