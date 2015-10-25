;;; Markdown

(require-package 'markdown-mode)

;; Can be enabled in files with unfilled lines.
(define-key markdown-mode-map (kbd "C-c f") 'buffer-face-mode)

(defun my/markdown-mode-hook ()
  (setq-local buffer-face-mode-face '(:family "Input Serif"))
  (buffer-face-mode -1)

  (show-paren-mode -1)
  (visual-line-mode 1)
  (auto-fill-mode 1))

(add-hook 'markdown-mode-hook 'my/markdown-mode-hook)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
