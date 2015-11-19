;;; Markdown

(require-package 'markdown-mode)

(defun my/markdown-mode-hook ()
  ;; Can be enabled in files with unfilled lines.
  (setq-local buffer-face-mode-face '(:family "Input Serif"))
  (buffer-face-mode -1)
  (local-set-key (kbd "C-c f") 'buffer-face-mode)

  ;; Disable modes usefull for programming.
  (show-paren-mode -1)
  (subword-mode -1)

  ;; Prper line wrapping for text.
  (visual-line-mode 1)
  (require-package 'visual-fill-column)
  (require 'visual-fill-column)
  (visual-fill-column-mode 1))

(add-hook 'markdown-mode-hook 'my/markdown-mode-hook)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

