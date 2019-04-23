;; -*- lexical-binding: t; -*-

(require 'luis-text-wrap)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook #'luis-text-wrap-mode))

(provide 'luis-markdown)
