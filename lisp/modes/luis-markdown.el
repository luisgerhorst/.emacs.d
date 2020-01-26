;; -*- lexical-binding: t; -*-

(require 'virtual-auto-fill)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook #'virtual-auto-fill-mode))

(provide 'luis-markdown)
