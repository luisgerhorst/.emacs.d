;; -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :bind (("C-c a" . org-agenda)
         ("C-c o l" . org-store-link)
         ("C-c o c" . org-capture)
         ("C-c o b" . org-switchb))
  :config
  (add-hook 'org-mode-hook #'auto-revert-mode)
  (add-hook 'org-mode-hook #'auto-save-visited-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-to-list 'org-latex-packages-alist '("ngerman" "babel" t))
  (setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE"))))

(provide 'luis-org)
