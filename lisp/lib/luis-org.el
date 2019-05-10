;; -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o b" . org-switchb))
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-to-list 'org-latex-packages-alist '("ngerman" "babel" t)))

(provide 'luis-org)
