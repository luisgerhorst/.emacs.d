;; -*- lexical-binding: t; -*-

(use-package org
  ;; Primarily added because this allows us to run Emacs without having to clone
  ;; anything using HTTPS which is frequently blocked by firewalls.
  :straight (org :type built-in)
  :bind (("C-c a" . org-agenda)
         ("C-c t" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o b" . org-switchb))
  :config
  (add-hook 'org-mode-hook #'auto-revert-mode)
  (add-hook 'org-mode-hook #'auto-save-visited-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-to-list 'org-latex-packages-alist '("ngerman" "babel" t)))

(provide 'luis-org)
