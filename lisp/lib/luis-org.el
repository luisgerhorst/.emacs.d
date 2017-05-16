(use-package org-mode
  :defer t
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-to-list 'org-latex-packages-alist '("ngerman" "babel" t)))

(provide 'luis-org)
