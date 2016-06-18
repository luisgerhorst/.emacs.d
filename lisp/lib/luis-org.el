(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)

(use-package org-mode
  :defer t
  :config
  (message "org-mode configured")
  (add-to-list 'org-latex-packages-alist '("ngerman" "babel" t)))


(provide 'luis-org)
