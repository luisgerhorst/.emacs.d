(use-package jdee
  :ensure t
  :mode "\\.java\\'"
  :commands (jdee-mode))

(add-hook 'jdee-mode-hook #'company-mode)


(provide 'luis-java)
