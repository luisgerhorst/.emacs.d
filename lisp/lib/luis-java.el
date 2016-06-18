(use-package jdee
  :ensure t
  :mode ("\\.java\\'" . jdee-mode)
  :commands (jdee-mode)
  :config
  (add-hook 'jdee-mode-hook #'company-mode))


(provide 'luis-java)
