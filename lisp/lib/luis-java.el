(require 'luis-ensime)

(use-package jdee
  :ensure t
  :mode ("\\.java\\'" . jdee-mode)
  :commands (jdee-mode)
  :config
  (add-hook 'jdee-mode-hook
            #'luis-company-configure-automatic-completion))

(provide 'luis-java)
