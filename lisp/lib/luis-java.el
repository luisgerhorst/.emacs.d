(require-package 'jdee)
(require 'jdee)

(add-to-list 'auto-mode-alist '("\\.java\\'" . jdee-mode))

(require-package 'company)
(add-hook 'jdee-mode-hook #'company-mode)

(provide 'luis-java)
