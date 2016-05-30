(require-package 'jdee)
(require 'jdee)

(add-to-list 'auto-mode-alist '("\\.java\\'" . jdee-mode))

(add-hook 'jdee-mode-hook
          (lambda ()
            (company-mode-on)))

(provide 'luis-java)
