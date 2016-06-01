(require-package 'jdee)
(require 'jdee)

(defun luis/java-mode-hook ()
  (aggressive-indent-mode -1))
(add-hook 'jdee-mode-hook 'luis/java-mode-hook)

(require-package 'company)
(require 'company)
(add-hook 'jdee-mode-hook #'company-mode-on)

(add-to-list 'auto-mode-alist '("\\.java\\'" . jdee-mode))

(provide 'luis-java)
