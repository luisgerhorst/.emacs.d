(require-package 'jdee)
(require 'jdee)

(defun luis/java-mode-hook ()
  (aggressive-indent-mode -1)
  (require-package 'company)
  (require 'company)
  (company-mode-on)
  ;; Only complete using company-dabbrev-code to prevent annoying
  ;; suggestions in comments and strings.
  ;; I have not tried company-eclim.
  (luis/setq-local-company-backends '(company-dabbrev-code)))
(add-hook 'jdee-mode-hook 'luis/java-mode-hook)

(add-to-list 'auto-mode-alist '("\\.java\\'" . jdee-mode))

(provide 'luis-java)
