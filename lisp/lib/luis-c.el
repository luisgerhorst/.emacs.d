(setq c-basic-offset tab-width)

(defun luis/c-mode-hook ()
  (aggressive-indent-mode -1)
  (require-package 'company)
  (require 'company)
  (company-mode-on)
  ;; Prevent annoying completions in comments and strings.
  (luis/setq-local-company-backends '(company-dabbrev-code
                                      company-clang)))

(add-hook 'c-mode-hook 'luis/c-mode-hook)


(provide 'luis-c)
