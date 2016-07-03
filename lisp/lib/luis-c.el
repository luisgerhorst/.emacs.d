(setq c-basic-offset tab-width)
(add-hook 'c-mode-hook
          #'luis-company-configure-automatic-completion)

(use-package company-c-headers
  :ensure t
  :after (company)
  :commands (company-c-headers)
  :init
  (add-to-list 'company-backends #'company-c-headers))


(provide 'luis-c)
