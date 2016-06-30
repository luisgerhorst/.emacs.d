(setq c-basic-offset tab-width)
(add-hook 'c-mode-hook
          #'luis-company-configure-automatic-completion)


(provide 'luis-c)
