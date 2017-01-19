(add-hook 'nxml-mode-hook #'luis-company-configure-automatic-completion)
(add-hook 'nxml-mode-hook
          (lambda ()
            (luis-set-local-company-backends '((company-nxml company-dabbrev)))))

(provide 'luis-xml)
