(use-package tex-site ;; AUCTeX overrides LaTeX
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-newline-function #'newline-and-indent)
  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'LaTeX-mode-hook #'flycheck-mode))

(use-package company-auctex
  :commands (company-auctex-init)
  :init
  (add-hook 'LaTeX-mode-hook #'company-auctex-init))

(provide 'luis-latex)
