(use-package tex-site ;; AUCTeX overrides LaTeX
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-newline-function #'newline-and-indent)

  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'LaTeX-mode-hook #'flycheck-mode))

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))


(provide 'luis-latex)
