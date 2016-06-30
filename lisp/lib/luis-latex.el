(require-package 'auctex)

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-newline-function #'newline-and-indent)

(add-hook 'LaTeX-mode-hook #'luis-latex-mode-hook)
(add-hook 'LaTeX-mode-hook #'luis-company-configure-automatic-completion)

(defun luis-latex-mode-hook ()
  ;; Proper line wrapping for text.
  (visual-line-mode 1)
  (auto-fill-mode 1))

(provide 'luis-latex)
