(require-package 'auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-newline-function 'newline-and-indent)

(require-package 'company)
(require-package 'company-auctex)
(require 'company-auctex)
(company-auctex-init)
(add-hook 'LaTeX-mode-hook #'company-mode)

(defun my/latex-mode-hook ()
  ;; Proper line wrapping for text.
  (visual-line-mode 1)
  (auto-fill-mode 1))

(add-hook 'LaTeX-mode-hook 'my/latex-mode-hook)

(defun bjm/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))


(provide 'luis-latex)
