(require-package 'auctex)

(require-package 'company)
(require-package 'company-auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-newline-function 'newline-and-indent)

(require 'company-auctex)
(company-auctex-init)

(defun my/latex-mode-hook ()
  ;; Proper line wrapping for text.
  (visual-line-mode 1)
  (auto-fill-mode 1)
  ;; Company.
  (company-mode-on))

(add-hook 'LaTeX-mode-hook 'my/latex-mode-hook)

(defun bjm/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))


(provide 'luis-latex)
