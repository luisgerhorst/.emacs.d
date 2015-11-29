(require-package 'auctex)

(require-package 'company)
(require-package 'company-auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-newline-function 'newline-and-indent)

(defun my/latex-mode-hook ()
  ;; Proper line wrapping for text.
  (visual-line-mode 1)
  (auto-fill-mode 1))
(add-hook 'LaTeX-mode-hook 'my/latex-mode-hook)
