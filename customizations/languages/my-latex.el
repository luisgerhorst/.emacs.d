(require-package 'auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-newline-function 'newline-and-indent)

(defun my/latex-mode-hook ()
  (auto-fill-mode 1))
(add-hook 'LaTeX-mode-hook 'my/latex-mode-hook)
