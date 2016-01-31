(require-package 'auctex)

(require-package 'company)
(require-package 'company-auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-newline-function 'newline-and-indent)

(defun my/latex-mode-hook ()
  ;; Proper line wrapping for text.
  (visual-line-mode 1)
  (auto-fill-mode 1)
  
  ;; Only auto-complete using auctex backends (not regular words in
  ;; text).
  (require 'company-auctex)
  (setq company-backends '())
  (company-auctex-init)
  ;; Make suggestions immediately.
  (setq company-dabbrev-time-limit 0.0)
  (setq company-dabbrev-minimum-length 2)
  (company-mode-on))

(add-hook 'LaTeX-mode-hook 'my/latex-mode-hook)