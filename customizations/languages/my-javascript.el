(require-package 'js2-mode)

;; Use js2-mode for JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'auto-highlight-symbol-mode)
