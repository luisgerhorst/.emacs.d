(require-package 'haskell-mode)

;; Haskell indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Use Haskell mode for Elm
(add-to-list 'auto-mode-alist '("\\.elm\\'" . haskell-mode))
