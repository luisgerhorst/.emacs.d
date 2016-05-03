(require-package 'nasm-mode)
(require 'nasm-mode)

(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))


(provide 'luis-asm)
