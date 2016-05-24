(require-package 'jdee)
(require 'jdee)

(add-to-list 'auto-mode-alist '("\\.java\\'" . jdee-mode))

(add-hook 'jdee-mode-hook
          (lambda ()
            (local-set-key (kbd "M-i") 'dabbrev-expand)))

(provide 'luis-java)
