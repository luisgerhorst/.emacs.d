;; -*- lexical-binding: t; -*-

(add-hook 'makefile-mode-hook
          #'luis-company-configure-automatic-completion)

(provide 'luis-make)
