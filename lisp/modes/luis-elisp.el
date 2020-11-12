;; -*- lexical-binding: t; -*-

(use-package elisp-mode
  :straight nil
  :mode ("\\.el\\.template\\'" . emacs-lisp-mode)
  :bind (:map
         emacs-lisp-mode-map
         ("C-c r" . eval-buffer))
  :config
  (add-hook 'emacs-lisp-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(provide 'luis-elisp)
