;; -*- lexical-binding: t; -*-

(use-package paredit
  :diminish (paredit-mode . "")
  :commands (paredit-mode)
  :bind (:map
         paredit-mode-map
         ("M-r" . nil)
         ("M-q" . nil))
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode))

(use-package elisp-mode
  :mode ("\\.el\\.template\\'" . emacs-lisp-mode)
  :bind (:map
         emacs-lisp-mode-map
         ("C-c r" . eval-buffer))
  :config
  (add-hook 'emacs-lisp-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(provide 'luis-elisp)
