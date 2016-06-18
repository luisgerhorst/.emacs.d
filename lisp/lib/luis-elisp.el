(use-package paredit
  :commands (enable-paredit-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(progn
  ;; eldoc-mode shows documentation in the minibuffer when writing code
  ;; http://www.emacswiki.org/emacs/ElDoc
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'company-mode-on)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c e") 'eval-buffer)))

(add-to-list 'auto-mode-alist '("\\.el.template\\'" . emacs-lisp-mode))


(provide 'luis-elisp)
