(use-package cc-mode
  :config
  (setq c-basic-offset tab-width)

  ;; Set default indentation style to K&R instead of weird GNU style. See
  ;; https://www.emacswiki.org/emacs/IndentingC#toc2
  (unless (listp c-default-style)
    (setq c-default-style nil))
  (add-to-list 'c-default-style '(other . "k&r"))

  ;; Use // for comments instead of /* and */.
  (defun luis-c-configure-comments ()
    (setq-local comment-start "//")
    (setq-local comment-end ""))

  (add-hook 'c-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'c-mode-hook #'luis-c-configure-comments)
  (add-hook 'c-mode-hook #'flycheck-mode))

(use-package irony
  :commands (irony-mode)
  :init
  (add-hook 'c-mode-hook #'irony-mode))

(use-package company-irony
  :after company
  :commands (company-irony)
  :init
  (add-to-list 'company-backends
               '(:separate
                 company-irony
                 :with company-dabbrev-code company-keywords))
  (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands))

(use-package company-c-headers
  :after (company)
  :commands (company-c-headers)
  :init
  (add-to-list 'company-backends #'company-c-headers))


(provide 'luis-c)
