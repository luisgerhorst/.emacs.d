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
  (add-hook 'c-mode-hook #'luis-c-configure-comments))

(use-package company-c-headers
  :ensure t
  :after (company)
  :commands (company-c-headers)
  :init
  (add-to-list 'company-backends #'company-c-headers))


(provide 'luis-c)
