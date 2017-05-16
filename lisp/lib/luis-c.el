;; -*- lexical-binding: t -*-

;; Use // for comments instead of /* and */.
(defun luis-c-configure-comments ()
  (setq-local comment-start "//")
  (setq-local comment-end ""))

(defun luis-c-comment-box ()
  (interactive)
  (let ((comment-start "/*")
        (comment-end "*/"))
    (call-interactively #'comment-box)))

(use-package cc-mode
  ;; The needed autoloads already exist by default.
  :defer t
  :config
  (setq c-basic-offset tab-width)

  ;; Set default indentation style to K&R instead of weird GNU style. See
  ;; https://www.emacswiki.org/emacs/IndentingC#toc2
  (unless (listp c-default-style)
    (setq c-default-style nil))
  (add-to-list 'c-default-style '(other . "k&r"))

  (add-hook 'c-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'c-mode-hook #'luis-c-configure-comments)
  (add-hook 'c-mode-hook #'luis-flycheck-unless-file-remote))

(use-package irony
  :commands (irony-mode)
  :init
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode))

;;; Completion

(use-package company-irony
  :after company
  :commands (company-irony
             company-irony-setup-begin-commands)
  :init
  (add-to-list 'company-backends #'company-irony)
  (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands))

(use-package company-c-headers
  :after company
  :commands (company-c-headers)
  :init
  ;; Ensure this is executed after `company-irony' is added to
  ;; `company-backends', it must appear in the list first.
  (add-to-list 'company-backends #'company-c-headers))

;;; Syntax Checking

(use-package flycheck-irony
  :after flycheck
  :commands (flycheck-irony-setup)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


(provide 'luis-c)
