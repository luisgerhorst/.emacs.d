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
  :defer t
  :config
  ;; C
  (setq c-basic-offset tab-width)

  ;; Set default indentation style to K&R instead of weird GNU style. See
  ;; https://www.emacswiki.org/emacs/IndentingC#toc2
  (unless (listp c-default-style)
    (setq c-default-style nil))
  (add-to-list 'c-default-style '(other . "k&r"))

  (add-hook 'c-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'c-mode-hook #'luis-c-configure-comments)
  (add-hook 'c-mode-hook #'luis-flycheck-unless-file-remote)

  ;; cscope
  (add-hook 'c-mode-hook #'helm-cscope-mode)
  (add-hook 'helm-cscope-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
              ;; (local-set-key (kbd "M-@") 'helm-cscope-find-calling-this-function)
              ;; (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
              (local-set-key (kbd "M-,") 'helm-cscope-pop-mark))))

  ;; C++
  (add-hook 'c++-mode-hook #'luis-company-configure-automatic-completion))

(defun luis-irony-unless-file-remote ()
  (let ((current-file (buffer-file-name (current-buffer))))
    (unless (and current-file (file-remote-p current-file))
      (irony-mode 1))))

(use-package irony
  :defer t
  :init
  (add-hook 'c-mode-hook #'luis-irony-unless-file-remote)
  (add-hook 'c++-mode-hook #'luis-irony-unless-file-remote)
  (add-hook 'objc-mode-hook #'luis-irony-unless-file-remote))

;;; Completion

(use-package company-irony
  :defer t
  :init
  (with-eval-after-load 'irony
    (require 'company)
    (add-to-list 'company-backends #'company-irony)
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))

(use-package company-c-headers
  :defer t
  :init
  (with-eval-after-load 'company-irony
    ;; Ensure this is executed after `company-irony' is added to
    ;; `company-backends', it must appear in the list first.
    (add-to-list 'company-backends #'company-c-headers)))

;;; Syntax Checking

(use-package flycheck-irony
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))


(provide 'luis-c)
