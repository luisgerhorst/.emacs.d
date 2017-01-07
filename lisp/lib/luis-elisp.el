(use-package paredit
  :diminish (paredit-mode . "")
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
         ("C-c e" . eval-buffer))
  :config
  (add-hook 'emacs-lisp-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(defun test-emacs-config ()
  "Start shell Emacs in background to test config."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"(condition-case e (progn (load \\\"~/.emacs.d/init.el\\\") (message \\\"-OK-\\\")) (error (message \\\"ERROR!\\\") (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "Emacs config ok"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

(use-package auto-compile
  :config
  (auto-compile-on-save-mode))


(provide 'luis-elisp)
