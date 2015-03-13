;; "short and sweet LISP editing"
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lispy-mode-hook
          (lambda ()
            ;; That's my buffer switch command (defined in ui.el).
            (define-key lispy-mode-map (kbd "M-o") nil)))

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Auto Fill for comments, function defined in ../my-editing.el
(add-hook 'emacs-lisp-mode-hook 'my/comment-auto-fill)

;; Auto Complete
(add-hook 'emacs-lisp-mode-hook 'global-auto-complete-mode)
