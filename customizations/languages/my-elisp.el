;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Auto Fill for comments, function defined in ../my-editing.el
(add-hook 'emacs-lisp-mode-hook 'my/comment-auto-fill)

;; Auto Complete
(add-hook 'emacs-lisp-mode-hook 'global-auto-complete-mode)

;; Evil
(evil-leader/set-key-for-mode 'emacs-lisp-mode "c A" 'paredit-comment-dwim)
(require 'paredit)
(advice-add 'paredit-comment-dwim :after
            (lambda (&optional argument) (evil-append 1)))
