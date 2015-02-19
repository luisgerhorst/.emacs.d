;; When erlang-mode:
(add-hook 'erlang-mode-hook 'my/erlang-mode-hook)
(defun my/erlang-mode-hook ()

  ;; Indent newlines. Using something like
  ;; (global-set-key (kbd "RET") 'newline-and-indent) 
  ;; doesn't work with erlang-mode.
  (local-set-key (kbd "RET") '(lambda ()(interactive)(newline)(indent-for-tab-command)))

  ;; No spaces when inserting parenthesis
  (setq-local parens-require-space nil)
  (setq-local parens-require-spaces nil)

  (defun my/erlang-paredit-space-for-delimiter-p (endp delimiter)
    nil)
  (setq-local paredit-space-for-delimiter-predicates '(my/erlang-paredit-space-for-delimiter-p)))

;; When in erlang-shell-mode:
(add-hook 'erlang-shell-mode-hook 'my/erlang-shell-mode-hook)
(defun my/erlang-shell-mode-hook ()
  ;; Use up/down keys to navigate history.
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))

;; Minor modes for erlang-mode.
(add-hook 'erlang-mode-hook 'my/comment-auto-fill)
(add-hook 'erlang-mode-hook #'enable-paredit-mode)
(add-hook 'erlang-mode-hook #'aggressive-indent-mode)

;; Reindent yanked lines
(add-to-list 'yank-indent-modes 'erlang-mode)

;; EDTS
(require 'edts-start)
