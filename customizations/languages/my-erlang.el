;; When in erlang-shell-mode:
(add-hook 'erlang-shell-mode-hook 'my/erlang-shell-mode-hook)
(defun my/erlang-shell-mode-hook ()
  ;; Use up/down keys to navigate history.
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))

;; Minor modes for erlang-mode.
(add-hook 'erlang-mode-hook 'my/comment-auto-fill)
(add-hook 'erlang-mode-hook #'aggressive-indent-mode)

;; ParEdit
(add-hook 'erlang-mode-hook #'enable-paredit-mode)
(add-hook 'erlang-mode-hook 'my/erlang-paredit-mode-hook)
(defun my/erlang-paredit-mode-hook ()
  ;; No space when inserting parentheses.
  (defun my/erlang-paredit-space-for-delimiter-p (endp delimiter) nil)
  (setq-local paredit-space-for-delimiter-predicates '(my/erlang-paredit-space-for-delimiter-p))
  
  ;; No paredit-comment-dwim (inserts lisp comments).
  (local-set-key [remap paredit-comment-dwim] 'comment-dwim))

;; EDTS
(require 'edts-start)
