;;; Install EDTS not erlang, may cause some strange bug (https://github.com/tjarvstrand/edts/issues/145)
(require-package 'edts)

(require 'erlang)

;; When in erlang-shell-mode:
(add-hook 'erlang-shell-mode-hook 'my/erlang-shell-mode-hook)
(defun my/erlang-shell-mode-hook ()
  ;; Use up/down keys to navigate history.
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))

;; Minor modes for erlang-mode.
(add-hook 'erlang-mode-hook 'my/comment-auto-fill)

;; Indent yanked text.
(add-to-list 'yank-indent-modes 'erlang-mode)

;; Indent newlines and continue comments.
(add-to-list 'erlang-electric-commands 'erlang-electric-newline)

(add-hook 'erlang-mode-hook
          (lambda ()
            ;; Line comments directly after code.
            (setq-local comment-column 0)))

;; EDTS
(require 'edts-start)
