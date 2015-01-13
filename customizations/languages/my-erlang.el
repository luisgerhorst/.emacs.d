;; When erlang-mode:
(add-hook 'erlang-mode-hook 'my/erlang-mode-hook)
(defun my/erlang-mode-hook ()
  ;; Indent newlines. Using something like
  ;; (global-set-key (kbd "RET") 'newline-and-indent)
  ;; doesn't work with erlang-mode.
  (local-set-key (kbd "RET") '(lambda ()(interactive)(newline)(indent-for-tab-command))))

;; When in erlang-shell-mode:
(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; Use up/down keys to navigate history.
            (local-set-key (kbd "<up>") 'comint-previous-input)
            (local-set-key (kbd "<down>") 'comint-next-input)))

;; Minor modes for erlang-mode. May require edts to work nicely.
(add-hook 'erlang-mode-hook #'auto-complete-mode)
(add-hook 'erlang-mode-hook #'auto-highlight-symbol-mode)

;; Start EDTS.
(require 'edts-start)
