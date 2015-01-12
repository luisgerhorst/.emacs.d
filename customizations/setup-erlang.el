;; When erlang-mode:
(add-hook 'erlang-mode-hook
          (lambda ()
            ;; Indent newlines.
            (local-set-key (kbd "RET") '(lambda ()(interactive)(newline)(erlang-indent-line)))))
;; When in erlang-shell-mode:
(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; Use up/down keys to navigate history.
            (local-set-key (kbd "<up>") 'comint-previous-input)
            (local-set-key (kbd "<down>") 'comint-next-input)))
;; start EDTS
(require 'edts-start)
