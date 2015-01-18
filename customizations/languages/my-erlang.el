;; Start EDTS
(require 'edts-start)

;; Load Erlang with Homebrew path and MELPA package.
(setq erlang-root-dir "/usr/local/Cellar/erlang/17.3.4/lib/erlang")
;; Redundant because erlang should already be in the exec-path from shell, but that's ok.
(setq exec-path (cons "/usr/local/Cellar/erlang/17.3.4/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; When erlang-mode:
(add-hook 'erlang-mode-hook 'my/erlang-mode-hook)
(defun my/erlang-mode-hook ()
  ;; Indent newlines. Using something like
  ;; (global-set-key (kbd "RET") 'newline-and-indent)
  ;; doesn't work with erlang-mode.
  (local-set-key (kbd "RET") '(lambda ()(interactive)(newline)(indent-for-tab-command))))

;; When in erlang-shell-mode:
(add-hook 'erlang-shell-mode-hook 'my/erlang-shell-mode-hook)
(defun my/erlang-shell-mode-hook ()
  ;; Use up/down keys to navigate history.
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))

;; Minor modes for erlang-mode. May require edts to work nicely.
(add-hook 'erlang-mode-hook #'auto-complete-mode)
(add-hook 'erlang-mode-hook #'auto-highlight-symbol-mode)
