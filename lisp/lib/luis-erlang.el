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
            (setq-local comment-column 0)
            ;; Better shortcuts for moving to beginning / end of a clause
            (local-set-key (kbd "H-a") 'erlang-beginning-of-clause)
            (local-set-key (kbd "H-e") 'erlang-end-of-clause)))

;; EDTS
(require 'edts-start)

(add-to-list 'auto-mode-alist '("/rebar\.config\\'" . erlang-mode))

(add-hook 'edts-mode-hook
          (lambda ()
            ;; Slows down display speed.
            (auto-complete-mode -1)
            ;; Use simple company mode instead.
            (company-mode-on)
            ;; Use comment box with ===
            (local-set-key (kbd "C-;")
                           (lambda (n)
                             (interactive "p")
                             (if (use-region-p)
                                 (my/comment-box-markdown-style)
                               (endless/comment-line (or n 1)))))))

(defun my/comment-box-markdown-style ()
  (when (< (mark) (point)) (exchange-point-and-mark))
  (beginning-of-line)
  (insert "===================================================================\n")
  (previous-line)
  (exchange-point-and-mark)
  (beginning-of-line)
  (next-line)
  (insert "===================================================================\n")
  (comment-region (region-beginning) (region-end)))

;; Yaws

(require 'xml-lite)
(require 'two-mode-mode)
(add-to-list 'auto-mode-alist '("\\.yaws\\'" . two-mode-mode))


(provide 'luis-erlang)