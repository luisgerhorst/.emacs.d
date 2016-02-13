;;;; Customizations relating to editing a buffer.

;; Indentation settings.
(require 'luis-indentation)

(defun die-tabs ()
  "Replace tabs with spaces."
  (interactive)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))


;; Auto Fill for comments, enable per major mode in languages/*.el
(defun my/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

;; Enable upcase / downcase region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Fancier manipulate whitespace function.
;; M-SPC does not work on my Mac, is used for opening Spotlight. If you
;; don't have such problems you may remove the first line.
(global-set-key (kbd "H-SPC") 'just-one-space)
(global-set-key [remap just-one-space] 'cycle-spacing)

;; comment-line if region is inactive, comment-box otherwise.
(global-set-key (kbd "C-;")
                (lambda (n)
                  (interactive "p")
                  (if (use-region-p)
                      (comment-box (region-beginning) (region-end) (or n 0))
                    (endless/comment-line (or n 1)))))

;; `comment-line` function will be built in in Emacs 25.1
;; http://endlessparentheses.com/new-in-emacs-25-1-comment-line.html
(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))


;; Delete trailing whitespaces from modified lines.
(require-package 'ws-butler)
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Manually delete all trailing whitespaces.
(global-set-key (kbd "C-c d") #'delete-trailing-whitespace)

(require-package 'auto-complete)
(require 'auto-complete)
(add-hook 'auto-complete-mode-hook
          (lambda ()
            ;; Use M-n and M-p to select next/previous completion and use these for moving
            ;; by line.
            (define-key ac-menu-map (kbd "C-n") nil)
            (define-key ac-menu-map (kbd "C-p") nil)))

(require-package 'paredit)
(require 'paredit)

;; More handy then C-M-k with negative argument.
(global-set-key (kbd "<C-M-backspace>")
                (lambda (&optional argument)
                  (interactive "P")
                  (kill-sexp (- (or argument 1)))))

;; Use this when you want to enable paredit in a non-lisp.
(defun my/disable-paredit-spaces-before-paren ()
  ;; Function which always returns nil -> never insert a space when insert a parentheses.
  (defun my/erlang-paredit-space-for-delimiter-p (endp delimiter) nil)
  ;; Set this function locally as only predicate to check when determining if a space should be inserted
  ;; before a newly created pair of parentheses.
  (setq-local paredit-space-for-delimiter-predicates '(my/erlang-paredit-space-for-delimiter-p)))

;; Especially C-w is handy for killing whole lines.
(require-package 'whole-line-or-region)
(whole-line-or-region-mode 1)

;; Use to show invisible chars.
(require-package 'leerzeichen)
(require 'leerzeichen)


(provide 'luis-editing)
