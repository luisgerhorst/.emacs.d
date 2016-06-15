;;;; Customizations relating to editing a buffer.

;; Indentation settings.
(require 'luis-indentation)

;; Snippet setup.
(require 'luis-snippet)

(defun die-tabs ()
  "Replace tabs with spaces."
  (interactive)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c r") 'eval-and-replace)

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


;; Auto-delete trailing whitespaces from modified lines.
(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

;; Manually delete all trailing whitespaces.
(global-set-key (kbd "C-c d") #'delete-trailing-whitespace)

(use-package auto-complete
  :config
  (add-hook 'auto-complete-mode-hook
            (lambda ()
              ;; Use M-n and M-p to select next/previous completion and
              ;; use these for moving by line.
              (define-key ac-menu-map (kbd "C-n") nil)
              (define-key ac-menu-map (kbd "C-p") nil))))

(use-package company
  :commands (luis/setq-local-company-backends)
  :bind ("<tab>" . company-complete)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-backends '(company-elisp
                           company-nxml
                           company-css
                           (company-dabbrev-code
                            company-keywords)))
  ;; Used to only enable certain backends in a buffer to avoid possibly
  ;; annoying completions while for example writing comments.
  (defun luis/setq-local-company-backends (local-company-backends)
    (if (company-safe-backends-p local-company-backends)
        (setq-local company-backends local-company-backends)
      (progn
        (message (concat "Warning: '%S did not fullfill "
                         "company-safe-backends-p predicate. "
                         "Automatic completion was disabled in this buffer.")
                 local-company-backends)
        ;; Disable automatic completion if cursor is idle locally.
        (setq-local company-idle-delay nil)))))

;;; Paredit
(use-package paredit
  :commands (enable-paredit-mode))

(defun luis/backwards-kill-sexp (&optional argument)
  (interactive "P")
  (kill-sexp (- (or argument 1))))

;; More handy then C-M-k with negative argument.
(global-set-key (kbd "<C-M-backspace>") #'luis/backwards-kill-sexp)

;; Especially C-w is handy for killing whole lines.
(use-package whole-line-or-region
  :config
  (whole-line-or-region-mode 1))

;; Use to show invisible chars.
(use-package leerzeichen
  :commands (leerzeichen-mode))

;; Join lines. M-^ does not work on my Mac.
(global-set-key (kbd "C-^") #'delete-indentation)

;; Always insert matching brackets.
(electric-pair-mode t)

(setq-default fill-column 80)

(defun luis/resize-window-to-fill-column ()
  (interactive)
  (window-resize (selected-window)
                 (- fill-column (window-total-width))
                 t))

(global-set-key (kbd "C-c m") #'luis/resize-window-to-fill-column)


(provide 'luis-editing)
