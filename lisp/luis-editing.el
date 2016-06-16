;;;; Customizations relating to editing a buffer.

;;; Indentation

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)

  (add-to-list
   'aggressive-indent-dont-indent-if
   ;; Don't indent in Java and C files when line does not contain a
   ;; semicolon or code block bracket.
   '(and (or (derived-mode-p 'jdee-mode)
             (derived-mode-p 'c-mode))
         (not (string-match "[;{}]"
                            (thing-at-point 'line))))))

;;; Completion

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
        `(,(expand-file-name "snippets" user-emacs-directory)))

  (yas-global-mode 1))

(use-package auto-complete
  :ensure t
  :config
  (add-hook 'auto-complete-mode-hook
            (lambda ()
              ;; Use M-n and M-p to select next/previous completion and
              ;; use these for moving by line.
              (define-key ac-menu-map (kbd "C-n") nil)
              (define-key ac-menu-map (kbd "C-p") nil))))

(use-package company
  :ensure t
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

;;; Filling

(setq-default fill-column 80)

(defun luis/resize-window-to-fill-column ()
  (interactive)
  (window-resize (selected-window)
                 (- fill-column (window-total-width))
                 t))

(global-set-key (kbd "C-c m") #'luis/resize-window-to-fill-column)

;; Auto Fill for comments, enable per major mode in languages/*.el
(defun my/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

;;; Commenting

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

;;; Whitespaces

;; Fancier manipulate whitespace function:
;; M-SPC does not work on my Mac, is used for opening Spotlight. If you
;; don't have such problems you may remove the first line.
(global-set-key (kbd "H-SPC") 'just-one-space)
(global-set-key [remap just-one-space] 'cycle-spacing)

;; Auto-delete trailing whitespaces from modified lines.
(use-package ws-butler
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

;; Manually delete all trailing whitespaces.
(global-set-key (kbd "C-c d") #'delete-trailing-whitespace)

;;; Sexp

(use-package paredit
  :ensure t
  :commands (enable-paredit-mode))

(defun luis/backwards-kill-sexp (&optional argument)
  (interactive "P")
  (kill-sexp (- (or argument 1))))

;; More handy then C-M-k with negative argument.
(global-set-key (kbd "<C-M-backspace>") #'luis/backwards-kill-sexp)

;; Always insert matching brackets.
(electric-pair-mode t)

;;; Misc

;; Enable upcase / downcase region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Especially C-w is handy for killing whole lines.
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-mode 1))

;; Join lines. M-^ does not work on my Mac.
(global-set-key (kbd "C-^") #'delete-indentation)


(provide 'luis-editing)
