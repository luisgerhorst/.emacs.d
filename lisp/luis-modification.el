;;;; Customizations relating to editing a buffer.

;;; Indentation

(electric-indent-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 4)

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode " AggressiveIndent"
  :commands (aggressive-indent-mode)
  :config
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
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs
        `(,(locate-user-emacs-file "snippets")))
  (yas-global-mode 1)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "M-i") #'yas-expand))

(use-package auto-complete
  :ensure t
  :init
  (setq ac-auto-show-menu 0.8
        ac-auto-start 3
        ac-use-fuzzy t
        ac-use-menu-map t)
  :config
  (add-hook 'auto-complete-mode-hook
            (lambda ()
              ;; Use M-n and M-p to select next/previous completion and
              ;; use these for moving by line.
              (define-key ac-menu-map (kbd "C-n") nil)
              (define-key ac-menu-map (kbd "C-p") nil))))

(use-package company-quickhelp
  :ensure t
  :commands (company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 1.5))

(use-package company
  :ensure t
  :demand
  :diminish company-mode
  :bind ("H-i" . company-complete)
  :config
  (setq-default company-idle-delay nil
                company-minimum-prefix-length 2)
  (setq company-backends '(company-elisp
                           company-nxml
                           company-css
                           (company-dabbrev-code
                            company-keywords)))

  (company-quickhelp-mode 1)
  (global-company-mode 1)

  (defun luis-company-configure-completion (idle-delay minimum-prefix-length)
    (setq-local company-idle-delay idle-delay)
    (setq-local company-minimum-prefix-length minimum-prefix-length))

  (defun luis-company-configure-automatic-completion ()
    (interactive)
    (luis-company-configure-completion 0 0))

  ;; Used to only enable certain backends in a buffer to avoid possibly
  ;; annoying completions while for example writing comments.
  (defun luis-set-local-company-backends (local-company-backends)
    (if (company-safe-backends-p local-company-backends)
        (setq-local company-backends local-company-backends)
      (progn
        (message (concat "Warning: '%S did not fullfill "
                         "company-safe-backends-p predicate. "
                         "Automatic completion was disabled in this buffer.")
                 local-company-backends)
        ;; Disable automatic completion locally.
        (setq-local company-idle-delay nil)))))

;;; Filling

(setq-default fill-column 80)

(defun luis-resize-window-to-fill-column ()
  (interactive)
  (window-resize (selected-window)
                 (+ (- fill-column (window-total-width)) 2)
                 t))

(global-set-key (kbd "C-c m") #'luis-resize-window-to-fill-column)

(use-package visual-fill-column
  :ensure t
  :commands (visual-fill-column-mode))

(diminish 'auto-fill-function)

;; Auto Fill for comments, enable per major mode.
(defun luis-comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(use-package fillcode
  :ensure t
  :diminish fillcode-mode
  :commands (fillcode-mode))

(add-hook 'prog-mode-hook #'luis-comment-auto-fill)
(add-hook 'prog-mode-hook #'fillcode-mode)

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
(global-set-key (kbd "H-SPC") #'just-one-space)
(global-set-key [remap just-one-space] #'cycle-spacing)

;; Auto-delete trailing whitespaces from modified lines.
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :commands (ws-butler-mode))

(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Manually delete all trailing whitespaces.
(global-set-key (kbd "C-c d") #'delete-trailing-whitespace)

;;; Sexp

(defun luis-backwards-kill-sexp (&optional argument)
  (interactive "P")
  (kill-sexp (- (or argument 1))))

;; More handy then C-M-k with negative argument.
(global-set-key (kbd "<C-M-backspace>") #'luis-backwards-kill-sexp)

;; Always insert matching brackets.
(electric-pair-mode 1)

;;; Misc

;; Enable upcase / downcase region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Especially C-w is handy for killing whole lines.
(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode 1))

;; Join lines. M-^ does not work on my Mac.
(global-set-key (kbd "C-^") #'delete-indentation)


(provide 'luis-modification)
