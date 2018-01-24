;;;; Customizations relating to editing a buffer.

;;; Indentation

(electric-indent-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 4)

(use-package aggressive-indent
  :delight
  :commands (aggressive-indent-mode)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   ;; Don't indent in Java and C files when line does not contain a
   ;; semicolon or code block bracket.
   '(and (or (derived-mode-p 'jdee-mode)
             (derived-mode-p 'cc-mode)
             (derived-mode-p 'c-mode))
         (not (string-match "[;{}]"
                            (thing-at-point 'line))))))

;;; Completion

(use-package yasnippet
  :delight yas-minor-mode
  :bind (:map
         yas-minor-mode-map
         ("C-c y y" . yas-expand)
         :map
         yas-keymap
         ("C-c y i" . yas-next-field-or-maybe-expand))
  :config
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-keymap [(tab)] nil)
  (define-key yas-keymap (kbd "TAB") nil))


(use-package auto-complete
  :commands (auto-complete-mode)
  :bind (:map
         ac-menu-map
         ;; Use M-n and M-p to select next/previous completion and use
         ;; these for moving by line.
         ("C-n" . nil)
         ("C-p" . nil))
  :init
  (setq ac-auto-show-menu 0.8
        ac-auto-start 3
        ac-use-fuzzy t
        ac-use-menu-map t))

(defun luis-company-configure-completion (idle-delay minimum-prefix-length)
  (setq-local company-idle-delay idle-delay)
  (setq-local company-minimum-prefix-length minimum-prefix-length))

(defun luis-company-configure-automatic-completion ()
  (interactive)
  (luis-company-configure-completion 0.5 2))

;; Used to only enable certain backends in a buffer to avoid possibly
;; annoying completions while for example writing comments.
(defun luis-set-local-company-backends (local-company-backends)
  (if (company-safe-backends-p local-company-backends)
      (setq-local company-backends local-company-backends)
    (message (concat "Warning: '%S did not fullfill "
                     "company-safe-backends-p predicate. "
                     "Automatic completion was disabled in this buffer.")
             local-company-backends)
    ;; Disable automatic completion locally.
    (setq-local company-idle-delay nil)))

(use-package company
  :demand
  :delight
  :bind (("C-M-i" . company-complete))
  :config
  (setq company-idle-delay nil)
  (setq company-frontends '(company-preview-common-frontend
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend))
  (setq company-backends '((company-elisp :with company-dabbrev-code)
                           company-css
                           (company-dabbrev-code company-keywords)))
  (global-company-mode 1))

;;; Filling

(setq-default fill-column 80)

(defun luis-resize-window-to-fill-column ()
  (interactive)
  (window-resize (selected-window)
                 (+ (- fill-column (window-total-width)) 2)
                 t))

;; Disabled because of http://debbugs.gnu.org/cgi/bugreport.cgi?bug=29789
;; (use-package visual-fill-column
;; :commands (visual-fill-column-mode))

;; Auto Fill for comments, enable per major mode.
(use-package luis-comment-auto-fill
  :delight
  :commands (luis-comment-auto-fill-mode)
  :init
  (add-hook 'prog-mode-hook #'luis-comment-auto-fill-mode))

(use-package fillcode
  :delight
  :commands (fillcode-mode)
  :init
  (add-hook 'prog-mode-hook #'fillcode-mode))

;;; Whitespaces

;; Fancier manipulate whitespace function: M-SPC does not work on my Mac, is
;; used for opening Spotlight. If you don't have such problems you may remove
;; the first line.
(global-set-key (kbd "M-#") #'just-one-space)
(global-set-key [remap just-one-space] #'cycle-spacing)

;; Auto-delete trailing whitespaces from modified lines.
(use-package ws-butler
  :delight
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode))

;; Manually delete all trailing whitespaces.
(global-set-key (kbd "C-c d") #'delete-trailing-whitespace)

;;; Refactoring

(use-package iedit
  :bind ("C-." . iedit-mode))

;;; Sexp

(defun luis-backwards-kill-sexp (&optional argument)
  (interactive "P")
  (kill-sexp (- (or argument 1))))

;; More handy then C-M-k with negative argument.
(global-set-key (kbd "<C-M-backspace>") #'luis-backwards-kill-sexp)

;; Always insert matching brackets.
(if (version< emacs-version "25.1")
    (electric-pair-mode 1)
  (add-hook 'prog-mode-hook #'electric-pair-local-mode))


;;; Commenting

(when (version< emacs-version "25.1")
  (defun comment-line (n)
    "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
    (interactive "p")
    (if (use-region-p)
        (comment-or-uncomment-region
         (region-beginning) (region-end))
      (let ((range
             (list (line-beginning-position)
                   (goto-char (line-end-position n)))))
        (comment-or-uncomment-region
         (apply #'min range)
         (apply #'max range)))
      (forward-line 1)
      (back-to-indentation))))

(global-set-key (kbd "C-;") #'comment-line)
(setq comment-style 'extra-line)
(setq-default comment-column 0)

;;; Misc

;; Enable upcase / downcase region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(use-package whole-line-or-region
  :delight
  (whole-line-or-region-global-mode "")
  (whole-line-or-region-local-mode "")
  :init
  (whole-line-or-region-global-mode 1))


(provide 'luis-modification)
