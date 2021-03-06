;; -*- lexical-binding: t; -*-

;;;; Customizations relating to editing a buffer.

(prefer-coding-system 'utf-8-unix)

;;; Indentation

(electric-indent-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 4)

(use-package aggressive-indent
  :diminish ""
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

;;; Filling

(setq-default fill-column 80)

(use-package fillcode
  :defer t
  :diminish fillcode-mode
  :init
  (add-hook 'prog-mode-hook #'fillcode-mode))

(use-package luis-comment-auto-fill
  :straight nil
  :commands (luis-comment-auto-fill-mode)
  :init
  (add-hook 'prog-mode #'luis-comment-auto-fill-mode))

(defun luis-use-hard-newlines-t-guess ()
  "Distinct between hard and soft newlines but don't ask the user.
When composing mail, this automatically sets format=flowed, see
https://github.com/djcb/mu/issues/569."
  (use-hard-newlines t 'guess))
;; This should also enable it in `mu4e-compose-mode' as it's parent mode is
;; `message-mode', whose parent mode is `text-mode'.
(add-hook 'text-mode-hook #'luis-use-hard-newlines-t-guess)
(add-hook 'text-mode-hook #'luis-use-hard-newlines-t-guess)

;;; Whitespaces

;; Fancier manipulate whitespace function: M-SPC does not work on my Mac, is
;; used for opening Spotlight. If you don't have such problems you may remove
;; the first line.
(global-set-key (kbd "M-#") #'just-one-space)
(global-set-key [remap just-one-space] #'cycle-spacing)

;; Auto-delete trailing whitespaces from modified lines.
(use-package ws-butler
  :defer t
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode))

;; Manually delete all trailing whitespaces.
(global-set-key (kbd "C-c d") #'delete-trailing-whitespace)

;;; Refactoring

(use-package iedit
  :bind ("C-c i" . iedit-mode))

;;; Sexp

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

(global-set-key (kbd "C-\\") #'comment-line)

;;; Misc

;; Enable upcase / downcase region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(provide 'luis-editing)
