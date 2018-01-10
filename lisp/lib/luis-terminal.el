;; Eshell

(defun luis-eshell-basename-prompt ()
  (concat (file-name-nondirectory (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ ")))

(use-package eshell
  :bind (("C-c s s" . eshell))
  :config
  (setq eshell-prompt-function #'luis-eshell-basename-prompt)
  (setq eshell-prompt-regexp "^[^#$\n]* [#$] ")

  ;; We need the following since eshell-mode-map only is non-nil locally in
  ;; Eshell buffers.
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "M-m") #'eshell-bol)
              (define-key eshell-mode-map (kbd "C-a") nil))))

(use-package esh-autosuggest
  :config
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
  :init
  (setq esh-autosuggest-active-map
        (let ((keymap (make-sparse-keymap)))
          ;; As in ZSH:
          (define-key keymap (kbd "C-e") 'company-complete-selection)
          (define-key keymap (kbd "M-f") 'esh-autosuggest-complete-word)
          keymap))
  (add-hook 'esh-autosuggest-mode-hook
            (lambda () (luis-company-configure-completion 0.0 1))))

;; Term

(defun luis-term-mode-hook ()
  (setq-local scroll-margin 0))

(defun luis-instant-ansi-term ()
  "Open `ansi-term' without asking for shell to use"
  (interactive)
  (ansi-term
   ;; Copied from term.el
   (or explicit-shell-file-name
       (getenv "ESHELL")
       (getenv "SHELL")
       "/bin/sh")))

(use-package term
  :bind (("C-c s t" . luis-instant-ansi-term)
         :map term-raw-map
         ;; Otherwise C-y is sent to the shell (terminal yank).
         ("C-y" . term-paste))
  :config
  (add-hook 'term-mode-hook #'luis-term-mode-hook))


(provide 'luis-terminal)
