;; Eshell

(defun luis-eshell-basename-prompt ()
  (concat (file-name-nondirectory (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ ")))

(use-package eshell
  :bind (("C-c s s" . eshell)
         :map eshell-mode-map
         ("M-m" . eshell-bol))
  :config
  (setq eshell-prompt-function #'luis-eshell-basename-prompt)
  (setq eshell-prompt-regexp "^[^#$\n]* [#$] "))

(use-package company-eshell-autosuggest
  :config
  (add-hook 'eshell-mode-hook #'company-eshell-autosuggest-mode)
  :init
  (setq company-eshell-autosuggest-active-map
        (let ((keymap (make-sparse-keymap)))
          ;; As in ZSH:
          (define-key keymap (kbd "C-e") 'company-complete-selection)
          (define-key keymap (kbd "M-f") 'company-eshell-autosuggest-complete-word)
          keymap))
  (add-hook 'company-eshell-autosuggest-mode-hook
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

(defun luis-close-term-buffer-after-exit ()
  "Close term buffer after exit.

After you close the terminal, you get a useless buffer with no
process. It's probably left there for you to have a history of
what you did. I find it not useful, so here's a way to kill that
buffer automatically"
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(use-package term
  :bind (("C-c s t" . luis-instant-ansi-term)
         :map term-raw-map
         ;; Otherwise C-y is sent to the shell (terminal yank).
         ("C-y" . term-paste))
  :config
  (add-hook 'term-mode-hook #'luis-term-mode-hook)
  (add-hook 'term-exec-hook #'luis-close-term-buffer-after-exit))


(provide 'luis-terminal)
