;; Eshell

(defun luis-eshell-basename-prompt ()
  (concat (file-name-nondirectory (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ ")))

(use-package eshell
  :bind ("C-c s e" . eshell)
  :config
  (setq eshell-prompt-function #'luis-eshell-basename-prompt)
  (setq eshell-prompt-regexp "^[^#$\n]* [#$] "))

;; Term

(defun luis-no-scroll-margin ()
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
  :bind ("C-c s s" . luis-instant-ansi-term)
  :config
  (add-hook 'term-mode-hook #'luis-no-scroll-margin))


(provide 'luis-terminal)
