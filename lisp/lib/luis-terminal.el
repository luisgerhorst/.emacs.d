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

(defun oleh-term-exec-hook ()
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
  :bind (("C-c s s" . luis-instant-ansi-term)
         :map term-raw-map
         ("C-y" . term-paste))
  :config
  (add-hook 'term-mode-hook #'luis-no-scroll-margin))


(provide 'luis-terminal)
