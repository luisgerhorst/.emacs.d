;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Start server to allow opening files from shell
;; (server-start)

;; Discover Emacs with popup buffers.
(require-package 'discover)
(require 'discover)
(global-discover-mode t)

;; Focus on emacs when opening file. Doesn't work.
;; (defun activate-emacs (FILENAME &optional WILDCARDS)
;;   (message "activating window")
;;   (do-applescript "tell application \"Emacs\" to activate"))
;; (advice-add 'find-file :after #'activate-emacs)

;; Always prefer newer versions of a file.
(setq load-prefer-newer t)

(defun test-emacs-config ()
  "Start shell Emacs in background to test config."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"(condition-case e (progn (load \\\"~/.emacs.d/init.el\\\") (message \\\"-OK-\\\")) (error (message \\\"ERROR!\\\") (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "Emacs config ok"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

(require-package 'god-mode)
(require 'god-mode)
(global-set-key (kbd "<f7>") 'god-local-mode)

(defun my/god-mode-update-cursor ()
  "Cursor is vertical bar if in god-mode."
  (setq cursor-type (if god-local-mode 'bar 'box)))
(add-hook 'god-mode-enabled-hook 'my/god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'my/god-mode-update-cursor)

(setq magit-last-seen-setup-instructions "1.4.0")
