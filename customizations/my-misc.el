;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Start server to allow opening files from shell
(server-start)

;; Discover Emacs with popup buffers.
(require 'discover)
(global-discover-mode t)

;; Focus on emacs when opening file. Doesn't work.
;; (defun activate-emacs (FILENAME &optional WILDCARDS)
;;   (message "activating window")
;;   (do-applescript "tell application \"Emacs\" to activate"))
;; (advice-add 'find-file :after #'activate-emacs)

;; Always prefer newer versions of a file.
(setq load-prefer-newer t)

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Start emacs in background to test config.
(defun test-emacs-config ()
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
