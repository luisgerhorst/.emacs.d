;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 4)
(setq-default sh-indentation 4)

;; Start server to allow opening files from shell
(server-start)

;; Discover
(require 'discover)
(global-discover-mode t)

;; Focus on emacs when opening file. Doesn't work.
;; (defun activate-emacs (FILENAME &optional WILDCARDS)
;;   (message "activating window")
;;   (do-applescript "tell application \"Emacs\" to activate"))
;; (advice-add 'find-file :after #'activate-emacs)
