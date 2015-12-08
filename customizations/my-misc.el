(setq magit-last-seen-setup-instructions "1.4.0")

;;; Call when you are done with a task. Better than quitting Emacs.

(defun end-emacs-session ()
  (interactive)
  (save-some-buffers)
  (delete-other-windows)
  (command-execute 'bookmark-bmenu-list)
  (ns-do-hide-emacs))

(global-set-key [remap save-buffers-kill-terminal] 'end-emacs-session)
(global-set-key (kbd "C-c c") 'save-buffers-kill-emacs)
