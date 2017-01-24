;;;; Work nice together with other apps / Mac OS X.

;;; Shell

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH")))

;;; Mac

(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(defun ido-find-file-in-finder-dir ()
  "ido-find-file-in-dir but start in directory currently open in Finder"
  (interactive)
  (let ((finder-dir (do-applescript "tell application \"Finder\"\nreturn POSIX path of (target of window 1 as alias)\nend tell")))
    (ido-find-file-in-dir finder-dir)))

(when (eq system-type 'darwin)
  (global-set-key [remap suspend-frame] 'ns-do-hide-emacs))

;; Directly copied from frame.el but now hide Emacs instead of killing
;; it when last frame will be closed.
;; TODO: Replace ns-do-hide-emacs with a function that does everything
;; that would also happen if Emacs is closed and reopened, aka
;; save-some-buffers, revert all unsaved buffers, reset the frame to the
;; default configuration and finally ns-do-hide-emacs.
(defun handle-delete-frame-without-kill-emacs (event)
  "Handle delete-frame events from the X server."
  (interactive "e")
  (let ((frame (posn-window (event-start event)))
        (i 0)
        (tail (frame-list)))
    (while tail
      (and (frame-visible-p (car tail))
           (not (eq (car tail) frame))
           (setq i (1+ i)))
      (setq tail (cdr tail)))
    (if (> i 0)
        (delete-frame frame t)
      ;; Not (save-buffers-kill-emacs) but instead:
      (ns-do-hide-emacs))))

(when (eq system-type 'darwin)
  (advice-add 'handle-delete-frame :override
              #'handle-delete-frame-without-kill-emacs))


(provide 'luis-integration)
