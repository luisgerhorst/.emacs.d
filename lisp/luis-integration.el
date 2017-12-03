;;;; Work nice together with other apps / Mac OS X.

;;; Shell

;; This part is very slow, nearly half a second.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  ;; Otherwise -i is included by default which causes .zshrc to be loaded.
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(setenv "LC_ALL" "en_US.UTF-8")


;;; General

(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))


;;; Finder

(defun ido-find-file-in-finder-dir ()
  "ido-find-file-in-dir but start in directory currently open in Finder"
  (interactive)
  (let ((finder-dir (do-applescript "tell application \"Finder\"\nreturn POSIX path of (target of window 1 as alias)\nend tell")))
    (ido-find-file-in-dir finder-dir)))

(use-package reveal-in-osx-finder
  :bind ("C-c z" . reveal-in-osx-finder))


;;; Keep Emacs running when the last window is closed.

;; Directly copied from frame.el but now hide Emacs instead of killing
;; it when last frame will be closed.
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
      (luis-macos-like-close-window))))

(defun luis-macos-like-close-window ()
  (interactive)
  (save-some-buffers)
  (delete-other-windows)
  (switch-to-buffer "*Bookmark List*")
  (ns-do-hide-emacs))

(when  (and (eq system-type 'darwin) window-system)
  (global-set-key [remap suspend-frame] #'luis-macos-like-close-window)
  (advice-add 'handle-delete-frame :override
              #'handle-delete-frame-without-kill-emacs))


(provide 'luis-integration)
