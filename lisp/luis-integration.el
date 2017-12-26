;;;; Work nice together with other apps / Mac OS X.

;;; Shell

;; This part is very slow, nearly half a second.
(use-package exec-path-from-shell
  :if (and (display-graphic-p) (eq system-type 'darwin))
  :config
  ;; Otherwise -i is included by default which causes .zshrc to be loaded.
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(setenv "LC_ALL" "en_US.UTF-8")


;;; General

(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;; Integrate with the macOS pasteboard even when running in a terminal by using
;; external commands.
(when (and (eq system-type 'darwin) (not (display-graphic-p)))
  (defun luis-copy-from-macos ()
    (shell-command-to-string "pbpaste"))

  (defun luis-paste-to-macos (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'luis-paste-to-macos)
  (setq interprogram-paste-function 'luis-copy-from-macos))

;; We don't start the server here. When using emacsclient we instead use
;; --alternate-editor="", this will start Emacs as a deamon and then start the
;; server in that instance. This way we always connect to the Emacs deamon when
;; using emacsclient and not some random instance started in a terminal.

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
      (luis-ns-close-window))))

(defun luis-ns-close-window ()
  (interactive)
  (save-some-buffers)
  (delete-other-windows)
  (switch-to-buffer "*Bookmark List*")
  (ns-do-hide-emacs))

(when  (and (display-graphic-p) (eq system-type 'darwin))
  (global-set-key [remap suspend-frame] #'luis-ns-close-window)
  (advice-add 'handle-delete-frame :override
              #'handle-delete-frame-without-kill-emacs))


(provide 'luis-integration)
