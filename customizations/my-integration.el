;; Work nice together with other apps / Mac OS X.

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; For emacsclient.
(server-start)

;; Sets up exec-path-from-shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; Fix weird OS X kill error.
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(defun ido-find-file-in-finder-dir ()
  "ido-find-file-in-dir but start in directory currently open in Finder"
  (interactive)
  (let ((finder-dir (do-applescript "tell application \"Finder\"\nreturn POSIX path of (target of window 1 as alias)\nend tell")))
    (ido-find-file-in-dir finder-dir)))

(global-set-key (kbd "C-x C-v") 'ido-find-file-in-finder-dir)

;; TODO: Make cd silent.
(defun open-directory-in-iterm (directory)
  "Switch to iTerm and cd to directory."
  (do-applescript (concat  "
tell application \"iTerm\"
    activate

    try
        set _session to current session of current terminal
    on error
        set _term to (make new terminal)
        tell _term
            launch session \"Default\"
            set _session to current session
        end tell
    end try

    tell _session
        write text \"cd '" directory "'\"
    end tell
end tell
")))

(defun open-current-directory-in-iterm ()
  "Open active buffer's directory in iTerm."
  (interactive)
  (open-directory-in-iterm (file-name-directory (buffer-file-name))))

(global-set-key (kbd "<f8>") 'open-current-directory-in-iterm)

;; Use ls from GNU coreutils for dired.
(setq insert-directory-program (executable-find "gls"))

;; Focus on emacs when opening file. Doesn't work.
;; (defun activate-emacs (FILENAME &optional WILDCARDS)
;;   (message "activating window")
;;   (do-applescript "tell application \"Emacs\" to activate"))
;; (advice-add 'find-file :after #'activate-emacs)
