;; Work nice together with other apps / Mac OS X.

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
