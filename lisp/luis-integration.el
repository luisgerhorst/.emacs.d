;; -*- lexical-binding: t; -*-

;;;; Work nice together with other apps / Mac OS X.

;;; Shell

;; This part is very slow, nearly half a second.
(use-package exec-path-from-shell
  :if (and (display-graphic-p) (eq system-type 'darwin))
  :config
  ;; Otherwise -i is included by default which causes .zshrc to be loaded.
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;;; General

(when (not (display-graphic-p))
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4]
                  (lambda () (interactive) (scroll-down 2)))
  (global-set-key [mouse-5]
                  (lambda () (interactive) (scroll-up 2))))

;; We don't start the server here. When using emacsclient we instead use
;; --alternate-editor="", this will start Emacs as a deamon and then start the
;; server in that instance. This way we always connect to the Emacs deamon when
;; using emacsclient and not some random instance started in a terminal.

;;; Copy/Paste in Terminal Emacs

(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;;; Open in Desktop Application

(use-package openwith
  :straight (openwith :fork (:host github :repo "luisgerhorst/openwith"))
  :config
  (when openwith-desktop-environment-open
    (openwith-mode t)))

(defun xah-open-in-external-app (&optional fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
  (interactive)
  (let* (
         (file-list
          (if fname
              (progn (list fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         (do-it-p (if (<= (length file-list) 5)
                      t
                    (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fpath)
           (w32-shell-execute "open" fpath))
         file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fpath)
           (shell-command
            (concat "open " (shell-quote-argument fpath))))
         file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fpath) (let ((process-connection-type nil))
                           (start-process "" nil "xdg-open" fpath)))
         file-list))))))

(global-set-key (kbd "C-c v") #'xah-open-in-external-app)

(provide 'luis-integration)
