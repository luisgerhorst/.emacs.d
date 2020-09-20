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

(global-set-key (kbd "C-c v") #'browse-url-at-point)

;;; Copy/Paste in Terminal Emacs

(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;; Uses the clipboard-copy/paste executables to access the system clipboard from
;; within terminal Emacs.
(when (not (display-graphic-p))
  (defun luis-copy-to-terminal (text)
    "Store the given text into the clipboard."
    (let ((process-connection-type nil))
      (let ((proc (start-process "clipboard-copy" "*Messages*" "clipboard-copy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'luis-copy-to-terminal)

  ;; (defun luis-paste-from-terminal ()
  ;;   "Get the current clipboard contents."
  ;;   (shell-command-to-string "clipboard-paste"))
  ;; TODO: Correctly implement interprogram-paste-function, see help.
  ;; (setq interprogram-paste-function 'luis-paste-from-terminal)
  )

(defun luis-insert-clipboard ()
  "Pastes the current clipboard contents into Emacs.

This function is called from within tmux-smart-paste and
evaluated by emacs server."
  (if (display-graphic-p)
      (error "Trying to paste into GUI emacs.")
    (let ((paste-data (s-trim (shell-command-to-string "clipboard-paste"))))
      ;; When running via emacsclient, paste into the current buffer.  Without
      ;; this, we would paste into the server buffer.
      (with-current-buffer (window-buffer)
        (insert paste-data))
      ;; Add to kill-ring
      (kill-new paste-data))))

;;; Open in Desktop Application

(use-package openwith
  :straight (openwith :fork (:host github :repo "luisgerhorst/openwith"))
  :config
  (when openwith-desktop-environment-open
    (openwith-mode t)))

(provide 'luis-integration)
