;; -*- lexical-binding: t; -*-

;;;; Mail

;;; Sending Mail.

;; See
;; http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html
;; for a good tutorial.

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

;;; Reading Mail.

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(use-package mu4e
  :defer 30
  ;; Since mu4e is not installed via elpa we have to define the autoloads
  ;; manually.
  :commands (mu4e)
  :config

  ;; Emacs allows you to select an e-mail program as the default program it uses
  ;; when you press C-x m (compose-mail), call report-emacs-bug and so on.
  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-get-mail-command "mbsync -a")
  ;; Required when using mbsync (otherwise duplicate UID errors occur):
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-hide-index-messages t)

  ;; On startup, use the first context.
  (setq mu4e-context-policy 'pick-first)
  ;; When composing a blank new message, keep the current context.
  (setq mu4e-compose-context-policy nil)

  (defun luis-kill-mu4e-update-process-without-query (run-in-background)
    ;; Name from mu4e-utils.el function mu4e~update-mail-and-index-real. This
    ;; prevents Emacs from asking you if it is ok to kill offlineimap when Emacs
    ;; quits and mu4e is currently updating.
    (set-process-query-on-exit-flag (get-process "mu4e-update") nil))

  (advice-add 'mu4e~update-mail-and-index-real :after
              #'luis-kill-mu4e-update-process-without-query)

  (setq mu4e-show-images t)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; add option to view html message in a browser. Type `aV` in view to activate
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)

  (add-hook 'mu4e-compose-mode-hook #'luis-text-wrap-mode)
  (defun luis-mu4e-message-txt-body-will-show (msg)
    (let* ((txt (mu4e-message-field msg :body-txt))
           (html (mu4e-message-field msg :body-html)))
      ;; From `mu4e-message-body-text' definiting in mu4e-message.el
      (and
       ;; does it look like some text? ie., if the text part is more than
       ;; mu4e-view-html-plaintext-ratio-heuristic times shorter than the html
       ;; part, it should't be used. This is an heuristic to guard against 'This
       ;; messages requires html' text bodies.
       (> (* mu4e-view-html-plaintext-ratio-heuristic
             (length txt))
          (length html))
       ;; use html if it's prefered, unless there is no html
       (or (not mu4e-view-prefer-html) (not html)))))
  (defun luis-mu4e-enable-text-wrap-mode-when-plain-text ()
    (if (luis-mu4e-message-txt-body-will-show (mu4e-message-at-point))
        (luis-text-wrap-mode 1)
      (luis-text-wrap-mode -1)))
  (remove-hook 'mu4e-view-mode-hook #'luis-text-wrap-mode)
  (add-hook 'mu4e-view-mode-hook
            #'luis-mu4e-enable-text-wrap-mode-when-plain-text)

  ;; To protect yourself from sending messages too hastily, add a
  ;; final confirmation.
  (add-hook 'message-send-hook
            (lambda ()
              (unless (y-or-n-p "Sure you want to send this?")
                (signal 'quit nil))))

  ;; Load account specific configuration.
  (require 'luis-mail-fastmail)

  ;; Start mu4e in background.
  (mu4e t))

(provide 'luis-mail)
