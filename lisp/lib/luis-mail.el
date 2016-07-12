;;;; Mail

;;; Sending Mail.

;; See
;; http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html
;; for a good tutorial.

(setq send-mail-function 'smtpmail-send-it)

;;; Reading Mail.

(use-package mu4e
  :demand
  :ensure nil
  :config

  ;; Emacs allows you to select an e-mail program as the default program it uses
  ;; when you press C-x m (compose-mail), call report-emacs-bug and so on.
  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-hide-index-messages t)

  ;; First one is the default fallback context.
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)

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

  ;; add option to view html message in a browser
  ;; `aV` in view to activate
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)
  (add-hook 'mu4e-compose-mode-hook #'luis-text-wrap-mode)

  ;; To protect yourself from sending messages too hastily, add a
  ;; final confirmation.
  (add-hook 'message-send-hook
            (lambda ()
              (unless (yes-or-no-p "Sure you want to send this?")
                (signal 'quit nil))))

  ;; Load account specific configuration.
  (require 'luis-mail-private)

  ;; Start mu4e in background.
  (mu4e t))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'notifier)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))


(provide 'luis-mail)
