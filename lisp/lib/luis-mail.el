;;;; Mail

;;; Sending Mail.

;; See
;; http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html
;; for a good tutorial.

(setq send-mail-function 'smtpmail-send-it)

;;; Reading Mail.

(use-package mu4e
  :ensure nil
  :defer 10
  :commands (mu4e mu4e-compose-new)
  :config

  ;; Emacs allows you to select an e-mail program as the default program
  ;; it uses when you press C-x m (compose-mail), call report-emacs-bug
  ;; and so on.
  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-hide-index-messages t)

  ;; First one is the default fallback context.
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)

  (defun luis-kill-mu4e-update-process-without-query (run-in-background)
    ;; Name from mu4e-utils.el function mu4e~update-mail-and-index-real.
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

  (use-package visual-fill-column
    :commands (my/mu4e-compose-mode-hook)
    :config
    (defun my/mu4e-compose-mode-hookp ()
      "My settings for message composition."
      (visual-line-mode 1)
      (auto-fill-mode -1)
      (visual-fill-column-mode 1)))

  (add-hook 'mu4e-compose-mode-hook 'my/mu4e-compose-mode-hook)

  ;; To protect yourself from sending messages too hastily, add a
  ;; final confirmation.
  (add-hook 'message-send-hook
            (lambda ()
              (unless (yes-or-no-p "Sure you want to send this?")
                (signal 'quit nil))))

  ;;; Get notified when new mails arrive.
  (require 'mu4e-alert)
  (mu4e-alert-set-default-style 'notifier)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

  ;; Load account specific configuration.
  (require 'luis-mail-private)

  ;; Start mu4e in background.
  (mu4e t))


(provide 'luis-mail)
