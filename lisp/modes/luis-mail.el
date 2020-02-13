;; -*- lexical-binding: t; -*-

;;;; Mail

;;; Sending Mail.

;; See
;; http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html
;; for a good tutorial.

(setq send-mail-function 'smtpmail-send-it)

;;; Reading Mail.

;; macOS, installed via Homebrew:
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

;; Debian, installed from source:
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :straight nil
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

  ;; When deleting a message in mu4e (i.e. press d in mu4e-headers-mode), the
  ;; message is marked as IMAP deleted and will be removed permanently the next
  ;; time an expunge takes place (e.g. the next time mbsync runs), see
  ;; https://github.com/djcb/mu/issues/1136. In contrast, what desktop mail
  ;; clients usually do, is to just move the message to the Trash folder from
  ;; where the server auto-deletes it after some delay (by default 1 week at
  ;; FastMail), see
  ;; https://www.fastmail.com/help/clients/deleteissue.html?u=4bbc411b.
  ;;
  ;; In mu4e-headers-mode, d marks a message as IMAP deleted AND moves it to the
  ;; trash folder, D only marks a message as IMAP deleted. Thus rebind d to
  ;; only move the message to the Trash folder:
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        (mu4e~proc-move docid
                                        (mu4e~mark-check-target target)
                                        ;; Here's the main difference to the
                                        ;; regular trash mark, no +T before -N
                                        ;; so the message is not marked as
                                        ;; IMAP deleted:
                                        "-N"))))

  ;; On startup, use the first context.
  (setq mu4e-context-policy 'pick-first)
  ;; When composing a blank new message, keep the current context.
  (setq mu4e-compose-context-policy nil)

  (setq mu4e-headers-results-limit 100)

  (defun luis-kill-mu4e-update-process-without-query (run-in-background)
    ;; Name from mu4e-utils.el function mu4e~update-mail-and-index-real. This
    ;; prevents Emacs from asking you if it is ok to kill mbsync when Emacs
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
