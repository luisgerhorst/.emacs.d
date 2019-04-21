;; -*- lexical-binding: t; -*-

;;; Sending

;; user-mail-address / user-full-name set using EMAIL / NAME in .zprofile
(setq smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "mumble@fastmail.com"
      smtpmail-local-domain "fastmail.com")

;; (let* ((luis-mu4e-interesting-mail-query
;;         "maildir:\"/Inbox\" OR maildir:\"/Arbeit\" OR maildir:\"/Uni\" OR maildir:\"/Uni Lists\"")
;;        (luis-mu4e-interesting-unread-mail-query
;;         (concat "flag:unread AND (" luis-mu4e-interesting-mail-query ")")))

;;; Alerts

;; (setq mu4e-alert-interesting-mail-query
;;       luis-mu4e-interesting-unread-mail-query)

;;; Reading

;; The following variables are account specific.
(setq mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder "/Sent Items"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      ;; mu4e-maildir-shortcuts
      ;; '(("/Inbox" . ?i)
      ;;   ("/Arbeit" . ?w)
      ;;   ("/Uni" . ?u)
      ;;   ("/Uni Lists" . ?l)
      ;;   ("/Sent Items" . ?s)
      ;;   ("/Dokumente" . ?d))
      ;; mu4e-bookmarks
      ;; `((,luis-mu4e-interesting-mail-query
      ;;    "Personal messages"
      ;;    ?p)
      ;;   (,luis-mu4e-interesting-unread-mail-query
      ;;    "Unread personal messages"
      ;;    ?u))
      mu4e-contexts
      `(,(make-mu4e-context
          :name "Privat"
          :match-func (lambda (msg)
                        (when msg
                          (or
                           (mu4e-message-contact-field-matches
                            msg '(:to :cc :bcc)
                            "privat@luisgerhorst.de"))))
          :vars '((user-mail-address . "privat@luisgerhorst.de")))
        ,(make-mu4e-context
          :name "Arbeit"
          :match-func (lambda (msg)
                        (when msg
                          (or
                           (mu4e-message-contact-field-matches
                            msg '(:to :cc :bcc) "work@luisgerhorst.de")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/Arbeit"))))
          :vars '((user-mail-address . "work@luisgerhorst.de")))
        ,(make-mu4e-context
          :name "FAU"
          :match-func (lambda (msg)
                        (when msg
                          (or
                           (mu4e-message-contact-field-matches
                            msg '(:to :cc :bcc) "luis.gerhorst@fau.de")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/Uni")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/Uni Lists"))))
          :vars '((user-mail-address . "luis.gerhorst@fau.de")))
        ,(make-mu4e-context
          :name "Uni"
          :vars '((user-mail-address . "uni@luisgerhorst.de")))))

;; This sets `mu4e-user-mail-address-list' to the concatenation of all
;; `user-mail-address' values for all contexts. If you have other mail
;; addresses as well, you'll need to add those manually.
(setq mu4e-user-mail-address-list
      (delq nil
            (mapcar (lambda (context)
                      (when (mu4e-context-vars context)
                        (cdr (assq 'user-mail-address
                                   (mu4e-context-vars context)))))
                    mu4e-contexts)))

(provide 'luis-mail-fastmail)
