;; -*- lexical-binding: t; -*-

;;; Sending

;; user-mail-address / user-full-name set using EMAIL / NAME in .zprofile
(setq smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "mumble@fastmail.com"
      smtpmail-local-domain "fastmail.com")

;; When deleting a message in mu4e (i.e. press d in mu4e-headers-mode), the
;; message is marked as IMAP-deleted and will be removed permanently the next
;; time an expunge takes place (e.g. the next time mbsync runs), see
;; https://github.com/djcb/mu/issues/1136. In contrast, what desktop mail
;; clients usually do, is to just move the message to the Trash folder from
;; where the server auto-deletes it after some delay (by default 1 week at
;; FastMail), see
;; https://www.fastmail.com/help/clients/deleteissue.html?u=4bbc411b.
;;
;; In mu4e-headers-mode, d marks a message as IMAP-deleted AND moves it to the
;; trash folder, D only marks a message as IMAP-deleted. Thus rebind d to
;; only move the message to the Trash folder:
(setf (alist-get 'trash mu4e-marks)
      (list :char '("d" . "▼")
            :prompt "dtrash"
            :dyn-target (lambda (target msg)
                          (mu4e-get-trash-folder msg))
            :action (lambda (docid msg target)
                      ;; Here's the main difference to the regular trash mark,
                      ;; no +T before -N so the message is not marked as
                      ;; IMAP-deleted:
                      (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))

(setq luis-mu4e-interesting-mail-query
      (concat "not maildir:\"/Mailing Lists/*/*\""
              " AND not maildir:\"/Learn Junk\""
              " AND not maildir:\"/Junk Mail\""
              " AND not maildir:\"/Drafts\""
              " AND not maildir:\"/Sent Items\""
              " AND not maildir:\"/Trash\""
              " AND not maildir:\"/Archive\""))
(setq luis-mu4e-interesting-unread-mail-query
      (concat "flag:unread AND (" luis-mu4e-interesting-mail-query ")"))

(setq mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder "/Sent Items"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-bookmarks
      `((,luis-mu4e-interesting-mail-query
         "Personal messages"
         ?p)
        (,luis-mu4e-interesting-unread-mail-query
         "Unread personal messages"
         ?u))
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
