(setq user-full-name "Luis Gerhorst"
      user-mail-address "privat@luisgerhorst.de"
      smtpmail-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 587)

(setq mu4e-maildir "~/.maildir/fastmail")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent Items")
(setq mu4e-trash-folder "/Trash")
(setq mu4e-refile-folder "/Archive")

(setq mu4e-maildir-shortcuts
      '(("/Inbox" . ?i)
        ("/Arbeit" . ?w)
        ("/Uni" . ?u))
      mu4e-bookmarks
      '(("flag:unread AND ( maildir:\"/Inbox\" OR maildir:\"/Arbeit\" OR maildir:\"/Uni\" )"
         "Unread personal messages"
         ?u))
      mu4e-alert-interesting-mail-query
      "flag:unread AND ( maildir:\"/Inbox\" OR maildir:\"/Arbeit\" OR maildir:\"/Uni\" )")

(setq mu4e-contexts
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
          :name "Uni"
          :match-func (lambda (msg)
                        (when msg
                          (or
                           (mu4e-message-contact-field-matches
                            msg '(:to :cc :bcc) "uni@luisgerhorst.de")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/Uni")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/Mailing Lists.Uni Lists")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/Mailing Lists.Uni Markt"))))
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
