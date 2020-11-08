;; -*- lexical-binding: t; -*-

;;; Sending

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;; user-mail-address / user-full-name set using EMAIL / NAME in .zprofile
(setq smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user (get-string-from-file "~/.authinfo.d/emacs_smtpmail-smtp-user")
      smtpmail-local-domain "fastmail.com")

(setq mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder "/Sent Items"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive")

(setq luis-mu4e-interesting-mail-query
      (concat (get-string-from-file "~/.authinfo.d/emacs_luis-mu4e-interesting-mail-query")
              " AND not maildir:\"/Junk Mail\""
              " AND not maildir:" mu4e-drafts-folder ""
              " AND not maildir:\"" mu4e-sent-folder "\""
              " AND not maildir:" mu4e-trash-folder ""
              " AND not maildir:" mu4e-refile-folder ""))
(setq luis-mu4e-interesting-unread-mail-query
      (concat "flag:unread AND (" luis-mu4e-interesting-mail-query ")"))

(setq mu4e-bookmarks
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
                            msg '(:to :cc :bcc) "job@luisgerhorst.de")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/Arbeit"))))
          :vars '((user-mail-address . "job@luisgerhorst.de")))
        ,(make-mu4e-context
          :name "FAU"
          :match-func (lambda (msg)
                        (when msg
                          (or
                           (mu4e-message-contact-field-matches
                            msg '(:to :cc :bcc) "luis.gerhorst@fau.de")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/FAU")
                           (string-equal
                            (mu4e-message-field msg :maildir)
                            "/FAU_Info"))))
          :vars '((user-mail-address . "luis.gerhorst@fau.de")))))

(provide 'luis-mail-fastmail)
