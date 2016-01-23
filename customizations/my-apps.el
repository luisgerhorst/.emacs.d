;; Standalone features / tools installation and keybindings.

;; laucher-map defined in init.el because it may be used in multiple
;; config files.

;; Git interface.
(require-package 'magit)
(define-key launcher-map (kbd "g") 'magit-status)

;; Fancier list-packages.
(require-package 'paradox)
(define-key launcher-map (kbd "p") 'paradox-list-packages)

(define-key launcher-map (kbd "c") #'calc)

;; Shell
(define-key launcher-map (kbd "s") #'ansi-term)

(require-package 'sx)
;; Ordered by frequency of use, for no particular reason.
(define-key launcher-map (kbd "q q") #'sx-tab-all-questions)
(define-key launcher-map (kbd "q i") #'sx-inbox)
(define-key launcher-map (kbd "q o") #'sx-open-link)
(define-key launcher-map (kbd "q u") #'sx-tab-unanswered-my-tags)
(define-key launcher-map (kbd "q a") #'sx-ask)
(define-key launcher-map (kbd "q s") #'sx-search)

;; Because proced does not work on OS X.
(require-package 'vkill)
(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

(defun test-emacs-config ()
  "Start shell Emacs in background to test config."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"(condition-case e (progn (load \\\"~/.emacs.d/init.el\\\") (message \\\"-OK-\\\")) (error (message \\\"ERROR!\\\") (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "Emacs config ok"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

;; Mail

;; Options required for sending mails. Additional variables have to be
;; set in private.el.

;; See
;; http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html
;; for a good tutorial.
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-debug-info t
      starttls-extra-arguments nil
      starttls-gnutls-program (executable-find "gnutls-cli")
      smtpmail-warn-about-unknown-extensions t
      starttls-use-gnutls t)

;; Reading Mail.

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-mu-binary "/usr/local/bin/mu")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; convert html emails properly
;; Possible options:
;;   - html2text -utf8 -width 72
;;   - textutil -stdin -format html -convert txt -stdout
;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;;   - w3m -dump -cols 80 -T text/html
;;   - view in browser (provided below)
(setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; fetch mail every 10 mins
(setq mu4e-update-interval 600)
