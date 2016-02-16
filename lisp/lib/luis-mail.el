;;;; Mail

;;; Sending Mail.

;; See
;; http://justinsboringpage.blogspot.de/2013/02/configuring-emacs-to-send-icloud-mail.html
;; for a good tutorial.

(setq send-mail-function 'smtpmail-send-it)

;;; Reading Mail.

(require 'mu4e)

;; Replace compose-mail with mu4e's version.
(global-set-key [remap compose-mail] #'mu4e-compose-new)

;; Start mu4e in background when opening Emacs (to receive notifications
;; about new mail).
(add-hook 'after-init-hook (lambda () (mu4e 1)))

(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval (* 5 60))
(setq mu4e-hide-index-messages t)

;; First one is the default fallback context.
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'pick-first)

(setq mu4e-show-images t)
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

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(require-package 'visual-fill-column)
(require 'visual-fill-column)

(defun my/mu4e-view-mode-hook ()
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(defun my/mu4e-compose-mode-hook ()
  "My settings for message composition."
  (visual-line-mode 1)
  (auto-fill-mode -1)
  (visual-fill-column-mode 1))

(add-hook 'mu4e-compose-mode-hook 'my/mu4e-compose-mode-hook)
(add-hook 'mu4e-view-mode-hook 'my/mu4e-view-mode-hook)

;;; Load account specific configuration.

(require 'luis-mail-private)

;;; Get notified when new mails arrive.

(require-package 'mu4e-alert)
(mu4e-alert-set-default-style 'notifier)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)


(provide 'luis-mail)
