;; Standalone features / tools installation and keybindings.

(define-prefix-command 'launcher-map)
(global-set-key (kbd "<f5>") 'launcher-map)

;; Git interface.
(require-package 'magit)
(define-key launcher-map "g" 'magit-status)

;; Fancier list-packages.
(require-package 'paradox)
(define-key launcher-map "p" 'paradox-list-packages)

(define-key launcher-map "c" #'calc)

(require-package 'sx)
;; Ordered by frequency of use, for no particular reason.
(define-key launcher-map "qq" #'sx-tab-all-questions)
(define-key launcher-map "qi" #'sx-inbox)
(define-key launcher-map "qo" #'sx-open-link)
(define-key launcher-map "qu" #'sx-tab-unanswered-my-tags)
(define-key launcher-map "qa" #'sx-ask)
(define-key launcher-map "qs" #'sx-search)
