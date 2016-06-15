;; Standalone features / tools installation and keybindings.

(require 'luis-keybindings)

;; Git interface.

(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (:map launcher-map
              ("g" . magit-status)))

;; Fancier list-packages.
(use-package paradox
  :ensure t
  :bind (:map launcher-map
              ("p" . paradox-list-packages)))

(define-key launcher-map (kbd "c") #'calc)

;; Because proced does not work on OS X.
(use-package vkill
  :commands (vkill list-unix-processes))

;; Mail.
(require 'luis-mail)
(define-key launcher-map (kbd "m") #'mu4e)


(provide 'luis-apps)
