;; Standalone features / tools installation and keybindings.

;; Git interface
(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("H-s g" . magit-status))

;; Fancier list-packages
(use-package paradox
  :ensure t
  :bind ("H-s p" . paradox-list-packages))

;; Because proced does not work on OS X.
(use-package vkill
  :ensure t
  :commands (vkill list-unix-processes))

;; Mail
(require 'luis-mail)
(global-set-key (kbd "H-s m") #'mu4e)


(provide 'luis-apps)
