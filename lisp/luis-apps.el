;; Standalone features / tools installation and keybindings.  -*- lexical-binding: t; -*-

;; Git interface
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-c g" . magit-status))

;; Fancier list-packages
(use-package paradox
  :bind ("C-c s p" . paradox-list-packages))

;; Mail
(require 'luis-mail)
(global-set-key (kbd "C-c s m") #'mu4e)

(global-set-key (kbd "C-c m") #'man)

(provide 'luis-apps)
