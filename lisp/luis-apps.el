;; Standalone features / tools installation and keybindings.  -*- lexical-binding: t; -*-

;; Git interface
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-c g" . magit-status))

;; Fancier list-packages
(use-package paradox
  :bind ("C-c s p" . paradox-list-packages))

(use-package man
  :bind ("C-c m" . man)
  :config
  (setq Man-notify-method 'aggressive))

(when (eq system-type 'darwin)
  (require 'luis-mail)
  (global-set-key (kbd "C-c s m") #'mu4e))

(provide 'luis-apps)
