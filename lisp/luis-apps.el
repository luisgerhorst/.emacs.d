;; Standalone features / tools installation and keybindings.  -*- lexical-binding: t; -*-

;; Git interface
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-c g" . magit-status))

(use-package man
  :bind ("C-c m" . man)
  :config
  (setq Man-notify-method 'aggressive))

(when (string-prefix-p "luis-" (system-name))
  (require 'luis-mail)
  (global-set-key (kbd "C-c u") #'mu4e))

(provide 'luis-apps)
