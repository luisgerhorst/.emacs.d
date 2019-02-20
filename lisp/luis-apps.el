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
  ;; Warning: When changing `Man-notify-method', keep in mind that .zshrc
  ;; assumes that M-x man makes the manpage the current buffer.
  (setq Man-notify-method 'aggressive))

(provide 'luis-apps)
