;; Standalone features / tools installation and keybindings.  -*- lexical-binding: t; -*-

;; Git interface
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-c s g" . magit-status))

;; Fancier list-packages
(use-package paradox
  :bind ("C-c s p" . paradox-list-packages))

;; Because proced does not work on OS X.
(use-package vkill
  :commands (vkill list-unix-processes))

;; Mail
(require 'luis-mail)
(global-set-key (kbd "C-c s m") #'mu4e)

;; Terminal: see lib/luis-terminal.el

(use-package dsvn
  :after vc-svn
  :bind (("C-c s v" . svn-status)))

;; TODO: function to prefix cip_ if file on CIP

(use-package man
  :bind (("C-c s m" . man)))

(provide 'luis-apps)
