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

;; Man
(defun prefixed-man (prefix manargs)
  "Used to quickly access manpages that were copied from a remote host and prefixed with a string to distinguish them from local ones."
  (interactive (list
                (if (and default-directory (file-remote-p default-directory))
                    "cip_"
                  "")
                ;; The first item of man's interactive form.
                (eval (nth 1 (nth 1 (interactive-form 'man))))))
  (man (concat prefix manargs)))

(use-package man
  :bind (("C-c s m" . prefixed-man)))

(provide 'luis-apps)
