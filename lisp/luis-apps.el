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
(defcustom luis-man-prefixed-completions
  '("" "cip_")
  "Known prefixes `luis-man-prefixed' offers as completions when called.")

(defun luis-man-prefixed (prefix manargs)
  "Used to quickly access manpages that were copied from a remote host and prefixed with a string to distinguish them from local ones."
  (interactive (list (ido-completing-read "Prefix: " luis-man-prefixed-completions)
                     (eval (nth 1 (nth 1 (interactive-form 'man))))))
  (let ((manargs-list (split-string manargs)))
    (man (mapconcat #'identity
                    (append (butlast manargs-list)
                            (list (concat prefix (car (last manargs-list)))))
                    " "))))

(use-package man
  :bind (("C-c s m" . luis-man-prefixed)))

(provide 'luis-apps)
