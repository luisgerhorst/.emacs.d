;; Standalone features / tools installation and keybindings.

(require 'luis-keybindings)

;; Git interface.
(require-package 'magit)
(define-key launcher-map (kbd "g") #'magit-status)

;; Fancier list-packages.
(require-package 'paradox)
(define-key launcher-map (kbd "p") #'paradox-list-packages)

(define-key launcher-map (kbd "c") #'calc)

;; Because proced does not work on OS X.
(require-package 'vkill)
(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

;; Mail.
(require 'luis-mail)
(define-key launcher-map (kbd "m") #'mu4e)


(provide 'luis-apps)