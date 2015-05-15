;; Standalone features / tools installation and keybindings.

;; Git interface.
(require-package 'magit)
(global-set-key (kbd "H-g") 'magit-status)

;; Fancier list-packages.
(require-package 'paradox)
(global-set-key (kbd "H-x") 'paradox-list-packages)

(global-set-key (kbd "H-c") #'calc)

;; Shell
(global-set-key (kbd "H-s") #'ansi-term)

(require-package 'sx)
;; Ordered by frequency of use, for no particular reason.
(global-set-key (kbd "H-q q") #'sx-tab-all-questions)
(global-set-key (kbd "H-q i") #'sx-inbox)
(global-set-key (kbd "H-q o") #'sx-open-link)
(global-set-key (kbd "H-q u") #'sx-tab-unanswered-my-tags)
(global-set-key (kbd "H-q a") #'sx-ask)
(global-set-key (kbd "H-q s") #'sx-search)

;; Because proced does not work on OS X.
(require-package 'vkill)
(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)
