;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 4)
(setq-default sh-indentation 4)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Start server to allow opening files from shell
(server-start)

;; Discover
(require 'discover)
(global-discover-mode t)
