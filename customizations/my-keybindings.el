;; Only put keybinding that don't fit into another file here.
;; e.g. for editing commands use `my-editing.el`

;; Git interface.
(require-package 'magit)
(global-set-key (kbd "<f5>") 'magit-status)

;; Fancier list-packages.
(require-package 'paradox)
(global-set-key (kbd "<f6>") 'paradox-list-packages)

