;; Only put keybinding that don't fit into another file here.
;; e.g. for editing commands use `my-editing.el`

;; Open magit.
(global-set-key (kbd "<f5>") 'magit-status)

;; Enable upcase / downcase region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Other Window.
(global-set-key (kbd "M-o") 'other-window)
