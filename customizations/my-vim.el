;; Only use Evil for text editing. Not for stuff like navigating to a file,
;; switching buffers, etc. This way Emacs stays useable for you if you're in a
;; buffer that is not used for text editing (e.g. magit, paradox or some shell).

;; Reminder: Only editing commands.
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(require 'evil)
(evil-mode 1)

;; No undo-tree anyway.
(define-key evil-normal-state-map (kbd "C-r") nil)

(add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)
