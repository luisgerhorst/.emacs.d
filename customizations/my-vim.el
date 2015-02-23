(require 'evil)
(evil-mode 1)

;; No undo-tree anyway.
(define-key evil-normal-state-map (kbd "C-r") nil)

(add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)
