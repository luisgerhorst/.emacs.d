;;; reStructuredText

(require 'rst)

(define-key rst-mode-map (kbd "C-c f") 'refill-mode)

(defun my/rst-mode-hook ()
  (show-paren-mode -1))
(add-hook 'rst-mode-hook 'my/rst-mode-hook)
