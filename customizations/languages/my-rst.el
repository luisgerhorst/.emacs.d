;;; reStructuredText

(require 'rst)

(defun my/rst-mode-hook ()
  (show-paren-mode -1)
  (visual-line-mode 1)
  (auto-fill-mode 1)
  (subword-mode -1))
(add-hook 'rst-mode-hook 'my/rst-mode-hook)
