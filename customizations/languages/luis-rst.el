;;; reStructuredText

(require 'rst)

(add-hook 'rst-mode-hook 'my/rst-mode-hook)
(defun my/rst-mode-hook ()
  ;; Disable modes usefull for programming.
  (show-paren-mode -1)
  (subword-mode -1)

  ;; Proper line wrapping for text.
  (visual-line-mode 1)
  (auto-fill-mode 1))
