(require 'org)

(defun my/org-mode-hook ()
  ;; Proper line wrapping for text.
  (visual-line-mode 1)
  (auto-fill-mode 1))

(add-hook 'org-mode-hook 'my/org-mode-hook)

(provide 'luis-org)
