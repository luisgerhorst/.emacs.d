(setq c-basic-offset tab-width)

(defun luis/c-mode-hook ()
  (aggressive-indent-mode -1))

(add-hook 'c-mode-hook 'luis/c-mode-hook)

(require-package 'company)
(add-hook 'c-mode-hook #'company-mode)

(provide 'luis-c)