(use-package company-jedi
  :ensure t
  :commands (company-jedi)
  :init
  (add-to-list 'company-backends #'company-jedi))

(defun luis-python-mode-hook ()
  (electric-indent-just-newline nil))

(add-hook 'python-mode-hook #'luis-python-mode-hook)
(add-hook 'python-mode-hook #'luis-comment-auto-fill)
(add-hook 'python-mode-hook #'company-mode-on)

(provide 'luis-python)
