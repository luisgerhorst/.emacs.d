(require 'python)

(require-package 'company)
(require-package 'company-jedi)
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

(add-hook 'python-mode-hook 'my/python-mode-hook)
(defun my/python-mode-hook ()
  (company-mode-on)
  (my/comment-auto-fill)
  (electric-indent-just-newline nil))


(provide 'luis-python)
