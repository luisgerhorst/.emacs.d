(require-package 'company)
(require-package 'company-jedi)

(require 'python)
(require 'company-jedi)

(add-hook 'python-mode-hook 'my/python-mode-hook)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  (company-mode-on)
  (my/comment-auto-fill)
  (electric-indent-just-newline))
