(require-package 'company)
(require-package 'company-jedi)

(require 'python)
(require 'company-jedi)

(add-hook 'python-mode-hook 'my/python-mode-hook)
(defun my/python-mode-hook ()  
  (add-to-list 'company-backends 'company-jedi)
  (company-mode-on))

(setq python-shell-interpreter "/usr/local/bin/python3"
      python-shell-interpreter-args "-i")
