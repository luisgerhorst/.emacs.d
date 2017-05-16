;; -*- lexical-binding: t; -*-

(setenv "PYTHONIOENCODING" "utf8")

(use-package company-jedi
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-jedi)))

(defun luis-python-mode-hook ()
  (electric-indent-just-newline nil))

(add-hook 'python-mode-hook #'flycheck-mode)
(add-hook 'python-mode-hook #'luis-python-mode-hook)
(add-hook 'python-mode-hook #'luis-company-configure-automatic-completion)

(provide 'luis-python)
