;; -*- lexical-binding: t; -*-

(setenv "PYTHONIOENCODING" "utf8")

(use-package company-jedi
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-jedi)))

(use-package anaconda-mode
  :defer t
  :bind (:map
         anaconda-mode-map
         ;; Use company completion powered by jedi since they look better.
         ("C-M-i" . nil)
         ("M-," . anaconda-mode-go-back)
         ;; As far as I can see this behaves like find-definition for functions
         ;; and since find-definition does not seem to work on variables anyway
         ;; we can also just always use this.
         ("M-." . anaconda-mode-find-assignments))
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  ;; The eldoc function by anaconda mode almost never works, thus we leave it
  ;; disabled.
  )

(defun luis-python-mode-hook ()
  (electric-indent-just-newline nil))

(add-hook 'python-mode-hook #'flycheck-mode)
(add-hook 'python-mode-hook #'luis-python-mode-hook)
(add-hook 'python-mode-hook #'luis-company-configure-automatic-completion)


(provide 'luis-python)
