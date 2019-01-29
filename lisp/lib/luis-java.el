;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :defer t
  :config
  (add-hook 'java-mode-hook
            #'luis-company-configure-automatic-completion))

(use-package meghanada
  :defer t
  :config
  (add-hook 'meghanda-mode-hook #'flycheck-mode))

(defun luis-gradle-run ()
  (interactive)
  (gradle-execute "run"))

(use-package gradle-mode
  :diminish gradle-mode
  :bind (:map
         gradle-mode-map
         ("C-c C-g e" . luis-gradle-run))
  :bind-keymap ("C-c C-g" . gradle-mode-map)
  :config
  (gradle-mode 1))

(provide 'luis-java)
