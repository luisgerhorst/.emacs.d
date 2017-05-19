(use-package cc-mode
  :defer t
  :config
  (add-hook 'java-mode-hook #'luis-company-configure-automatic-completion))

;;; Eclim

(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook #'meghanada-mode)
  (add-hook 'java-mode-hook #'flycheck-mode))

(use-package gradle-mode
  :diminish gradle-mode
  :bind-keymap ("C-c C-g" . gradle-mode-map)
  :config
  (gradle-mode 1))

(provide 'luis-java)
