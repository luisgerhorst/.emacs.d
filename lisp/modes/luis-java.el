;; -*- lexical-binding: t; -*-

(require 'luis-ide)

(use-package cc-mode
  :defer t
  :config
  (add-hook 'java-mode-hook
            #'luis-company-configure-automatic-completion))

(use-package lsp-java
  :defer t
  ;; As of 2020-02-02 meghanada works better for Gradle projects (lsp-java did
  ;; not highlight syntax errors in some files). However, try lsp-java again at
  ;; a later point or when the project is not supported by meghanada.
  ;;
  ;; :init
  ;; (add-hook 'java-mode-hook #'lsp)
  )

(use-package meghanada
  :defer t
  ;; Not sure if this is bad if the project does not use gradle/maven. If it
  ;; causes problems remove it / make it conditional.
  :init
  (add-hook 'java-mode-hook #'meghanada-mode)
  :config
  (add-hook 'meghanada-mode-hook #'flycheck-mode))

(provide 'luis-java)
