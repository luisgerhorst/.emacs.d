(require 'luis-sbt)

(use-package cc-mode
  :bind (:map java-mode-map
              ("C-c c" . luis-sbt-compile)
              ("C-c r" . luis-sbt-run)
              ("C-c t" . luis-sbt-test)))

(use-package company-emacs-eclim
  :after company
  :commands company-emacs-eclim-setup)

(use-package eclimd
  :commands start-eclimd
  :config
  (setq eclimd-wait-for-process nil))

;; Run `eclim-project-create' when opening a Java file you want to edit for
;; the first time.
(use-package eclim
  :bind (:map eclim-mode-map
              ("C-M-i" . company-emacs-eclim))
  :config
  (global-eclim-mode)
  (add-hook 'java-mode-hook #'eclim-mode)
  ;; Start eclimd the first time a Java file is opened:
  (add-hook 'eclim-mode-hook
            (lambda () (start-eclimd eclimd-default-workspace)))
  (add-hook 'eclim-mode-hook #'luis-company-configure-automatic-completion))


(provide 'luis-java)
