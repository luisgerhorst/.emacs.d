(require 'luis-sbt)

(use-package jdee
  :ensure t
  :mode ("\\.java\\'" . jdee-mode)
  :commands (jdee-mode)
  :bind (:map jdee-mode-map
              ("C-c i" . jdee-complete)
              ("C-c c" . luis-sbt-compile)
              ("C-c r" . luis-sbt-run)
              ("C-c t" . luis-sbt-test))
  :config
  (add-hook 'jdee-mode-hook
            #'luis-company-configure-automatic-completion))

(provide 'luis-java)
