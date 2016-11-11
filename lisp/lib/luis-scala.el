(require 'luis-sbt)

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook #'luis-company-configure-automatic-completion))


(provide 'luis-scala)
