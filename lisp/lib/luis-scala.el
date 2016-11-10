(require 'luis-ensime)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook #'luis-company-configure-automatic-completion))


(provide 'luis-scala)
