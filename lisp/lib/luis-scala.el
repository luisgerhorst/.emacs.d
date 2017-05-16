(require 'luis-sbt)

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook #'luis-company-configure-automatic-completion))

(use-package ensime
  ;; Autoloads are created by elpa.
  :defer t
  :after scala-mode
  :config
  ;; Using ensime requires a .ensime file in your project's root, that's why you
  ;; have to start the server manually using M-x ensime. Otherwise ensime-mode
  ;; is pretty useless.
  (add-hook 'scala-mode-hook #'ensime-mode))

(provide 'luis-scala)
