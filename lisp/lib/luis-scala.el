(use-package ensime
  :ensure t)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook #'luis-company-configure-automatic-completion))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(provide 'luis-scala)
