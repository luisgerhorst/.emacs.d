;; -*- lexical-binding: t; -*-

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)

  (add-hook 'sbt-mode-hook #'luis-code-wrap-mode))

(defun luis-sbt-compile ()
  (interactive)
  (sbt-command "compile"))
(defun luis-sbt-run ()
  (interactive)
  (sbt-command "run"))
(defun luis-sbt-test ()
  (interactive)
  (sbt-command "test"))

(provide 'luis-sbt)
