;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :defer t
  :config
  (add-hook 'java-mode-hook
            #'luis-company-configure-automatic-completion))

(use-package meghanada
  :defer t
  :init
  (with-eval-after-load 'cc-mode
    ;; Not sure if this is bad if the project does not use gradle/maven. If it
    ;; causes problems remove it / make it conditional.
    (add-hook 'java-mode-hook #'meghanada-mode))
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'meghanada-mode-hook #'flycheck-mode)))

(defun luis-gradle-run ()
  (interactive)
  (gradle-execute "run"))

;;; Only works if the gradle binary is in the PATH.
(use-package gradle-mode
  :diminish gradle-mode
  :bind (:map
         gradle-mode-map
         ("C-c C-g e" . luis-gradle-run))
  :bind-keymap ("C-c C-g" . gradle-mode-map)
  :config
  (gradle-mode 1))

(provide 'luis-java)
