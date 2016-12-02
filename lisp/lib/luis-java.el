;; meghanada completion is very fast but does not work always, it is enabled
;; automatically when you are typing (meghanada sets `company-backends'
;; locally). To invoke the slow but very powerfull eclim completion use
;; `company-emacs-eclim'.

;;; Meghanada

(use-package meghanada
  :commands (meghanada-mode)
  :init
  (add-hook 'java-mode-hook #'meghanada-mode)
  :config
  (add-hook 'meghanada-mode-hook #'flycheck-mode))

;;; Eclim

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
  ;; Start eclimd the first time eclim-mode is enabled:
  (add-hook 'eclim-mode-hook
            (lambda () (start-eclimd eclimd-default-workspace)))
  (add-hook 'eclim-mode-hook #'luis-company-configure-automatic-completion))


(provide 'luis-java)
