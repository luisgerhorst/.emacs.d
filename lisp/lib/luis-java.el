(use-package cc-mode
  :defer t
  :config
  (add-hook 'java-mode-hook #'luis-company-configure-automatic-completion))

;;; Eclim

(defun luis-force-company-emacs-eclim ()
  "Stop active backend before starting `company-emacs-eclim'"
  (interactive)
  (call-interactively #'company-abort)
  ;; Autoloaded:
  (call-interactively #'company-emacs-eclim))

(use-package eclim
  :defer t
  :bind (:map
         eclim-mode-map
         ("C-M-i" . luis-force-company-emacs-eclim))
  :init
  (add-hook 'java-mode-hook #'eclim-mode)
  :config
  (setq eclimd-autostart t
        eclimd-autostart-with-default-workspace t))

(use-package gradle-mode
  :diminish gradle-mode
  :bind-keymap ("C-c C-g" . gradle-mode-map)
  :config
  (gradle-mode 1))

(provide 'luis-java)
