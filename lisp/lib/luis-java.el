(use-package cc-mode
  :defer
  :config
  (add-hook 'java-mode-hook #'luis-company-configure-automatic-completion))

;;; Eclim

(use-package company-emacs-eclim
  :after company
  :commands (company-emacs-eclim
             company-emacs-eclim-setup
             luis-force-company-emacs-eclim)
  :config
  (defun luis-force-company-emacs-eclim ()
    "Stop active backend before starting `company-emacs-eclim'"
    (interactive)
    (call-interactively #'company-abort)
    (call-interactively #'company-emacs-eclim)))

(use-package eclim
  :bind (:map eclim-mode-map
              ("C-M-i" . luis-force-company-emacs-eclim))
  :commands (eclim-mode
             global-eclim-mode)
  :init
  (add-hook 'java-mode-hook #'eclim-mode)
  :config
  (setq eclimd-autostart t
        eclimd-autostart-with-default-workspace t))

(progn
  ;; Displays eclim problems under point.
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.9)
  (help-at-pt-set-timer))


(provide 'luis-java)
