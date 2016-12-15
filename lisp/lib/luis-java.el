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

(use-package eclimd
  :commands (start-eclimd))

;; Run `eclim-project-create' when you edit a Java file for the first time to
;; create a Eclipse project. Otherwise many eclim features will not be available
;; (e.g. completion).
(use-package eclim
  :bind (:map eclim-mode-map
              ("C-M-i" . luis-force-company-emacs-eclim))
  :commands (eclim-mode
             global-eclim-mode)
  :init
  (add-hook 'java-mode-hook #'eclim-mode))

(progn
  ;; Displays eclim problems under point.
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.9)
  (help-at-pt-set-timer))


(provide 'luis-java)
