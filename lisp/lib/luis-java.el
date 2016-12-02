(use-package cc-mode
  :defer
  :config
  (add-hook 'java-mode-hook #'luis-company-configure-automatic-completion))

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
             global-eclim-mode))

(progn
  (defun luis-eclimd-start ()
    "Start eclimd and disable asking for confimation to kill it
when Emacs exits. Automatically enable eclim-mode in buffers that
visit Eclipse projects."
    (require 'eclimd)
    (when (not eclimd-process)
      (start-eclimd eclimd-default-workspace)
      ;; This requires eclimd to be running, thus `eclimd-wait-for-process' must
      ;; not be nil:
      (global-eclim-mode)
      (add-hook 'kill-emacs-hook #'stop-eclimd)))

  (add-hook 'java-mode-hook #'luis-eclimd-start))


(provide 'luis-java)
