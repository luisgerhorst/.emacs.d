(require 'luis-compile)
(require 'luis-modification)

(use-package ess-julia
  :config
  (add-hook 'ess-julia-mode-hook #'luis-company-configure-automatic-completion)
  ;; (add-hook 'ess-julia-mode-hook #'luis-flycheck-unless-file-remote)
  )

;; (use-package flycheck-julia
;;   :defer t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (flycheck-julia-setup)))

(provide 'luis-julia)
