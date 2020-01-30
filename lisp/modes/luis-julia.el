(require 'luis-ide)

(use-package ess-julia
  :straight ess
  :defer t
  :config
  (add-hook 'ess-julia-mode-hook #'luis-company-configure-automatic-completion))

(provide 'luis-julia)
