(require 'luis-compile)
(require 'luis-modification)

(use-package ess-julia
  :straight ess
  :defer t
  :config
  (add-hook 'ess-julia-mode-hook #'luis-company-configure-automatic-completion))

(provide 'luis-julia)
