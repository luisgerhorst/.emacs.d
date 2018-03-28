(use-package cargo
  :defer t
  :bind (:map
         cargo-minor-mode-map
         ("C-c c" . cargo-process-build)
         ("C-c r" . cargo-process-run)))

(provide 'luis-rust)
