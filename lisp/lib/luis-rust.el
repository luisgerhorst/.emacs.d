(use-package rust-mode
  :defer t
  :config
  (add-hook 'rust-mode-hook #'luis-company-configure-automatic-completion))

(use-package cargo
  :defer t
  :bind (:map
         cargo-minor-mode-map
         ("C-c c" . cargo-process-build)
         ("C-c r" . cargo-process-run)))

(defun luis-add-cargo-dir-local-variables ()
  (interactive)
  (add-dir-local-variable 'rust-mode 'mode 'cargo-minor))

(use-package company-race
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer)))

(provide 'luis-rust)