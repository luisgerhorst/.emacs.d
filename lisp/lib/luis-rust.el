;; -*- lexical-binding: t -*-

(use-package rust-mode
  :defer t
  :config
  (add-hook 'rust-mode-hook #'luis-company-configure-automatic-completion))

(use-package cargo
  :defer t
  :bind (:map
         cargo-minor-mode-map
         ("C-c b" . cargo-process-build)
         ("C-c r" . cargo-process-run)))

(defun luis-add-cargo-dir-local-variables ()
  (interactive)
  (add-dir-local-variable 'rust-mode 'mode 'cargo-minor))

(use-package company-race
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer)))

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (add-hook 'rust-mode-hook #'flycheck-mode)))

(provide 'luis-rust)
