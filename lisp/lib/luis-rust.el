;; -*- lexical-binding: t -*-

(use-package rust-mode
  :defer t
  :init
  (setq rust-format-on-save t)
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

(provide 'luis-rust)
