;; -*- lexical-binding: t; -*-

(use-package tex-site ;; AUCTeX overrides LaTeX
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-newline-function #'newline-and-indent)
  (add-hook 'LaTeX-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'LaTeX-mode-hook #'flycheck-mode)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode))

(use-package bibclean-format
  :defer t)

(use-package company-auctex
  :commands (company-auctex-init)
  :init
  (add-hook 'LaTeX-mode-hook #'company-auctex-init))

(use-package langtool
  :config
  (setq langtool-language-tool-jar (locate-user-emacs-file "lib/LanguageTool-5.0/languagetool-commandline.jar")))

(provide 'luis-latex)
