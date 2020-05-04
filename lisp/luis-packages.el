;; -*- lexical-binding: t; -*-

;;; straight.el

;; (use-package my-mode) installs my-mode as required.
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; package-selected-packages that may have to be installed:
;;
;; rust-auto-use
;; fzf
;; cuda-mode
;; flycheck-julia
;; async
;; git-commit
;; magit-svn
;; mips-mode
;; delight
;; esh-autosuggest
;; reveal-in-osx-finder
;; flx-ido
;; ido-completing-read+
;; ace-window
;; xcscope
;; company-jedi
;; anaconda-mode
;; frame-restore
;; meghanada
;; gradle-mode
;; dsvn
;; groovy-mode
;; flycheck-irony
;; company
;; ag
;; git-timemachine
;; whole-line-or-region
;; yaml-mode
;; projectile
;; ido-vertical-mode
;; company-irony
;; dumb-jump
;; ensime
;; iedit
;; ws-butler
;; which-key
;; vkill
;; swift-mode
;; smex
;; scss-mode
;; paredit
;; paradox
;; nasm-mode
;; mu4e-alert
;; markdown-mode
;; magit
;; lua-mode
;; fillcode
;; expand-region
;; exec-path-from-shell
;; discover-my-major
;; discover
;; diminish
;; crm-custom
;; company-quickhelp
;; company-c-headers
;; company-auctex
;; avy
;; auto-complete
;; auto-compile
;; apache-mode
;; aggressive-indent
;; adaptive-wrap


(provide 'luis-packages)
