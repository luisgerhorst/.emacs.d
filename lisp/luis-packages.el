;; -*- lexical-binding: t; -*-

;;; Site-Lisp

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

(require 'cl)
(defun luis-add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; Beware that packages installed this way are overwritten by packages installed
;; from elpa package archives.
(luis-add-subdirs-to-load-path (locate-user-emacs-file "site-lisp/"))

;;; straight.el

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

(straight-use-package 'use-package)

;; package-selected-packages that may have to be installed:
;;
;; lsp-ui
;; company-lsp
;; cargo
;; rust-auto-use
;; fzf
;; cuda-mode
;; flycheck-julia
;; async
;; git-commit
;; magit-svn
;; mips-mode
;; ess
;; rust-mode
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
;; fic-mode
;; yaml-mode
;; projectile
;; ido-vertical-mode
;; flycheck
;; company-irony
;; irony
;; use-package
;; dumb-jump
;; ensime
;; iedit
;; auctex
;; ws-butler
;; which-key
;; vkill
;; visual-fill-column
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
