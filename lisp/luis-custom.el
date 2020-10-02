(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-switches "-a")
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(calendar-week-start-day 1)
 '(coffee-tab-width 2)
 '(comment-column 0)
 '(comment-multi-line t)
 '(company-etags-use-main-table-list nil)
 '(compilation-always-kill t)
 '(compilation-scroll-output t)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-in-non-selected-windows t)
 '(custom-safe-themes
   (quote
    ("2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(directory-abbrev-alist
   (quote
    (("^/cip" . "/ssh:un65esoq@faui00l.informatik.uni-erlangen.de:"))))
 '(dired-listing-switches "-alh")
 '(edts-man-root "/Users/luis/.emacs.d/edts/doc/17.5")
 '(enable-remote-dir-locals t)
 '(eshell-cp-interactive-query t)
 '(eshell-hist-ignoredups t)
 '(eshell-mv-interactive-query t)
 '(eshell-rm-removes-directories t)
 '(ess-smart-S-assign-key nil)
 '(fci-rule-color "#073642")
 '(file-coding-system-alist
   (quote
    (("\\.\\(scala\\|sbt\\)\\'" utf-8 . utf-8)
     ("\\.tzst\\'" no-conversion . no-conversion)
     ("\\.zst\\'" no-conversion . no-conversion)
     ("\\.dz\\'" no-conversion . no-conversion)
     ("\\.txz\\'" no-conversion . no-conversion)
     ("\\.xz\\'" no-conversion . no-conversion)
     ("\\.lzma\\'" no-conversion . no-conversion)
     ("\\.lz\\'" no-conversion . no-conversion)
     ("\\.g?z\\'" no-conversion . no-conversion)
     ("\\.\\(?:tgz\\|svgz\\|sifz\\)\\'" no-conversion . no-conversion)
     ("\\.tbz2?\\'" no-conversion . no-conversion)
     ("\\.bz2\\'" no-conversion . no-conversion)
     ("\\.Z\\'" no-conversion . no-conversion)
     ("\\.elc\\'" . utf-8-emacs)
     ("\\.el\\'" . prefer-utf-8)
     ("\\.utf\\(-8\\)?\\'" . utf-8)
     ("\\.xml\\'" . xml-find-file-coding-system)
     ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
     ("\\.tar\\'" no-conversion . no-conversion)
     ("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
     ("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system)
     ("" . utf-8-unix))))
 '(git-commit-fill-column 72)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(ido-cr+-function-blacklist
   (quote
    (read-file-name-internal read-buffer todo-add-category gnus-emacs-completing-read gnus-iswitchb-completing-read grep-read-files magit-builtin-completing-read ess-completing-read Info-read-node-name tmm-prompt man woman)))
 '(ido-ubiquitous-mode t)
 '(ido-vertical-define-keys nil)
 '(initial-buffer-choice "~/")
 '(ispell-program-name "aspell")
 '(jedi:environment-root "3.5.3")
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-sideline-enable nil)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-diff-use-overlays nil)
 '(magit-push-always-verify nil)
 '(magit-use-overlays nil)
 '(neo-hidden-regexp-list (quote ("\\.pyc$" "~$" "^#.*#$" "\\.elc$")))
 '(neo-theme (quote nerd))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(ns-alternate-modifier (quote meta))
 '(ns-antialias-text t)
 '(ns-auto-hide-menu-bar nil)
 '(ns-command-modifier (quote super))
 '(ns-control-modifier (quote control))
 '(ns-function-modifier (quote none))
 '(ns-pop-up-frames nil)
 '(org-agenda-files (quote ("~/Dropbox/org/Privat.org")))
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-span (quote day))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-todo-ignore-deadlines nil)
 '(org-agenda-todo-ignore-scheduled t)
 '(org-agenda-todo-list-sublevels nil)
 '(org-capture-templates
   (quote
    (("f" "FAU/Inbox" entry
      (file+headline "FAU.org" "Inbox")
      "** TODO ")
     ("p" "Privat/Inbox" entry
      (file+headline "Privat.org" "Inbox")
      "** TODO "))))
 '(org-default-notes-file "~/Dropbox/org/Privat.org")
 '(org-directory "~/Dropbox/org")
 '(org-priority-default 68)
 '(org-treat-insert-todo-heading-as-state-change t)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(projectile-completion-system (quote ido))
 '(projectile-global-mode t)
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (file-remote-p default-directory)
         " Projectile"
       (format " Project[%s]"
               (projectile-project-name))))))
 '(python-environment-directory "~/.pyenv/versions")
 '(python-shell-interpreter "/usr/local/bin/python3")
 '(safe-local-variable-values
   (quote
    ((flycheck-disabled-checkers emacs-lisp-checkdoc)
     (eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (unless
               (derived-mode-p
                (quote emacs-lisp-mode))
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (eval progn
           (make-local-variable
            (quote process-environment))
           (setq process-environment
                 (copy-sequence process-environment))
           (setenv "ARCH" "arm"))
     (flycheck-checker . luis-c-gcc-make)
     (eval progn
           (make-local-variable
            (quote process-environment))
           (setq process-environment
                 (copy-sequence process-environment))
           (setenv "ARCH" "arm")
           (setenv "CROSS_COMPILE" "/usr/bin/arm-linux-gnueabi-"))
     (flycheck-checker . luis-c/c++-gcc-make)
     (flycheck-checker . luis-linux-kmod)
     (flycheck-checker . luis-linux)
     (flycheck-checker
      (quote luis-linux))
     (flycheck-checker luis-linux)
     (flycheck-checker luis-flycheck-linux))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#839496" 0.2))
 '(solarized-high-contrast-mode-line t)
 '(solarized-use-more-italic t)
 '(split-height-threshold 160)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(warning-minimum-level :error))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit company-preview-common))))
 '(flymake-warning ((t nil)))
 '(iedit-occurrence ((t (:inherit lazy-highlight))))
 '(leerzeichen ((t (:foreground "#475B62"))))
 '(lsp-ui-sideline-global ((t (:background "black" :foreground "brightgreen")))))
