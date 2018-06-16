(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote aggressive))
 '(Man-switches "-a")
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auto-coding-alist
   (quote
    (("\\.\\(arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\|rar\\|7z\\|ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\|RAR\\|7Z\\)\\'" . no-conversion-multibyte)
     ("\\.\\(exe\\|EXE\\)\\'" . no-conversion)
     ("\\.\\(sx[dmicw]\\|odt\\|tar\\|t[bg]z\\)\\'" . no-conversion)
     ("\\.\\(gz\\|Z\\|bz\\|bz2\\|xz\\|gpg\\)\\'" . no-conversion)
     ("\\.\\(jpe?g\\|png\\|gif\\|tiff?\\|p[bpgn]m\\)\\'" . no-conversion)
     ("\\.pdf\\'" . no-conversion)
     ("/#[^/]+#\\'" . utf-8-emacs-unix)
     ("\\.tex\\'" . utf-8))))
 '(bibtex-BibTeX-entry-alist
   (quote
    (("Article" "Article in Journal"
      (("author")
       ("title" "Title of the article (BibTeX converts it to lowercase)"))
      (("journal")
       ("year"))
      (("volume" "Volume of the journal")
       ("number" "Number of the journal (only allowed if entry contains volume)")
       ("pages" "Pages in the journal")
       ("month")
       ("note")))
     ("InProceedings" "Article in Conference Proceedings"
      (("author")
       ("title" "Title of the article in proceedings (BibTeX converts it to lowercase)"))
      (("booktitle" "Name of the conference proceedings")
       ("year"))
      (("editor")
       ("volume" "Volume of the conference proceedings in the series")
       ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
       ("series" "Series in which the conference proceedings appeared")
       ("pages" "Pages in the conference proceedings")
       ("month")
       ("address")
       ("organization" "Sponsoring organization of the conference")
       ("publisher" "Publishing company, its location")
       ("note")))
     ("InCollection" "Article in a Collection"
      (("author")
       ("title" "Title of the article in book (BibTeX converts it to lowercase)")
       ("booktitle" "Name of the book"))
      (("publisher")
       ("year"))
      (("editor")
       ("volume" "Volume of the book in the series")
       ("number" "Number of the book in a small series (overwritten by volume)")
       ("series" "Series in which the book appeared")
       ("type" "Word to use instead of \"chapter\"")
       ("chapter" "Chapter in the book")
       ("pages" "Pages in the book")
       ("edition" "Edition of the book as a capitalized English word")
       ("month")
       ("address")
       ("note")))
     ("InBook" "Chapter or Pages in a Book"
      (("author" nil nil 0)
       ("editor" nil nil 0)
       ("title" "Title of the book")
       ("chapter" "Chapter in the book"))
      (("publisher")
       ("year"))
      (("volume" "Volume of the book in the series")
       ("number" "Number of the book in a small series (overwritten by volume)")
       ("series" "Series in which the book appeared")
       ("type" "Word to use instead of \"chapter\"")
       ("address")
       ("edition" "Edition of the book as a capitalized English word")
       ("month")
       ("pages" "Pages in the book")
       ("note")))
     ("Proceedings" "Conference Proceedings"
      (("title" "Title of the conference proceedings")
       ("year"))
      nil
      (("booktitle" "Title of the proceedings for cross references")
       ("editor")
       ("volume" "Volume of the conference proceedings in the series")
       ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
       ("series" "Series in which the conference proceedings appeared")
       ("address")
       ("month")
       ("organization" "Sponsoring organization of the conference")
       ("publisher" "Publishing company, its location")
       ("note")))
     ("Book" "Book"
      (("author" nil nil 0)
       ("editor" nil nil 0)
       ("title" "Title of the book"))
      (("publisher")
       ("year"))
      (("volume" "Volume of the book in the series")
       ("number" "Number of the book in a small series (overwritten by volume)")
       ("series" "Series in which the book appeared")
       ("address")
       ("edition" "Edition of the book as a capitalized English word")
       ("month")
       ("note")))
     ("Booklet" "Booklet (Bound, but no Publisher)"
      (("title" "Title of the booklet (BibTeX converts it to lowercase)"))
      nil
      (("author")
       ("howpublished" "The way in which the booklet was published")
       ("address")
       ("month")
       ("year")
       ("note")))
     ("PhdThesis" "PhD. Thesis"
      (("author")
       ("title" "Title of the PhD. thesis")
       ("school" "School where the PhD. thesis was written")
       ("year"))
      nil
      (("type" "Type of the PhD. thesis")
       ("address" "Address of the school (if not part of field \"school\") or country")
       ("month")
       ("note")))
     ("MastersThesis" "Master's Thesis"
      (("author")
       ("title" "Title of the master's thesis (BibTeX converts it to lowercase)")
       ("school" "School where the master's thesis was written")
       ("year"))
      nil
      (("type" "Type of the master's thesis (if other than \"Master's thesis\")")
       ("address" "Address of the school (if not part of field \"school\") or country")
       ("month")
       ("note")))
     ("TechReport" "Technical Report"
      (("author")
       ("title" "Title of the technical report (BibTeX converts it to lowercase)")
       ("institution" "Sponsoring institution of the report")
       ("year"))
      nil
      (("type" "Type of the report (if other than \"technical report\")")
       ("number" "Number of the technical report")
       ("address")
       ("month")
       ("note")))
     ("Manual" "Technical Manual"
      (("title" "Title of the manual"))
      nil
      (("author")
       ("organization" "Publishing organization of the manual")
       ("address")
       ("edition" "Edition of the manual as a capitalized English word")
       ("month")
       ("year")
       ("note")))
     ("Unpublished" "Unpublished"
      (("author")
       ("title" "Title of the unpublished work (BibTeX converts it to lowercase)")
       ("note"))
      nil
      (("month")
       ("year")))
     ("Misc" "Miscellaneous" nil nil
      (("author")
       ("title" "Title of the work (BibTeX converts it to lowercase)")
       ("howpublished" "The way in which the work was published")
       ("month")
       ("year")
       ("note")))
     ("Online" "Biblatex Online"
      (("title")
       ("url"))
      nil
      (("urldate")
       ("date")
       ("author" nil nil))))))
 '(coffee-tab-width 2)
 '(comment-multi-line t)
 '(company-etags-use-main-table-list nil)
 '(compilation-message-face (quote default))
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
 '(fci-rule-color "#073642")
 '(flycheck-display-errors-delay 60)
 '(flycheck-mode-line (quote (:eval "FlyC")))
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
 '(ispell-dictionary "de_DE")
 '(jedi:environment-root "3.5.3")
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
 '(package-selected-packages
   (quote
    (racer company-racer cargo rust-mode delight esh-autosuggest reveal-in-osx-finder flx-ido ido-completing-read+ ace-window xcscope ssh-agency company-jedi anaconda-mode frame-restore meghanada osx-trash gradle-mode dsvn groovy-mode company-emacs-eclim eclim flycheck-irony company ag git-timemachine whole-line-or-region fic-mode yaml-mode projectile ido-vertical-mode flycheck company-irony irony use-package dumb-jump ensime iedit powerline auctex ws-butler which-key vkill visual-fill-column swift-mode solarized-theme smex scss-mode paredit paradox nasm-mode mu4e-alert markdown-mode magit lua-mode fillcode expand-region exec-path-from-shell discover-my-major discover diminish crm-custom company-quickhelp company-c-headers company-auctex avy auto-complete auto-compile apache-mode aggressive-indent adaptive-wrap)))
 '(paradox-automatically-star t)
 '(paradox-execute-asynchronously t)
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
    ((flycheck-checker . luis-c-gcc-make)
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
 '(warning-minimum-level :error)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Input Mono"))))
 '(completions-common-part ((t (:inherit company-preview-common))))
 '(helm-ff-dotted-directory ((t (:foreground "#586e75"))))
 '(helm-ff-dotted-symlink-directory ((t (:foreground "#586e75"))))
 '(helm-selection ((t (:background "#839496" :foreground "#002b36" :underline nil))))
 '(helm-source-header ((t (:background "#268bd2" :foreground "#002b36" :underline nil))))
 '(iedit-occurrence ((t (:inherit lazy-highlight))))
 '(leerzeichen ((t (:foreground "#475B62")))))
