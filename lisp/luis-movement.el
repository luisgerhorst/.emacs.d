;;; Scrolling

(setq scroll-preserve-screen-position t
      scroll-conservatively 50
      scroll-margin 2
      ;; Scroll in smaller steps when mouse is used.
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; On Mac: Meta-Option-n/p
(global-set-key (kbd "M-N") #'scroll-up-line)
(global-set-key (kbd "M-P") #'scroll-down-line)

;;; Windows

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

;;; Misc

(use-package subword
  :defer t
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  :config
  (diminish 'subword-mode))

(use-package avy
  :bind (("C-:" . avy-goto-char)))

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :demand
  :init
  (setq-default save-place t)
  ;; Keep track of saved places in ~/.emacs.d/places
  (setq save-place-file (locate-user-emacs-file "places")))

(use-package paredit
  :bind (;; These also work in strings.
         ("C-M-u" . paredit-backward-up)
         ("C-M-d" . paredit-forward-down)
         ;; I need these more often then forward/backward-list.
         ("C-M-p" . paredit-backward-down)
         ("C-M-n" . paredit-forward-up)))



(provide 'luis-movement)
