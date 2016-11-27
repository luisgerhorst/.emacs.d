;;; Scrolling

(setq scroll-preserve-screen-position t
      scroll-conservatively 50
      scroll-margin 2
      ;; Scroll in smaller steps when mouse is used.
      mouse-wheel-scroll-amount '(2 ((shift) . 1)))

(global-set-key (kbd "<prior>") #'scroll-down-line)
(global-set-key (kbd "<next>") #'scroll-up-line)

;;; Misc

(use-package subword
  :defer t
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  :config
  (diminish 'subword-mode))

(use-package avy
  :ensure t
  :bind ("C-:" . avy-goto-char))

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :demand
  :ensure nil
  :init
  (setq-default save-place t)
  ;; Keep track of saved places in ~/.emacs.d/places
  (setq save-place-file (locate-user-emacs-file "places")))

(use-package paredit
  :ensure t
  :bind (;; These also work in strings.
         ("C-M-u" . paredit-backward-up)
         ("C-M-d" . paredit-forward-down)
         ;; I need these more often then forward/backward-list.
         ("C-M-p" . paredit-backward-down)
         ("C-M-n" . paredit-forward-up)))

(use-package dumb-jump
  :ensure
  :pin "melpa"
  :bind (("H-d M-." . dumb-jump-go)
         ;; Use xref-pop-marker-stack to go back after jumping to definition
         ;; (bound to M-,).
         ))


(provide 'luis-movement)
