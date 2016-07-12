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

;; Auto Highligh Symbol
(use-package auto-highlight-symbol
  :ensure t
  :demand
  :diminish auto-highlight-symbol-mode
  :bind (("M-n" . ahs-highlight-now-and-forward)
         ("M-p" . ahs-highlight-now-and-backward))
  :config
  (global-auto-highlight-symbol-mode 1)
  (define-key auto-highlight-symbol-mode-map (kbd "M--") nil)
  (customize-set-variable 'ahs-default-range 'ahs-range-whole-buffer)
  ;; 'Disable' automatic highlighting
  (ahs-set-idle-interval most-positive-fixnum)
  (defun ahs-highlight-now-and-forward ()
    (interactive)
    (ahs-highlight-now)
    (ahs-forward))
  (defun ahs-highlight-now-and-backward ()
    (interactive)
    (ahs-highlight-now)
    (ahs-backward)))

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

(provide 'luis-movement)
