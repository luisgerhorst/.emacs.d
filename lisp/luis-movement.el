;;; Scrolling

(setq scroll-preserve-screen-position t
      scroll-conservatively 50
      scroll-margin 2
      ;; Scroll in smaller steps when mouse is used.
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; On Mac: Meta-Option-n/p
(global-set-key (kbd "M-N") #'scroll-up-line)
(global-set-key (kbd "M-P") #'scroll-down-line)


;;; Jump to definition

(defun luis-dumb-jump-save-and-go ()
  (interactive)
  (save-buffer)
  (dumb-jump-go))

;; On Mac: Meta-Option-.
(global-set-key (kbd "M-'") #'luis-dumb-jump-save-and-go)


;;; Windows

(use-package winum
  :bind (("M-0" . winum-select-window-0-or-10)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9))
  :demand
  :init
  (winum-mode))

(use-package winner
  :demand
  :bind (("M-_" . winner-undo)
         ("M-Ö" . winner-redo))
  :config
  (winner-mode 1))


;;; Misc

(use-package subword
  :defer t
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  :config
  (diminish 'subword-mode))

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("M-µ c :" . avy-goto-char)))

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
