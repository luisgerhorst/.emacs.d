;; Auto Highligh Symbol
(use-package auto-highlight-symbol
  :ensure t
  :bind (("M-n" . ahs-highlight-now-and-forward)
         ("M-p" . ahs-highlight-now-and-backward))
  :config
  (global-auto-highlight-symbol-mode t)
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
  :ensure nil
  :init
  (setq-default save-place t)
  ;; Keep track of saved places in ~/.emacs.d/places
  (setq save-place-file (expand-file-name "places" user-emacs-directory))
  :demand)

(use-package paredit
  :ensure t
  :bind (;; These also work in strings.
         ("C-M-u" . paredit-backward-up)
         ("C-M-d" . paredit-forward-down)
         ;; I need these more often then forward/backward-list.
         ("C-M-p" . paredit-backward-down)
         ("C-M-n" . paredit-forward-up)))

;; Scroll relative to current window size.
(progn
  (setq relative-scroll-ratio 0.5)

  (defun relative-scroll-lines ()
    (max 1 (round (* relative-scroll-ratio (window-total-height)))))

  (defun relative-scroll-up-command (&optional argument)
    (interactive "^P")
    (scroll-up-command (or argument (relative-scroll-lines))))

  (defun relative-scroll-down-command (&optional argument)
    (interactive "^P")
    (scroll-down-command (or argument (relative-scroll-lines))))

  (global-set-key [remap scroll-up-command] 'relative-scroll-up-command)
  (global-set-key [remap scroll-down-command] 'relative-scroll-down-command))


(provide 'luis-movement)
