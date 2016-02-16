;; Auto Highligh Symbol
(require-package 'auto-highlight-symbol)
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(define-key auto-highlight-symbol-mode-map (kbd "M--") nil)
(customize-set-variable 'ahs-default-range 'ahs-range-whole-buffer)
(ahs-set-idle-interval 9999999999) ;; 'Disable' automatic highlighting
(global-set-key (kbd "M-n") (lambda ()
                              (interactive)
                              (ahs-highlight-now)
                              (ahs-forward)))
(global-set-key (kbd "M-p") (lambda ()
                              (interactive)
                              (ahs-highlight-now)
                              (ahs-backward)))

;; C-s with overview.
(require-package 'swiper)
(require 'ivy)
(setq ivy-display-style 'plain)
(setq ivy-wrap t)
(setq ivy-height 10)
(global-set-key (kbd "C-s") 'swiper)

(require-package 'avy)
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(require-package 'paredit)
(require 'paredit)
;; These also work in strings.
(global-set-key (kbd "C-M-u") #'paredit-backward-up)
(global-set-key (kbd "C-M-d") #'paredit-forward-down)
;; I need these more often then forward/backward-list.
(global-set-key (kbd "C-M-p") #'paredit-backward-down)
(global-set-key (kbd "C-M-n") #'paredit-forward-up)

;;; Scroll relative to current window size.

(setq relative-scroll-ratio 0.5)

(defun relative-scroll-lines ()
  (max 1 (round (* relative-scroll-ratio (window-total-height)))))

(defun relative-scroll-up-command (&optional argument)
  (interactive "^P")
  (scroll-up-command (or argument (relative-scroll-lines))))
(global-set-key [remap scroll-up-command] 'relative-scroll-up-command)

(defun relative-scroll-down-command (&optional argument)
  (interactive "^P")
  (scroll-down-command (or argument (relative-scroll-lines))))
(global-set-key [remap scroll-down-command] 'relative-scroll-down-command)


(provide 'luis-movement)
