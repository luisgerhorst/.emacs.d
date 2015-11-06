;; Navigate in a buffer.

(global-set-key (kbd "M-i") 'imenu)

;; Fancier goto-line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
;; Replace goto-line
(global-set-key [remap goto-line] 'goto-line-with-feedback)

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

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Jump directly to a word beginning with a given char.
(require-package 'ace-jump-mode)
(global-set-key (kbd "C-ö") 'ace-jump-mode)

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
