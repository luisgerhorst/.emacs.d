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
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(define-key auto-highlight-symbol-mode-map (kbd "M--") nil) ; Thats negative-argument.
(customize-set-variable 'ahs-default-range 'ahs-range-whole-buffer)
(ahs-set-idle-interval 9999999999) ;; 'Disable' automatic highlighting

;; ahs-forward/backward always leave the point at the same postion in the symbol so
;; we don't have to worry about this.
(evil-define-motion my/evil-ahs-highlight-and-forward (count)
  (interactive "<c>")
  (ahs-highlight-now)
  (let ((n (or count 1)))
    (cond ((< 0 n) (dotimes (i n) (ahs-forward)))
          ((< n 0) (dotimes (i (- n)) (ahs-backward))))))

(evil-define-motion my/evil-ahs-highlight-and-backward (count)
  (interactive "<c>")
  (my/evil-ahs-highlight-and-forward (- (or count 1))))

;; Replace evil-search-word-forward/backward.
(define-key evil-motion-state-map (kbd "*") 'my/evil-ahs-highlight-and-forward)
(define-key evil-motion-state-map (kbd "#") 'my/evil-ahs-highlight-and-backward)

;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Jump directly to a word beginning with a given char.
(evil-leader/set-key "j" 'ace-jump-mode)
