;; Customizations relating to editing a buffer.

;; Tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)     ; Spaces instead of tabs.

(defun die-tabs ()
  "Replace tabs with spaces."
  (interactive)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; Auto Fill for comments, enable per major mode in languages/*.el
(defun my/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

;; Fancy manipulate whitespace function.
(evil-leader/set-key "SPC" 'cycle-spacing)

;; automatically indenting yanked text if in programming-modes

(require 'dash)

(defvar yank-indent-modes '()
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defun yank-unindented ()
  (interactive)
  (yank 1))

;; Auto Complete
(require 'auto-complete)
(setq ac-auto-start nil)
;; Normally, Evil's primitive auto complete commands are bound to these keys.
(define-key evil-insert-state-map (kbd "C-n") 'auto-complete)
(define-key evil-insert-state-map (kbd "C-p") nil) ; We don't need this with auto-complete.

;; Continue comment on newline.
(setq-default comment-multi-line t)

;; Make Emacs commenting functions work like NERD Commenter's one's.

(evil-leader/set-key "c A"
  (lambda (ARG)
    (interactive "*P")
    (comment-dwim ARG)
    (evil-append 1)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(evil-leader/set-key "c c" 'comment-or-uncomment-region-or-line)
