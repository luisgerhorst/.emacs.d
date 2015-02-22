;; Customizations relating to editing a buffer.

;; Makes copy/paste command operate on current line if region is not
;; active. Also does some other stuff.
(load "whole-line-or-region.el")

;; Highlights matching parenthesis
(show-paren-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ;; Spaces instead of tabs.

(defun die-tabs ()
  "Replace tabs with spaces."
  (interactive)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Auto Fill for comments, enable per major mode in languages/*.el
(defun my/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

;; Auto Highligh Symbol
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

;; Enable upcase / downcase region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Fancier manipulate whitespace function.
(global-set-key (kbd "M-SPC") 'cycle-spacing)

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
(setq ac-auto-show-menu nil)

;; Join Lines, there seems to be an error with M-^, when I press M-^ Emacs
;; thinks this key is pressed.
(global-set-key (kbd "C-^") 'join-line)

;; Goto line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun open-line-and-indent ()
  (interactive)
  (newline-and-indent)
  (end-of-line 0)
  (indent-for-tab-command))
(global-set-key (kbd "C-o") 'open-line-and-indent)

(defun join-line-and-indent ()
  (interactive)
  (join-line)
  (indent-for-tab-command))
;; Actually, this is M-^ on my keyboard because of some bug on computer.
(global-set-key (kbd "C-^") 'join-line-and-indent)

;; Continue comment on newline.
(setq-default comment-multi-line t)
