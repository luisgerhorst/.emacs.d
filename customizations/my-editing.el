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

;; Enable upcase / downcase region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-2") 'er/expand-region)

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

;; Add newlines with C-n when at end of buffer.
(setq next-line-add-newlines t)

(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

;; `comment-line` function will be built in in Emacs 25.1
;; http://endlessparentheses.com/new-in-emacs-25-1-comment-line.html
(global-set-key (kbd "C-;") #'endless/comment-line)
