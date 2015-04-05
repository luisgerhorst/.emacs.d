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
(require-package 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-2") 'er/expand-region)

;; Fancier manipulate whitespace function.
(global-set-key [remap just-one-space] 'cycle-spacing)

;; automatically indenting yanked text if in programming-modes

(require-package 'dash)
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

;; There seems to be an error with M-^ in my Emacs, when I press M-^, Emacs
;; thinks C-^ is pressed.
(global-set-key (kbd "C-^") 'join-line)

(global-set-key [remap join-line] 'join-line-and-indent)
(defun join-line-and-indent ()
  (interactive)
  (join-line)
  (indent-for-tab-command))

(global-set-key [remap open-line] 'open-line-and-indent)
(defun open-line-and-indent ()
  (interactive)
  (newline-and-indent)
  (end-of-line 0)
  (indent-for-tab-command))

;; Continue comment on newline.
(setq-default comment-multi-line t)

;; `comment-line` function will be built in in Emacs 25.1
;; http://endlessparentheses.com/new-in-emacs-25-1-comment-line.html
(global-set-key (kbd "C-;") #'endless/comment-line)

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

(global-set-key (kbd "C-c d") #'delete-trailing-whitespace)

(require-package 'auto-complete)
(require 'auto-complete)
(add-hook 'auto-complete-mode-hook
          (lambda ()
            ;; Use M-n and M-p to select next/previous completion and use these for moving
            ;; by line.
            (define-key ac-menu-map (kbd "C-n") nil)
            (define-key ac-menu-map (kbd "C-p") nil)))
