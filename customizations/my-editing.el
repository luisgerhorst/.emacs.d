;; Customizations relating to editing a buffer.

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

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
;; Prevent asking to recover changed file when changes were discarded.
(setq auto-save-default nil)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)

;; Tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ;; Spaces instead of tabs.

(defun die-tabs ()
  "Replace tabs with spaces."
  (interactive)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; Indent yanked text in specified modes
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode                        
                        ;; Insert modes here:
                        '(emacs-lisp-mode js2-mode haskell-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Auto Complete
;; (require 'fuzzy)
;; (require 'auto-complete)
;; (setq ac-auto-show-menu t
;;       ac-quick-help-delay 0.5
;;       ac-use-fuzzy t)
;; (global-auto-complete-mode t)

;; Smart Scan
(define-globalized-minor-mode my/global-smartscan-mode smartscan-mode
  (lambda () (smartscan-mode t)))
(my/global-smartscan-mode t)

;; Auto Fill for comments, enable per major mode in languages/*.el
(defun my/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
