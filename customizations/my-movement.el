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
(global-set-key (kbd "C-ö") 'ace-jump-mode)

;; Automatically wrap isearch.
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))
