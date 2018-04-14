;; -*- lexical-binding: t; -*-

;; The bindings listed here are not sent to Emacs by iTerm by default. I use
;; iTerm -> Preferences -> Keys -> Key Mappings, Send Escape Sequence to
;; redirect them here.

(defun luis-bound-command (key)
  "Return a closure that searches the command currently bound to the given key and calls it interactively"
  (lambda ()
    (interactive)
    (call-interactively (key-binding key))))

(when (not (display-graphic-p))
  (global-set-key (kbd "M-µ c ;") (luis-bound-command (kbd "C-;")))
  (global-set-key (kbd "M-µ c .") (luis-bound-command (kbd "C-.")))
  (global-set-key (kbd "M-µ c :") (luis-bound-command (kbd "C-:")))
  ;; Non-printable bindings, prefixed with an additional µ and using a character
  ;; that fits their function.
  (global-set-key (kbd "M-µ µ c d") (luis-bound-command (kbd "<C-S-backspace>")))
  (global-set-key (kbd "M-µ µ c m d") (luis-bound-command (kbd "<C-M-backspace>")))
  (global-set-key (kbd "M-µ µ c s") (luis-bound-command (kbd "C-SPC"))))


(provide 'luis-iterm)
