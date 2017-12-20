;; The bindings listed here are not sent to Emacs by iTerm by default. I use
;; iTerm -> Preferences -> Keys -> Key Mappings, Send Escape Sequence to
;; redirect them here.
(when (not (display-graphic-p))
  (global-set-key (kbd "M-µ c ;") #'comment-line)
  (global-set-key (kbd "M-µ c .") #'iedit-mode)
  ;; Non-printable bindings, prefixed with an additional µ and using a character
  ;; that fits their function.
  (global-set-key (kbd "M-µ µ c d") #'kill-whole-line))


(provide 'luis-iterm)
