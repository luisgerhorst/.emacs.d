(require 'diminish)

(define-minor-mode luis-comment-auto-fill-mode
  "Nice line wrapping for text files."
  :lighter " CommentFill"
  (if luis-comment-auto-fill-mode
      (progn
        (kill-local-variable 'comment-auto-fill-only-comments)
        (diminish-undo 'auto-fill-function)
        (auto-fill-mode -1))
    (setq-local comment-auto-fill-only-comments t)
    (diminish 'auto-fill-function)
    (auto-fill-mode 1)))


(provide 'luis-comment-auto-fill)
