(require 'adaptive-wrap)
(require 'visual-fill-column)
(require 'diminish)

(defvar luis-text-wrap--saved-mode-enabled-states nil
  "Saves enabled states of local minor modes. The mode function
and variable must behave according to define-minor-mode's
default.")

(defun luis-text-wrap--save-state ()
  (set (make-local-variable 'luis-text-wrap--saved-mode-enabled-states) nil)
  ;; Save enabled modes.
  (dolist (var '(visual-line-mode
                 adaptive-wrap-prefix-mode
                 visual-fill-column-mode))
    (push (cons var (symbol-value var))
          luis-text-wrap--saved-mode-enabled-states)))

(defun luis-text-wrap--restore-state ()
  ;; Restore enabled modes.
  (dolist (saved luis-text-wrap--saved-mode-enabled-states)
    (if (cdr saved)
        (funcall (car saved) 1)
      (funcall (car saved) -1)))
  ;; Clean up.
  (kill-local-variable 'luis-text-wrap--saved-mode-enabled-states))

(define-minor-mode luis-text-wrap-mode
  "Nice line wrapping for text files."
  :lighter " TextWrap"
  (if luis-text-wrap-mode
      (progn
        (luis-text-wrap--save-state)
        (visual-line-mode 1)
        (diminish 'visual-line-mode)
        (adaptive-wrap-prefix-mode 1)
        (visual-fill-column-mode 1))
    (luis-text-wrap--restore-state)
    (diminish-undo 'visual-line-mode)))


(provide 'luis-text-wrap)
