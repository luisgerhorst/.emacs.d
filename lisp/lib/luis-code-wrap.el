(require 'adaptive-wrap)

(defvar luis-code-wrap--saved-variable-states nil)
(defvar luis-code-wrap--saved-mode-enabled-states nil
  "Saves enabled states of local minor modes. The mode function
and variable must behave according to define-minor-mode's
default.")

(defun luis-code-wrap--save-state ()
  (set (make-local-variable 'luis-code-wrap--saved-variable-states) nil)
  (set (make-local-variable 'luis-code-wrap--saved-mode-enabled-states) nil)
  ;; Save the local values of some variables, to be restored if
  ;; luis-code-wrap-mode is turned off.
  (dolist (var '(line-move-visual
                 truncate-lines
                 word-wrap))
    (if (local-variable-p var)
        (push (cons var (symbol-value var))
              luis-code-wrap--saved-variable-states)))
  ;; Save enabled modes.
  (dolist (var '(adaptive-wrap-prefix-mode))
    (push (cons var (symbol-value var))
          luis-code-wrap--saved-mode-enabled-states)))

(defun luis-code-wrap--restore-state ()
  ;; Restore variable values.
  (kill-local-variable 'line-move-visual)
  (kill-local-variable 'truncate-lines)
  (kill-local-variable 'word-wrap)
  (dolist (saved luis-code-wrap--saved-variable-states)
    (set (make-local-variable (car saved)) (cdr saved)))
  ;; Restore enabled modes.
  (dolist (saved luis-code-wrap--saved-mode-enabled-states)
    (if (cdr saved)
        (funcall (car saved) 1)
      (funcall (car saved) -1)))
  ;; Clean up.
  (kill-local-variable 'luis-code-wrap--saved-variable-states)
  (kill-local-variable 'luis-code-wrap--saved-mode-enabled-states))

(define-minor-mode luis-code-wrap-mode
  "Nice line wrapping for code."
  :lighter " CodeWrap"
  (if luis-code-wrap-mode
      (progn
        (luis-code-wrap--save-state)
        (set (make-local-variable 'line-move-visual) nil)
        (setq truncate-lines nil
              word-wrap t)
        (adaptive-wrap-prefix-mode 1))
    (luis-code-wrap--restore-state)))


(provide 'luis-code-wrap)
