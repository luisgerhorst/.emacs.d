;; -*- lexical-binding: t; -*-

;;; CAUTION: Do not enable until
;;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29789 is fixed. Also described
;;; here https://github.com/joostkremers/visual-fill-column/issues/1

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

(defvar luis-text-wrap-fill-paragraph-require-confirmation t
  "Ask for confirmation before `fill-paragraph'.")

(defun luis-text-wrap-fill-paragraph-after-confirmation ()
  "Ask the first time a paragraph is filled in a buffer.
Confirmation is always skipped if
`luis-text-wrap-fill-paragraph-require-confirmation' is nil."
  (interactive)
  (when (not (when luis-text-wrap-fill-paragraph-require-confirmation
               (pcase (car (read-multiple-choice
                            "Really fill paragraph in visually wrapped buffer?"
                            '((?y "yes" "Fill the paragraph and do not ask again")
                              (?n "no" "Don't fill the paragraph and warn me again next time")
                              (?d "disable visual wrapping" "Disable luis-text-wrap-mode"))))
                 (?y (progn (setq-local luis-text-wrap-fill-paragraph-require-confirmation nil)
                            (call-interactively #'fill-paragraph)))
                 (?n nil)
                 (?d (progn (luis-text-wrap-mode -1)
                            (call-interactively #'fill-paragraph))))))))

(defvar luis-text-wrap-mode-enable-visual-fill-column-mode-in-emacs-pre-26-1 nil
  "Enable this mode even if the Emacs version is lower than 26.1
Emacs versions before 26.1 have a bug that can crash Emacs when
visual-fill-column-mode is enabled (a mode employed by
luis-text-wrap-mode). For further information, see
https://github.com/joostkremers/visual-fill-column/issues/1.")

(defvar luis-text-wrap-mode-no-warn-visual-fill-column-mode-in-emacs-pre-26-1 nil
  "Don't issue a warning when an Emacs version lower than 26.1 prevents this mode from being enabled.
Emacs versions before 26.1 have a bug that can crash Emacs when
visual-fill-column-mode is enabled (a mode employed by
luis-text-wrap-mode). For further information, see
https://github.com/joostkremers/visual-fill-column/issues/1.")

(define-minor-mode luis-text-wrap-mode
  "Visually wrap lines between wrap prefix and `fill-column'."
  :lighter " TextWrap"
  (if luis-text-wrap-mode
      (progn
        (luis-text-wrap--save-state)
        (visual-line-mode 1)
        (diminish 'visual-line-mode)
        (adaptive-wrap-prefix-mode 1)
        (if (and (version< emacs-version "26.1")
                 (not luis-text-wrap-mode-enable-visual-fill-column-mode-in-emacs-pre-26-1))
            (when (not luis-text-wrap-mode-no-warn-visual-fill-column-mode-in-emacs-pre-26-1)
              (message "You are running an Emacs version < 26.1 which has a bug that can crash Emacs when visual-fill-column-mode is enabled (a mode employed by luis-text-wrap-mode). This bug has been fixed starting with Emacs version 26.1. visual-fill-column-mode is left disabled for now. To enable it anyway, add (setq luis-text-wrap-mode-enable-visual-fill-column-mode-in-emacs-pre-26-1 t) to your .emacs.d/init.el and retry. To disable this warning (but leave luis-text-wrap-mode disabled), add (setq luis-text-wrap-mode-no-warn-visual-fill-column-mode-in-emacs-pre-26-1 t). For further information, see https://github.com/joostkremers/visual-fill-column/issues/1"))
          (visual-fill-column-mode 1))
        (local-set-key [remap fill-paragraph]
                       #'luis-text-wrap-fill-paragraph-after-confirmation)
        (local-set-key [remap mu4e-fill-paragraph]
                       #'luis-text-wrap-fill-paragraph-after-confirmation))
    (luis-text-wrap--restore-state)
    (diminish-undo 'visual-line-mode)
    (local-set-key [remap fill-paragraph] nil)
    (local-set-key [remap mu4e-fill-paragraph] nil)
    (kill-local-variable 'luis-text-wrap-fill-paragraph-require-confirmation)))

(provide 'luis-text-wrap)
