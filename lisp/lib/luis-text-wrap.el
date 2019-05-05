;; -*- lexical-binding: t; -*-

(require 'adaptive-wrap)
(require 'visual-fill-column)
(require 'diminish)

(require 'rmc)
(unless (fboundp 'read-multiple-choice)
  ;; Copied from rmc.el
  (defun read-multiple-choice (prompt choices)
    "Ask user a multiple choice question.
PROMPT should be a string that will be displayed as the prompt.

CHOICES is an alist where the first element in each entry is a
character to be entered, the second element is a short name for
the entry to be displayed while prompting (if there's room, it
might be shortened), and the third, optional entry is a longer
explanation that will be displayed in a help buffer if the user
requests more help.

This function translates user input into responses by consulting
the bindings in `query-replace-map'; see the documentation of
that variable for more information.  In this case, the useful
bindings are `recenter', `scroll-up', and `scroll-down'.  If the
user enters `recenter', `scroll-up', or `scroll-down' responses,
perform the requested window recentering or scrolling and ask
again.

When `use-dialog-box' is t (the default), this function can pop
up a dialog window to collect the user input. That functionality
requires `display-popup-menus-p' to return t. Otherwise, a text
dialog will be used.

The return value is the matching entry from the CHOICES list.

Usage example:

\(read-multiple-choice \"Continue connecting?\"
                      \\='((?a \"always\")
                        (?s \"session only\")
                        (?n \"no\")))"
    (let* ((altered-names nil)
           (full-prompt
            (format
             "%s (%s): "
             prompt
             (mapconcat
              (lambda (elem)
                (let* ((name (cadr elem))
                       (pos (seq-position name (car elem)))
                       (altered-name
                        (cond
                         ;; Not in the name string.
                         ((not pos)
                          (format "[%c] %s" (car elem) name))
                         ;; The prompt character is in the name, so highlight
                         ;; it on graphical terminals...
                         ((display-supports-face-attributes-p
                           '(:underline t) (window-frame))
                          (setq name (copy-sequence name))
                          (put-text-property pos (1+ pos)
                                             'face 'read-multiple-choice-face
                                             name)
                          name)
                         ;; And put it in [bracket] on non-graphical terminals.
                         (t
                          (concat
                           (substring name 0 pos)
                           "["
                           (upcase (substring name pos (1+ pos)))
                           "]"
                           (substring name (1+ pos)))))))
                  (push (cons (car elem) altered-name)
                        altered-names)
                  altered-name))
              (append choices '((?? "?")))
              ", ")))
           tchar buf wrong-char answer)
      (save-window-excursion
        (save-excursion
	      (while (not tchar)
	        (message "%s%s"
                     (if wrong-char
                         "Invalid choice.  "
                       "")
                     full-prompt)
            (setq tchar
                  (if (and (display-popup-menus-p)
                           last-input-event ; not during startup
                           (listp last-nonmenu-event)
                           use-dialog-box)
                      (x-popup-dialog
                       t
                       (cons prompt
                             (mapcar
                              (lambda (elem)
                                (cons (capitalize (cadr elem))
                                      (car elem)))
                              choices)))
                    (condition-case nil
                        (let ((cursor-in-echo-area t))
                          (read-char))
                      (error nil))))
            (setq answer (lookup-key query-replace-map (vector tchar) t))
            (setq tchar
                  (cond
                   ((eq answer 'recenter)
                    (recenter) t)
                   ((eq answer 'scroll-up)
                    (ignore-errors (scroll-up-command)) t)
                   ((eq answer 'scroll-down)
                    (ignore-errors (scroll-down-command)) t)
                   ((eq answer 'scroll-other-window)
                    (ignore-errors (scroll-other-window)) t)
                   ((eq answer 'scroll-other-window-down)
                    (ignore-errors (scroll-other-window-down)) t)
                   (t tchar)))
            (when (eq tchar t)
              (setq wrong-char nil
                    tchar nil))
            ;; The user has entered an invalid choice, so display the
            ;; help messages.
            (when (and (not (eq tchar nil))
                       (not (assq tchar choices)))
	          (setq wrong-char (not (memq tchar '(?? ?\C-h)))
                    tchar nil)
              (when wrong-char
                (ding))
              (with-help-window (setq buf (get-buffer-create
                                           "*Multiple Choice Help*"))
                (with-current-buffer buf
                  (erase-buffer)
                  (pop-to-buffer buf)
                  (insert prompt "\n\n")
                  (let* ((columns (/ (window-width) 25))
                         (fill-column 21)
                         (times 0)
                         (start (point)))
                    (dolist (elem choices)
                      (goto-char start)
                      (unless (zerop times)
                        (if (zerop (mod times columns))
                            ;; Go to the next "line".
                            (goto-char (setq start (point-max)))
                          ;; Add padding.
                          (while (not (eobp))
                            (end-of-line)
                            (insert (make-string (max (- (* (mod times columns)
                                                            (+ fill-column 4))
                                                         (current-column))
                                                      0)
                                                 ?\s))
                            (forward-line 1))))
                      (setq times (1+ times))
                      (let ((text
                             (with-temp-buffer
                               (insert (format
                                        "%c: %s\n"
                                        (car elem)
                                        (cdr (assq (car elem) altered-names))))
                               (fill-region (point-min) (point-max))
                               (when (nth 2 elem)
                                 (let ((start (point)))
                                   (insert (nth 2 elem))
                                   (unless (bolp)
                                     (insert "\n"))
                                   (fill-region start (point-max))))
                               (buffer-string))))
                        (goto-char start)
                        (dolist (line (split-string text "\n"))
                          (end-of-line)
                          (if (bolp)
                              (insert line "\n")
                            (insert line))
                          (forward-line 1)))))))))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (assq tchar choices))))

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
                            "Really fill paragraphs in visually wrapped buffer?"
                            '((?y "yes" "Fill the paragraph, do not ask again")
                              (?n "no" "Don't fill the paragraph and ask again next time")
                              (?d "disable visual wrapping" "Disable luis-text-wrap-mode"))))
                 (?y (progn (setq-local luis-text-wrap-fill-paragraph-require-confirmation nil)
                            (call-interactively #'fill-paragraph)))
                 (?n nil)
                 (?d (progn (luis-text-wrap-mode -1)
                            (call-interactively #'fill-paragraph))))))))

(defvar luis-text-wrap-mode-visual-fill-column-mode-in-emacs-pre-26-1 nil
  "Enable visual-fill-column-mode even if the Emacs version is lower than 26.1.
Emacs versions before 26.1 have a bug that can crash Emacs when
visual-fill-column-mode is enabled (a mode employed by
luis-text-wrap-mode). For further information, see
https://github.com/joostkremers/visual-fill-column/issues/1. Also
see
`luis-text-wrap-mode-visual-fill-column-mode-warning-in-emacs-pre-26-1'.")

(defvar luis-text-wrap-mode-visual-fill-column-mode-warning-in-emacs-pre-26-1 nil
  "Don't issue a warning when an Emacs version lower than 26.1 prevents visual-fill-column-mode from being enabled.
Emacs versions before 26.1 have a bug that can crash Emacs when
visual-fill-column-mode is enabled (a mode employed by
luis-text-wrap-mode). For further information, see
https://github.com/joostkremers/visual-fill-column/issues/1. Also
see
`luis-text-wrap-mode-visual-fill-column-mode-in-emacs-pre-26-1'.")

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
                 (not luis-text-wrap-mode-visual-fill-column-mode-in-emacs-pre-26-1))
            (when luis-text-wrap-mode-visual-fill-column-mode-warning-in-emacs-pre-26-1
              (message "You are running an Emacs version < 26.1 which has a bug that can crash Emacs when visual-fill-column-mode is enabled (a mode employed by luis-text-wrap-mode). This bug has been fixed starting with Emacs version 26.1. visual-fill-column-mode is left disabled for now. To enable it anyway, add (setq luis-text-wrap-mode-visual-fill-column-mode-in-emacs-pre-26-1 t) to your .emacs.d/init.el and retry. To disable this warning (but leave luis-text-wrap-mode disabled), add (setq luis-text-wrap-mode-visual-fill-column-mode-warning-in-emacs-pre-26-1 t). For further information, see https://github.com/joostkremers/visual-fill-column/issues/1"))
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
