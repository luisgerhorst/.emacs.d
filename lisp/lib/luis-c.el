;; -*- lexical-binding: t -*-

;;; Systemprogrammierung

(defun luis-sp-comment (&optional arg)
  "Create new C /*K ... */ comment, for Systemprogrammierung correction."
  (interactive "p")
  (kmacro-exec-ring-item
   (quote (";jjK " 0 "%d"))
   arg))

(defun luis-c-kill-comment (&optional arg)
  "Delete C comment starting with /* and ending with */."
  (interactive "p")
  (kmacro-exec-ring-item
   (quote ([19 42 47 13 27 181 181 99 115 18 47 42 13 16 5 23] 0 "%d"))
   arg))


;;; C

(use-package cc-mode
  :defer t
  :bind (:map
         c-mode-map
         ("C-c ;" . luis-sp-comment)
         ;; TODO: Fix.
         ;; ("C-c :" . luis-c-kill-comment)
         )
  :config
  ;; C
  (setq c-basic-offset tab-width)

  ;; Set default indentation style to K&R instead of weird GNU style. See
  ;; https://www.emacswiki.org/emacs/IndentingC#toc2
  (unless (listp c-default-style)
    (setq c-default-style nil))
  (add-to-list 'c-default-style '(other . "k&r"))

  (add-hook 'c-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'c-mode-hook #'luis-flycheck-unless-file-remote)

  ;; C++
  (add-hook 'c++-mode-hook #'luis-company-configure-automatic-completion))

(use-package xcscope
  :defer t
  :init
  (add-hook 'c-mode-hook #'cscope-minor-mode)
  (add-hook 'c++-mode-hook #'cscope-minor-mode)
  (add-hook 'dired-mode-hook #'cscope-minor-mode)
  :config
  (define-key cscope-minor-mode-keymap cscope-keymap-prefix nil)
  (setq cscope-keymap-prefix (kbd "C-c o"))
  (define-key cscope-minor-mode-keymap cscope-keymap-prefix
    cscope-command-map))

(defun luis-irony-if-installed-unless-file-remote ()
  (let ((current-file (buffer-file-name (current-buffer))))
    (when (and
           ;; File remote?
           (not (and current-file (file-remote-p current-file)))
           ;; Installed?
           (condition-case nil
               (irony--find-server-executable)
             (irony-server-error nil)))
      (irony-mode 1))))

(use-package irony
  :defer t
  :init
  (add-hook 'c-mode-hook #'luis-irony-if-installed-unless-file-remote)
  (add-hook 'c++-mode-hook #'luis-irony-if-installed-unless-file-remote)
  (add-hook 'objc-mode-hook #'luis-irony-if-installed-unless-file-remote))


;;; Completion

(use-package company-irony
  :defer t
  :init
  (with-eval-after-load 'irony
    (require 'company)
    (add-to-list 'company-backends #'company-irony)
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))

(use-package company-c-headers
  :defer t
  :init
  (with-eval-after-load 'company-irony
    ;; Ensure this is executed after `company-irony' is added to
    ;; `company-backends', it must appear in the list first.
    (add-to-list 'company-backends #'company-c-headers)))


;;; Syntax Checking

(use-package flycheck-irony
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(defun luis-current-buffer-has-c-extension ()
    (string= "c" (file-name-extension (buffer-file-name (current-buffer)))))

(defvar luis-flycheck-c-gcc-make-args "-k")
(defvar luis-flycheck-c-gcc-make-jobs "4")

(with-eval-after-load 'flycheck

  (flycheck-define-command-checker 'luis-c-gcc-make
    "Behave just like c/c++-gcc but invokes gcc using make instead of calling it directly"
    :command
    '("make"
      "-j" (eval luis-flycheck-c-gcc-make-jobs)
      (eval luis-flycheck-c-gcc-make-args))
    :standard-input t
    :error-patterns
    '((error line-start
             (message "In file included from") " " (or "<stdin>" (file-name))
             ":" line ":" column ":" line-end)
      (info line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": note: " (message) line-end)
      (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
               ": warning: " (message (one-or-more (not (any "\n["))))
               (optional "[" (id (one-or-more not-newline)) "]") line-end)
      (error line-start (or "<stdin>" (file-name)) ":" line ":" column
             ": " (or "fatal error" "error") ": " (message) line-end))
    ;; For some reason the following causes an error when working on the linux kernel:
    :error-filter
    (lambda (errors)
      (flycheck-fold-include-levels (flycheck-sanitize-errors errors)
                                    "In file included from"))
    :modes '(c-mode)
    :next-checkers '((warning . c/c++-cppcheck)))

  (defun luis-disable-flycheck-c-gcc-make-for-headers ()
    (when (not (luis-current-buffer-has-c-extension))
      (add-to-list 'flycheck-disabled-checkers 'luis-c-gcc-make)))

  (add-hook 'c-mode-hook #'luis-disable-flycheck-c-gcc-make-for-headers))

;;; Linux

(with-eval-after-load 'flycheck

  ;; The following makes flycheck work in C files located in the Linux Kernel
  ;; source tree. Found at
  ;; https://stackoverflow.com/questions/29709967/using-flycheck-flymake-on-kernel-source-tree
  ;; Does not work for modules located outside the kernel tree.

  (defun luis-flycheck-linux-search-makefile ()
    "Search for linux top `Makefile' "
    (cl-labels
        ((find-makefile-file-r (path)
                               (let* ((parent (file-name-directory path))
                                      (file (concat parent "Makefile")))
                                 (cond
                                  ((file-exists-p file)
                                   (progn
                                     (with-temp-buffer
                                       (insert-file-contents file)
                                       (if (string-match "VERSION = [0-9]+[[:space:]]*PATCHLEVEL" (buffer-string))
                                           (throw 'found-it parent)
                                         (find-makefile-file-r (directory-file-name parent))))))
                                  ((equal path parent) (throw 'found-it nil))
                                  (t (find-makefile-file-r (directory-file-name parent)))))))
      (if (buffer-file-name)
          (catch 'found-it
            (find-makefile-file-r (buffer-file-name)))
        (error "buffer is not visiting a file"))))

  (flycheck-define-command-checker 'luis-linux
    "Linux source checker"
    :command
    '("make"
      "C=1"
      "-C" (eval (luis-flycheck-linux-search-makefile))
      (eval (concat (file-name-sans-extension (file-relative-name
                                               buffer-file-name (luis-flycheck-linux-search-makefile))) ".o")))
    ;; Copied from c/c++-gcc:
    :error-patterns
    '((error line-start
             (message "In file included from") " " (file-name) ":" line ":"
             column ":"
             line-end)
      (info line-start (file-name) ":" line ":" column
            ": note: " (message) line-end)
      (warning line-start (file-name) ":" line ":" column
               ": warning: " (message) line-end)
      (error line-start (file-name) ":" line ":" column
             ": " (or "fatal error" "error") ": " (message) line-end))
    :error-filter
    (lambda (errors)
      (let ((errors (flycheck-sanitize-errors errors)))
        (dolist (err errors)
          (let* ((fn (flycheck-error-filename err))
                 (rn0 (file-relative-name fn default-directory)) ; flycheck-fix-error-filename converted to absolute, revert
                 (rn1 (expand-file-name rn0 (luis-flycheck-linux-search-makefile))) ; make absolute relative to "make -C dir"
                 (ef (file-relative-name rn1 default-directory))) ; relative to source
            (setf (flycheck-error-filename err) ef))))
      errors)
    :modes '(c-mode)
    :next-checkers '((warning . c/c++-cppcheck)))

  (defun luis-disable-flycheck-linux-for-headers ()
    (when (not (luis-current-buffer-has-c-extension))
      (add-to-list 'flycheck-disabled-checkers #'luis-linux)))

  (add-hook 'c-mode-hook #'luis-disable-flycheck-linux-for-headers))

(defun luis-add-mode-dir-local-variables (mode-vars)
  (let ((mode (nth 0 mode-vars))
        (assignments (nthcdr 1 mode-vars)))
    (mapc (lambda (assignment)
            (let ((variable (car assignment))
                  (value (cdr assignment)))
              (add-dir-local-variable mode variable value)))
          assignments)))

(defun luis-add-dir-local-variables (vars)
  "Calls `add-dir-local-variable' for every assignment in the structure"
  (mapc #'luis-add-mode-dir-local-variables vars))

(defun luis-add-linux-style-dir-local-variables ()
  (luis-add-dir-local-variables
   '((c-mode
      (indent-tabs-mode . t)
      (c-basic-offset . 8)
      (tab-width . 8)
      (c-file-style . "linux")))))

(defun luis-add-linux-dir-local-variables ()
  (interactive)
  (luis-add-linux-style-dir-local-variables)
  (add-dir-local-variable 'c-mode 'flycheck-checker 'luis-linux))

(defun luis-add-linux-arm-dir-local-variables ()
  (interactive)
  (luis-add-linux-dir-local-variables)
  ;; This doesn't work, bug reported.
  ;; (add-dir-local-variable 'c-mode 'eval
  ;;                         '(progn
  ;;                            (make-local-variable 'process-environment)
  ;;                            (setq process-environment (copy-sequence process-environment))
  ;;                            (setenv "ARCH" "arm")
  ;;                            (setenv "CROSS_COMPILE" "/usr/bin/arm-linux-gnueabi-")))
  )

(defun luis-add-linux-kmod-dir-local-variables ()
  (interactive)
  (luis-add-linux-style-dir-local-variables)
  (add-dir-local-variable 'c-mode 'flycheck-checker 'luis-c/c++-gcc-make))


(provide 'luis-c)
