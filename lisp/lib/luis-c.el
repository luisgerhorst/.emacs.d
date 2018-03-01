;; -*- lexical-binding: t -*-

(use-package cc-mode
  :defer t
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
  (setq cscope-keymap-prefix (kbd "C-c o"))
  (add-hook 'c-mode-hook #'cscope-minor-mode)
  (add-hook 'c++-mode-hook #'cscope-minor-mode)
  (add-hook 'dired-mode-hook #'cscope-minor-mode))

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

;;; Linux

;; The following makes flycheck work in C files located in the Linux Kernel
;; source tree. Found at
;; https://stackoverflow.com/questions/29709967/using-flycheck-flymake-on-kernel-source-tree
;; Does not work for modules located outside the kernel tree.
(with-eval-after-load 'flycheck
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

  (flycheck-define-checker luis-linux
    "Linux source checker"
    :command
    ("make" "C=1" "-C" (eval (luis-flycheck-linux-search-makefile))
     (eval (concat (file-name-sans-extension (file-relative-name
                                              buffer-file-name (luis-flycheck-linux-search-makefile))) ".o")))
    :error-patterns
    ((error line-start
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
    :modes (c-mode)))

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

(defun luis-add-linux-dir-local-variables ()
  (interactive)
  (luis-add-dir-local-variables
   '((c-mode
      (indent-tabs-mode . t)
      (c-basic-offset . 8)
      (tab-width . 8)
      (c-file-style . "linux")
      (flycheck-checker . luis-linux)))))


(provide 'luis-c)
