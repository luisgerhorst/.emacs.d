;;; ELPA

(progn
  (require 'package)

  (setq package-archives
        (append '(("melpa-stable" . "https://stable.melpa.org/packages/")
                  ("melpa" . "https://melpa.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/"))
                package-archives)

        package-archive-priorities
        '(("melpa-stable" . 10)
          ("melpa" . 5)
          ("gnu" . 0)
          ("marmalade" . -5))

        ;; Pinned packages. Must be added here since use-package's :pin does not
        ;; really work, see https://github.com/jwiegley/use-package/issues/343
        ;;
        ;; After adding a new pinned repository you MUST run
        ;; `package-refresh-contents' before installing the package.
        package-pinned-packages
        '((eclim . "melpa")
          (company-emacs-eclim . "melpa")
          (ensime . "melpa-stable")
          (dumb-jump . "melpa")
          ;; Because I wanted to customize `exec-path-from-shell-shell-name':
          (exec-path-from-shell . "melpa")))

  (package-initialize)

  ;; Load archives and install packages when Emacs opens for the first time
  (when (not package-archive-contents)
    (package-refresh-contents)
    ;; We'll use the `package-selected-packages' variable to remember installed
    ;; packages if .emacs.d/elpa gets deleted. The following code ensures all
    ;; listed packages are installed. Before calling this function you should
    ;; always call `package-refresh-contents' because otherwise newly pinned
    ;; packages may not be considered. Packages installed by the user
    ;; (e.g. using `package-install') will be added automatically to
    ;; `package-selected-packages' which is stored in `custom-file'.
    (package-install-selected-packages)))

;;; Site-Lisp

(progn
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

  (require 'cl)
  (defun luis-add-subdirs-to-load-path (parent-dir)
    "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
    (let* ((default-directory parent-dir))
      (progn
        (setq load-path
              (append
               (remove-if-not
                (lambda (dir) (file-directory-p dir))
                (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
               load-path)))))

  (luis-add-subdirs-to-load-path (locate-user-emacs-file "site-lisp/")))


(provide 'luis-packages)
