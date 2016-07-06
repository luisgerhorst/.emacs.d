;;; ELPA

(require 'package)

(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version) t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;;; Site-Lisp

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

(require 'cl)
(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(sanityinc/add-subdirs-to-load-path (locate-user-emacs-file "site-lisp/"))


(provide 'luis-packages)
