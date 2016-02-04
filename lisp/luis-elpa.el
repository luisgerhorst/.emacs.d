(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '()
  "List of packages that are required by the configuration. Used
  to detect orphan packages. Use require-package or
  maybe-require-package to add packages to this list.")

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))

;;; Delete orphan packages.

;; TODO: Fill the following variables.

(defvar luis-elpa/used-distant-packages '()
  "A list of all distant packages that are effectively used.")

(defvar luis-elpa/protected-packages nil
  "A list of packages that will be protected from removal as orphans.")

(defun luis-elpa/get-packages-dependencies ()
  "Returns dependencies hash map for all packages in `package-alist'."
  (let ((result (make-hash-table :size 512)))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (luis-elpa/get-package-deps-from-alist pkg-sym)))
        (dolist (dep deps)
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value (add-to-list 'value pkg-sym) (list pkg-sym))
                     result)))))
    result))

(defun luis-elpa/get-implicit-packages (packages)
  "Returns packages in `packages-alist' which are not found in PACKAGES."
  (let (imp-pkgs)
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (unless (object-assoc pkg-sym :name packages)
          (add-to-list 'imp-pkgs pkg-sym))))
    imp-pkgs))

(defun luis-elpa/get-orphan-packages
    (dist-pkgs implicit-pkgs dependencies)
  "Return orphan packages."
  (let (result)
    (dolist (imp-pkg implicit-pkgs)
      (when (luis-elpa/is-package-orphan
             imp-pkg dist-pkgs dependencies)
        (add-to-list 'result imp-pkg)))
    result))

(defun luis-elpa/is-package-orphan (pkg-name dist-pkgs dependencies)
  "Returns not nil if PKG-NAME is the name of an orphan package."
  (unless (or (object-assoc pkg-name :name dist-pkgs)
              (memq pkg-name luis-elpa/protected-packages))
    (if (ht-contains? dependencies pkg-name)
        (let ((parents (ht-get dependencies pkg-name)))
          (cl-reduce (lambda (x y) (and x y))
                     (mapcar (lambda (p) (luis-elpa/is-package-orphan
                                          p dist-pkgs dependencies))
                             parents)
                     :initial-value t))
      (not (object-assoc pkg-name :name dist-pkgs)))))

;; (defun configuration-layer//get-package-directory (pkg-name)
;;   "Return the directory path for package with name PKG-NAME."
;;   (let ((pkg-desc (assq pkg-name package-alist)))
;;     (cond
;;      ((version< emacs-version "24.3.50")
;;       (let* ((version (aref (cdr pkg-desc) 0))
;;              (elpa-dir (concat user-emacs-directory "elpa/"))
;;              (pkg-dir-name (format "%s-%s.%s"
;;                                    (symbol-name pkg-name)
;;                                    (car version)
;;                                    (cadr version))))
;;         (expand-file-name (concat elpa-dir pkg-dir-name))))
;;      (t (package-desc-dir (cadr pkg-desc))))))

(defun luis-elpa/get-package-deps-from-alist (pkg-name)
  "Return the dependencies alist for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (cond
       ((version< emacs-version "24.3.50") (aref (cdr pkg-desc) 1))
       (t (package-desc-reqs (cadr pkg-desc)))))))

;; (defun configuration-layer//get-package-deps-from-archive (pkg-name)
;;   "Return the dependencies alist for a PKG-NAME from the archive data."
;;   (let* ((pkg-arch (assq pkg-name package-archive-contents))
;;          (reqs (when pkg-arch (if (version< emacs-version "24.3.50")
;;                               (aref (cdr pkg-arch) 1)
;;                             (package-desc-reqs (cadr pkg-arch))))))
;;     ;; recursively get the requirements of reqs
;;     (dolist (req reqs)
;;       (let* ((pkg-name2 (car req))
;;              (reqs2 (configuration-layer//get-package-deps-from-archive
;;                      pkg-name2)))
;;         (when reqs2 (setq reqs (append reqs2 reqs)))))
;;     reqs))

(defun luis-elpa/get-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (cond
       ((version< emacs-version "24.3.50") (package-version-join
                                            (aref (cdr pkg-desc) 0)))
       (t (package-version-join (package-desc-version (cadr pkg-desc))))))))

;; (defun configuration-layer//get-package-version (pkg-name)
;;   "Return the version list for package with name PKG-NAME."
;;   (let ((version-string (luis-elpa/get-package-version-string pkg-name)))
;;     (unless (string-empty-p version-string)
;;       (version-to-list version-string))))

;; (defun configuration-layer//get-latest-package-version-string (pkg-name)
;;   "Return the version string for package with name PKG-NAME."
;;   (let ((pkg-arch (assq pkg-name package-archive-contents)))
;;     (when pkg-arch
;;       (cond
;;        ((version< emacs-version "24.3.50") (package-version-join
;;                                             (aref (cdr pkg-arch) 0)))
;;        (t (package-version-join (package-desc-version (cadr pkg-arch))))))))

;; (defun configuration-layer//get-latest-package-version (pkg-name)
;;   "Return the versio list for package with name PKG-NAME."
;;   (let ((version-string
;;          (configuration-layer//get-latest-package-version-string pkg-name)))
;;     (unless (string-empty-p version-string)
;;       (version-to-list version-string))))

(defun luis-elpa/package-delete (pkg-name)
  "Delete package with name PKG-NAME."
  (cond
   ((version< emacs-version "24.3.50")
    (let ((v (luis-elpa/get-package-version-string pkg-name)))
      (when v (package-delete (symbol-name pkg-name) v))))
   ((version<= "25.0.50" emacs-version)
    (let ((p (cadr (assq pkg-name package-alist))))
      ;; add force flag to ignore dependency checks in Emacs25
      (when p (package-delete p t t))))
   (t (let ((p (cadr (assq pkg-name package-alist))))
        (when p (package-delete p))))))

(defun configuration-layer/delete-orphan-packages (packages)
  "Delete PACKAGES if they are orphan."
  (interactive)
  (let* ((dependencies (luis-elpa/get-packages-dependencies))
         (implicit-packages (luis-elpa/get-implicit-packages
                             luis-elpa/used-distant-packages))
         (orphans (luis-elpa/get-orphan-packages
                   luis-elpa/used-distant-packages
                   implicit-packages
                   dependencies))
         (orphans-count (length orphans))
         deleted-count)
    ;; (message "dependencies: %s" dependencies)
    ;; (message "implicit: %s" implicit-packages)
    ;; (message "orphans: %s" orphans)
    (when orphans
      (progn
        (setq deleted-count 0)
        (dolist (orphan orphans)
          (setq deleted-count (1+ deleted-count))
          (luis-elpa/package-delete orphan))))))


(provide 'luis-elpa)
