;; -*- lexical-binding: t; -*-

;;; Site-Lisp

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

(require 'cl)
(defun luis-add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; Beware that packages installed this way are overwritten by packages installed
;; from elpa package archives.
(luis-add-subdirs-to-load-path (locate-user-emacs-file "site-lisp/"))

;;; ELPA

(require 'package)

(defvar luis-online-package-archives t
  "Include online package archives in `package-archives'.
Disable this if you behind a proxy/firewall that may cause
requests to online package archives to hang.")

(when (not luis-online-package-archives)
  ;; Remove standard archives (i.e. gnu).
  (setq package-archives nil))

(when luis-online-package-archives
  (setq package-archives
        (append `(("melpa-stable" . "https://stable.melpa.org/packages/")
                  ("melpa" . "https://melpa.org/packages/")
                  ;; See below for additional items added to this list.
                  )
                package-archives)

        ;; When installing packages, the package with the highest version number
        ;; from the archive with the highest priority is selected.  When higher
        ;; versions are available from archives with lower priorities, the user
        ;; has to select those manually.
        package-archive-priorities
        '(("melpa-stable" . 10)
          ("melpa" . 5)
          ;; Default is 0. This applies to the gnu archive.  See below for
          ;; additional items added to this list.
          )

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
          (exec-path-from-shell . "melpa"))))

;; Create a local package archive mirror that only contains the installed
;; packages, using `elpamr-create-mirror-for-installed'. Then copy it to a
;; machine without direct archive access (e.g. by commiting it to your dotfiles
;; repo) and install the packages there.
;;
;; `elpa-mirror' must be in Site-Lisp.
(require 'elpa-mirror)
(setq elpamr-default-output-directory
      ;; Trailing slash required for use in `package-archives' list and `concat'!
      (file-name-as-directory (locate-user-emacs-file "elpa-archive")))

;; Only add the local mirror to the package archive lists when it is initialized
;; (detect it by checking whether 'archive-contents' exists as a heuristic).
(when (file-readable-p (concat elpamr-default-output-directory "archive-contents"))
  (setq package-archives
        (append `(("local" . ,elpamr-default-output-directory)) package-archives)
        ;; When possible we want to use the local mirror. When there's a new
        ;; version available online we install it and then update the local
        ;; archive mirror using `elpamr-create-mirror-for-installed' if it works
        ;; fine.
        package-archive-priorities
        '(("local" . 15))))

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
  (when (version<= "25.1" emacs-version)
    (package-install-selected-packages)))


(provide 'luis-packages)
