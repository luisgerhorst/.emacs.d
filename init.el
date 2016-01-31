;; Bookmark on startup.
(setq initial-buffer-choice
      (lambda ()
        (bookmark-bmenu-list)
        (get-buffer "*Bookmark List*")))

;;; Packages

;; Define package repositories
(require 'package)

(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; On-demand installation of packages

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

;; Load and activate emacs packages. Do this first so that the packages
;; are loaded before you start trying to modify them.  This also sets
;; the load path.
(setq package-enable-at-startup nil)
(package-initialize)

;; Download the ELPA archive description if needed. This informs Emacs
;; about the latest versions of all packages, and makes them available
;; for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path (expand-file-name "vendor/" user-emacs-directory))

;; Prefix key for launching apps.
(define-prefix-command 'launcher-map)
(global-set-key (kbd "H-s") 'launcher-map)

;;;;
;; My Customizations
;;;;

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "customizations/my-custom.el" user-emacs-directory))
(load custom-file)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (expand-file-name "customizations/" user-emacs-directory))

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "my-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "my-navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "my-user-interaction.el")
(load "my-look.el")

(load "my-files.el")

;; These customizations make editing a bit nicer.
(load "my-editing.el")
(load "my-movement.el")

(load "my-languages.el")

(load "my-mail.el")
(load "my-mail-private.el")

(load "my-apps.el")

;; Hard-to-categorize customizations
(load "my-misc.el")

(load "my-machine-local.el")

;; This is for private stuff. Never publish.
(load "my-private.el")


;; Update the Emacs config automatically using git.
(setq git-pull-emacs-config-command
      (concat "cd " user-emacs-directory "; git pull"))
(start-process-shell-command "git pull .emacs.d" "*Auto Update Emacs Config*" git-pull-emacs-config-command)
