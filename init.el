;;;;
;; UI, turn of early to prevent momentary display
;;;;

;; Diable menu, tool and scroll bar.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No startup screen.
(setq inhibit-startup-message t)

;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
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
(load "my-ui.el")
(load "my-look.el")

(load "my-files.el")

;; These customizations make editing a bit nicer.
(load "my-editing.el")
(load "my-movement.el")

(load "my-languages.el")

(load "my-keybindings.el")

;; Hard-to-categorize customizations
(load "my-misc.el")

;; This is for private stuff. Never publish.
(load "my-private.el")
