;;;; Settings for interacting with the file system

;; For some reason my Emacs 24.5.1 installed via Homebrew --with-cocoa
;; throws function not defined errors for tramp-tramp-file-p.
;; - 2016-02-13 ~Luis
(require 'tramp)

(use-package autorevert
  :demand
  :diminish (auto-revert-mode . "")
  :config
  (global-auto-revert-mode))

;; Files automatically created by Emacs.
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups")))
      auto-save-default nil
      create-lockfiles nil)

;; Dired config

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(setq dired-recursive-deletes 'always
      delete-by-moving-to-trash t
      ;; Use ls from GNU coreutils for dired.
      insert-directory-program (executable-find "gls"))

;; To speed moving to trash up on macOS, you can do
;; $ brew install trash
;; - otherwise AppleScript is used.
(use-package osx-trash
  :if (eq system-type 'darwin)
  :commands (osx-trash-move-file-to-trash)
  :init
  ;; Don't use provided `osx-trash-setup' since it does not allow autoloading the
  ;; function.
  (defalias 'system-move-file-to-trash
    'osx-trash-move-file-to-trash))

;; End Dired config

(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-c f d") 'delete-current-buffer-file)
(global-set-key (kbd "C-c f r") 'rename-current-buffer-file)

(use-package recentf
  :demand
  :config
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 100)
  (recentf-mode 1)
  (setq recentf-exclude
        (append (list (regexp-opt '("/Users/luis/.emacs.d/bookmarks"
                                    "/Users/luis/.emacs.d/ido.last"))
                      "/TAGS\\'")
                recentf-exclude)))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read
               "Choose recent file: "
               (mapcar 'abbreviate-file-name recentf-list)
               nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c e") #'recentf-ido-find-file)

(use-package projectile
  :demand
  :config
  (projectile-mode))


(provide 'luis-files)
