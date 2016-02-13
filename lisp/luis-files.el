;;;; Settings for interacting with the file system.

;;; Tramp

;; For some reason my Emacs 24.5.1 installed via Homebrew --with-cocoa
;; throws function not defined errors for tramp-tramp-file-p.
;; - 2016-02-13 ~Luis
(require 'tramp)

;;; Files created by Emacs.

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Prevent asking to recover changed file when changes were discarded.
(setq auto-save-default nil)

;; No need for ~ files when editing.
(setq create-lockfiles nil)

;;; Easy actions on current file.

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
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

(global-set-key (kbd "C-c f d") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-c f r") 'rename-current-buffer-file)

;;; Dired

;; Cleaner dired.
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

;; Don't ask when deleting directory.
(setq dired-recursive-deletes 'always)

;; Use ls from GNU coreutils for dired.
(setq insert-directory-program (executable-find "gls"))

;;; Auto saving

(require-package 'super-save)
(require 'super-save)
(super-save-mode 1)
(setq auto-save-default nil)


(provide 'luis-files)
