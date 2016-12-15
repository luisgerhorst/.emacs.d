;;;; Settings for interacting with the file system

;; Tramp

;; For some reason my Emacs 24.5.1 installed via Homebrew --with-cocoa
;; throws function not defined errors for tramp-tramp-file-p.
;; - 2016-02-13 ~Luis
(require 'tramp)

;; Files created by Emacs
(setq
 ;; Emacs can automatically create backup files. This tells Emacs to put all
 ;; backups in ~/.emacs.d/backups. More info:
 ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
 backup-directory-alist
 `(("." . ,(locate-user-emacs-file "backups")))

 ;; Prevent asking to recover changed file when changes were discarded.
 auto-save-default nil

 ;; No need for ~ files when editing.
 create-lockfiles nil

 ;; Use system trashcan.
 delete-by-moving-to-trash t)

;; Easy actions on current file.

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

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
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

;; Dired config
(progn
  ;; Cleaner dired.
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (setq
   ;; Don't ask when deleting directory.
   dired-recursive-deletes 'always
   ;; Use ls from GNU coreutils for dired.
   insert-directory-program (executable-find "gls")))

(use-package recentf
  :demand
  :bind ("C-x f" . recentf-ido-find-file)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-menu-items 100
        recentf-max-saved-items 100)

  (recentf-mode 1)

  ;; Add files to be excluded to the list passed to regexp-opt.
  (setq recentf-exclude
        (append `(,(regexp-opt '("/Users/luis/.emacs.d/bookmarks"
                                 "/Users/luis/.emacs.d/ido.last"))
                  "/TAGS\\'")
                recentf-exclude))

  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read
                 "Choose recent file: "
                 (mapcar 'abbreviate-file-name recentf-list)
                 nil t)))
      (when file
        (find-file file)))))

(use-package projectile
  :commands (projectile-mode))


(provide 'luis-files)
