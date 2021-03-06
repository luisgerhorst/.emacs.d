;; -*- lexical-binding: t; -*-

;;;; Settings for interacting with the file system

(use-package autorevert
  :demand
  :diminish (auto-revert-mode . "")
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))

;; Files automatically created by Emacs.
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups")))
      auto-save-default nil
      create-lockfiles nil)

;; Dired config

(when (eq system-type 'darwin)
  ;; Use ls from GNU coreutils for dired when on macOS. Install them via brew.
  (setq insert-directory-program (executable-find "gls")))

;; End Dired config

(use-package recentf
  :demand
  :config
  (setq recentf-max-menu-items 200
        recentf-max-saved-items 200)
  (recentf-mode 1)
  (setq recentf-exclude
        (append (list "/.emacs.d/bookmarks\\'"
                      "/.emacs.d/ido.last\\'"
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

(global-set-key (kbd "C-c s .") #'find-grep)
(use-package ag
  :if (executable-find "ag")
  :bind ("C-c s ." . ag))
(use-package ripgrep
  :if (executable-find "rg")
  :bind ("C-c s ." . ripgrep-regexp))

(use-package projectile
  :demand
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)

  ;; grep dwyc (do what you can)
  (global-set-key (kbd "C-c s p") #'projectile-grep)
  (when (executable-find "ag")
    (global-set-key (kbd "C-c s p") #'projectile-ag))
  (when (executable-find "rg")
    (global-set-key (kbd "C-c s p") #'projectile-ripgrep)))

;; Offer `revert-buffer' when asking to save changes.
(add-to-list 'save-some-buffers-action-alist
             `(?r
               ,(lambda (buf)
                  (with-current-buffer buf
                    (revert-buffer nil t nil)))
               ,(purecopy "Revert Buffer")))


(provide 'luis-files)
