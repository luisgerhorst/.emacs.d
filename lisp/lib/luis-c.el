;; -*- lexical-binding: t -*-

(use-package cc-mode
  :defer t
  :config
  ;; C
  (setq c-basic-offset tab-width)

  ;; Set default indentation style to K&R instead of weird GNU style. See
  ;; https://www.emacswiki.org/emacs/IndentingC#toc2
  (unless (listp c-default-style)
    (setq c-default-style nil))
  (add-to-list 'c-default-style '(other . "k&r"))

  (add-hook 'c-mode-hook #'luis-company-configure-automatic-completion)
  (add-hook 'c-mode-hook #'luis-flycheck-unless-file-remote)

  ;; C++
  (add-hook 'c++-mode-hook #'luis-company-configure-automatic-completion)

  ;; Linux Kernel
  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
              (arglist-cont-nonempty
               c-lineup-gcc-asm-reg
               c-lineup-arglist-tabs-only)))))

(use-package xcscope
  :defer t
  :init
  (setq cscope-keymap-prefix (kbd "C-c o"))
  (add-hook 'c-mode-hook #'cscope-minor-mode)
  (add-hook 'c++-mode-hook #'cscope-minor-mode)
  (add-hook 'dired-mode-hook #'cscope-minor-mode))

(defun luis-irony-if-installed-unless-file-remote ()
  (let ((current-file (buffer-file-name (current-buffer))))
    (when (and
           ;; File remote?
           (not (and current-file (file-remote-p current-file)))
           ;; Memoized: Installed?
           (condition-case nil
               (irony--find-server-executable)
             (irony-server-error nil)))
      (irony-mode 1))))

(use-package irony
  :defer t
  :init
  (add-hook 'c-mode-hook #'luis-irony-if-installed-unless-file-remote)
  (add-hook 'c++-mode-hook #'luis-irony-if-installed-unless-file-remote)
  (add-hook 'objc-mode-hook #'luis-irony-if-installed-unless-file-remote))


;;; Completion

(use-package company-irony
  :defer t
  :init
  (with-eval-after-load 'irony
    (require 'company)
    (add-to-list 'company-backends #'company-irony)
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))

(use-package company-c-headers
  :defer t
  :init
  (with-eval-after-load 'company-irony
    ;; Ensure this is executed after `company-irony' is added to
    ;; `company-backends', it must appear in the list first.
    (add-to-list 'company-backends #'company-c-headers)))


;;; Syntax Checking

(use-package flycheck-irony
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))


(provide 'luis-c)
