;; -*- lexical-binding: t; -*-

;;; Compilation

(setq compilation-ask-about-save nil)
(global-set-key (kbd "C-c c") #'compile)

;;; Language Server Protocol

;; To enable this in a specific language, add
;;
;; (use-package <lang>-mode
;;   :config
;;   (add-hook '<lang>-mode-hook #'lsp))
;;
;; to 'lib/luis-<lang>.el'.
(use-package lsp-mode
  :defer t)

(use-package company-lsp
  :defer t)

;;; Completion

(defun luis-company-configure-completion (idle-delay minimum-prefix-length)
  (setq-local company-idle-delay idle-delay)
  (setq-local company-minimum-prefix-length minimum-prefix-length))

(defun luis-company-configure-automatic-completion ()
  (interactive)
  (luis-company-configure-completion 0.5 0))

;; Used to only enable certain backends in a buffer to avoid possibly
;; annoying completions while for example writing comments.
(defun luis-set-local-company-backends (local-company-backends)
  (if (company-safe-backends-p local-company-backends)
      (setq-local company-backends local-company-backends)
    (message (concat "Warning: '%S did not fullfill "
                     "company-safe-backends-p predicate. "
                     "Automatic completion was disabled in this buffer.")
             local-company-backends)
    ;; Disable automatic completion locally.
    (setq-local company-idle-delay nil)))

(use-package company
  :demand
  :diminish company-mode
  :bind (("C-M-i" . company-complete))
  :config
  (setq company-frontends '(company-preview-common-frontend
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend))
  (setq company-backends '((company-elisp :with company-dabbrev-code)
                           company-lsp
                           (company-dabbrev-code company-keywords)))

  ;; Initially, disable automatic completions but enable company mode
  ;; globally. Use the global shortcut for `company-complete' to get
  ;; completions.
  (setq company-idle-delay nil)
  (global-company-mode 1))

;;; Syntax Checking

(setq flymake-gui-warnings-enabled nil
      flymake-log-level 0)

(when (version< "26.1" emacs-version)
  (use-package flymake
    :defer t
    :bind (:map
           flymake-mode-map
           ("C-c n" . flymake-goto-next-error))))

(use-package flycheck
  :commands (flycheck-mode)
  :bind-keymap ("C-c !" . flycheck-command-map)
  :bind (:map
         flycheck-mode-map
         ;; We could also use 'C-c ! ...' but this way flymake and flycheck have
         ;; the same interface.
         ("C-c n" . flycheck-next-error))
  :init
  ;; Prevent multi-line error messages popping up in the mode line from
  ;; distracting us.
  (setq flycheck-display-errors-delay 60))

(defun luis-flycheck-unless-file-remote ()
  (let ((current-file (buffer-file-name (current-buffer))))
    (unless (and current-file (file-remote-p current-file))
      (flycheck-mode 1))))

;;; Misc

(setq load-prefer-newer t)

(provide 'luis-ide)
