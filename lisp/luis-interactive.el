;;; IDO

;; Configure Standard IDO
(progn
  (setq ido-use-filename-at-point nil)
  (setq ido-enable-flex-matching t)
  ;; Disable automatic switch to different directory when no matches are found.
  (setq ido-auto-merge-work-directories-length -1)

  (ido-mode 1)
  (ido-everywhere 1)

  ;; Allow typing filesnames that contain spaces.
  (define-key ido-common-completion-map (kbd "SPC") nil))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

;; https://github.com/DarwinAwardWinner/ido-ubiquitous/tree/4cc53187bf37fedabbc5b6dea41e041c80982552#ido-for-describe-face-and-certain-other-commands
(use-package crm-custom
  :config
  (crm-custom-mode 1))

(use-package ido-vertical-mode
  :config
  ;; Remove brackets around completions common part.
  (setcar (nthcdr 4 ido-vertical-decorations) "")
  (setcar (nthcdr 5 ido-vertical-decorations) "")

  (ido-vertical-mode 1))

(progn
  ;; Highligh the common part of ido completions using a special face and not
  ;; using brackets. See previous block for code to remote brackets.

  (defun luis-propertize-ido-common-match-string (&rest _)
    (when ido-common-match-string
      (setq ido-common-match-string
            (propertize ido-common-match-string
                        'face 'completions-common-part))))

  (defun luis-unpropertize-ido-common-match-string (&rest _)
    (when ido-common-match-string
      (setq ido-common-match-string
            (propertize ido-common-match-string
                        'face nil))))

  (advice-add 'ido-completions :before
              #'luis-propertize-ido-common-match-string)
  (advice-add 'ido-completions :after
              #'luis-unpropertize-ido-common-match-string))


;; For any case where ido cannot be used, there is another older mode called
;; icomplete-mode that integrates with standard emacs completion and adds some
;; ido-like behavior.
(use-package icomplete
  :config
  (icomplete-mode 1))

;;; Buffers

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;;; Discover Emacs commands and shortcuts with popup buffers.

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(use-package discover
  :config
  (global-discover-mode 1))

(use-package discover-my-major
  :after discover
  :bind ("C-h C-m" . discover-my-major))


(provide 'luis-interactive)
