;; Bookmark on startup.
(setq initial-buffer-choice
	  (lambda ()
		(bookmark-bmenu-list)
        (get-buffer "*Bookmark List*")))

(setq load-prefer-newer t)

(setq debug-on-error t)

;; Restore Emacs session (e.g. open buffers) on startup.
(desktop-save-mode 1)


(provide 'luis-misc)
