;; Bookmark on startup.
(setq initial-buffer-choice
	  (lambda ()
		(bookmark-bmenu-list)
        (get-buffer "*Bookmark List*")))

(setq desktop-buffers-not-to-save ".*"
      desktop-files-not-to-save ".*"
      desktop-globals-to-save nil
      desktop-load-locked-desktop t
      desktop-save t
      desktop-auto-save-timeout nil)
(desktop-save-mode 1)

(provide 'luis-misc)
