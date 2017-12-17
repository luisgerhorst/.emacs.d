;; Bookmark on startup.
(setq initial-buffer-choice
	  (lambda ()
		(bookmark-bmenu-list)
        (get-buffer "*Bookmark List*")))

;; We use the obsolete frame-restore sice the newer desktop-save does save a
;; wrong mouse-color on macOS so the mouse looks weird in Emacs.
(frame-restore)

(provide 'luis-misc)
