;; Bookmark on startup.
(setq initial-buffer-choice
	  (lambda ()
		(bookmark-bmenu-list)
		(get-buffer "*Bookmark List*")))

(provide 'luis-misc)
