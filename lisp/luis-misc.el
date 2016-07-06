;; Bookmark on startup.
(setq initial-buffer-choice
	  (lambda ()
		(bookmark-bmenu-list)
        (get-buffer "*Bookmark List*")))

(setq load-prefer-newer t)


(provide 'luis-misc)
