;; Bookmark on startup.
(setq initial-buffer-choice
	  (lambda ()
		(bookmark-bmenu-list)
        (get-buffer "*Bookmark List*")))

;; We use the obsolete frame-restore sice the newer desktop-save does save a
;; wrong mouse-color on macOS so the mouse looks weird in Emacs. Description
;; here
;; https://emacs.stackexchange.com/questions/29124/how-do-i-keep-emacs-from-saving-the-mouse-color-in-the-desktop-file
(frame-restore)

(provide 'luis-misc)
