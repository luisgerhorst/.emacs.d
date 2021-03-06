;; -*- lexical-binding: t; -*-

;; Scroll in smaller steps.
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))

;; Always just ask for y/n, even when yes/no would be required.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable Mouse
;; (dolist (mouse '([mouse-1] [down-mouse-1] [drag-mouse-1]
;;                  [double-mouse-1] [triple-mouse-1] [mouse-2]
;;                  [down-mouse-2] [drag-mouse-2] [double-mouse-2]
;;                  [triple-mouse-2] [mouse-3] [down-mouse-3]
;;                  [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
;;                  [mouse-4] [down-mouse-4] [drag-mouse-4]
;;                  [double-mouse-4] [triple-mouse-4] [mouse-5]
;;                  [down-mouse-5] [drag-mouse-5] [double-mouse-5]
;;                  [triple-mouse-5]))
;;   (global-unset-key mouse))

;; Because I regulary mistype C-x b as C-x C-b.
(global-set-key (kbd "C-x C-b") #'ido-switch-buffer)

(provide 'luis-user-interaction)
