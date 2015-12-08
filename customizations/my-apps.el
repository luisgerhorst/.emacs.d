;; Standalone features / tools installation and keybindings.

;; laucher-map defined in init.el because it may be used in multiple
;; config files.

;; Git interface.
(require-package 'magit)
(define-key launcher-map (kbd "g") 'magit-status)

;; Fancier list-packages.
(require-package 'paradox)
(define-key launcher-map (kbd "p") 'paradox-list-packages)

(define-key launcher-map (kbd "c") #'calc)

;; Shell
(define-key launcher-map (kbd "s") #'ansi-term)

(require-package 'sx)
;; Ordered by frequency of use, for no particular reason.
(define-key launcher-map (kbd "q q") #'sx-tab-all-questions)
(define-key launcher-map (kbd "q i") #'sx-inbox)
(define-key launcher-map (kbd "q o") #'sx-open-link)
(define-key launcher-map (kbd "q u") #'sx-tab-unanswered-my-tags)
(define-key launcher-map (kbd "q a") #'sx-ask)
(define-key launcher-map (kbd "q s") #'sx-search)

;; Because proced does not work on OS X.
(require-package 'vkill)
(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

(defun test-emacs-config ()
  "Start shell Emacs in background to test config."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"(condition-case e (progn (load \\\"~/.emacs.d/init.el\\\") (message \\\"-OK-\\\")) (error (message \\\"ERROR!\\\") (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "Emacs config ok"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))
