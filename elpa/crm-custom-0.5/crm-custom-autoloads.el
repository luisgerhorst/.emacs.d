;;; crm-custom-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "crm-custom" "crm-custom.el" (0 0 0 0))
;;; Generated autoloads from crm-custom.el

(defvar crm-custom-mode nil "\
Non-nil if Crm-Custom mode is enabled.
See the `crm-custom-mode' command
for a description of this minor mode.")

(custom-autoload 'crm-custom-mode "crm-custom" nil)

(autoload 'crm-custom-mode "crm-custom" "\
Use `completing-read-function' in `completing-read-multiple'.

When this mode is enabled, `completing-read-multiple' will work
by calling `completing-read' repeatedly until it receives an
empty string, and returning all the values read in this way. This
is useful because it will use `completing-read-function' to do
completion, so packages like `ido-ubiquitous' that offer an
alternative completion system will now work in
`completing-read-multiple'. (Remember that in ido, you must enter
an empty string by pressing `C-j', since RET simply selects the
first completion.)

Note that `crm-separator' is purely cosmetic when this mode is
enabled. It cannot actually be used as a separator.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "crm-custom" '("crm-custom-separator")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; crm-custom-autoloads.el ends here
