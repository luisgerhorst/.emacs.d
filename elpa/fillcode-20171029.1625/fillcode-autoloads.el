;;; fillcode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fillcode" "fillcode.el" (0 0 0 0))
;;; Generated autoloads from fillcode.el

(autoload 'fillcode-mode "fillcode" "\
Toggle fillcode mode.
With no argument, this command toggles the mode. Non-null prefix argument
turns on the mode. Null prefix argument turns off the mode.

Fillcode mode can intelligently fill some parts of source code, like function
calls and definitions, in many languages.

To see what version of fillcode you are running, enter `\\[fillcode-version]'.

For more information, see https://snarfed.org/fillcode

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fillcode" '("fillcode" "build-re")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fillcode-autoloads.el ends here
