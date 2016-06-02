;; Automatically indenting yanked text if in programming-modes.

(require-package 'aggressive-indent)
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)

(add-to-list
 'aggressive-indent-dont-indent-if
 ;; Don't indent in Java and C files when line does not contain a
 ;; semicolon or code block bracket.
 '(and (or (derived-mode-p 'jdee-mode)
           (derived-mode-p 'c-mode))
       (not (string-match "[;{}]"
                          (thing-at-point 'line)))))


(provide 'luis-indentation)
