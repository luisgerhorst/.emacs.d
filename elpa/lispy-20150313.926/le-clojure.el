;;; le-clojure.el --- lispy support for Clojure.

;; Copyright (C) 2014-2015 Oleh Krehel

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(declare-function nrepl-sync-request:eval "nrepl-client")
(declare-function nrepl-dict-get "nrepl-client")
(declare-function nrepl-send-sync-request "nrepl-client")
(declare-function nrepl-current-session "nrepl-client")
(declare-function cider-current-ns "cider-interaction")
(declare-function cider-find-file "cider-interaction")

(defvar lispy-do-pprint)

(defun lispy--eval-clojure (str &optional add-output)
  "Eval STR as Clojure code.
The result is a string.

When ADD-OUTPUT is t, add the standard output to the result."
  (require 'cider)
  (let* ((str
          (if lispy-do-pprint
              (format "(clojure.core/let [x %s] (with-out-str (clojure.pprint/pprint x)))"
                      str)
            str))
         (res (nrepl-sync-request:eval str))
         (val (nrepl-dict-get res "value"))
         out)
    (cond ((null val)
           (error "eval error: %S"
                  (nrepl-dict-get res "err")))

          (add-output
           (if (setq out (nrepl-dict-get res "out"))
               (format "%s\n%s"
                       (propertize
                        out 'face 'font-lock-string-face)
                       val)
             val))

          (lispy-do-pprint
           (read res))

          (t
           val))))

(defun lispy--clojure-resolve (symbol)
  "Return resolved SYMBOL.
Return 'special or 'keyword appropriately.
Otherwise try to resolve in current namespace first.
If it doesn't work, try to resolve in all available namespaces."
  (let ((str (lispy--eval-clojure
              (format
               "(if (symbol? '%s)
                   (if (special-symbol? '%s)
                       'special
                     (or (resolve '%s)
                         (first (keep #(ns-resolve %% '%s) (all-ns)))
                         (if-let [val (try (load-string \"%s\") (catch Exception e))]
                                 (list 'variable (str val)))))
                 (if (keyword? '%s)
                     'keyword
                   'unknown))"
               symbol
               symbol
               symbol
               symbol
               symbol
               symbol))))
    (if (string-match "^#'\\(.*\\)$" str)
        (match-string 1 str)
      (read str))))

(defun lispy--clojure-args (symbol)
  "Return a pretty string with arguments for SYMBOL.
Besides functions, handles specials, keywords, maps, vectors and sets."
  (let* ((sym (lispy--clojure-resolve symbol))
         (args (cond
                 ((eq sym 'special)
                  (read
                   (lispy--eval-clojure
                    (format
                     "(->> (with-out-str (clojure.repl/doc %s))
                       (re-find #\"\\(.*\\)\")
                       read-string rest
                       (map str)
                       (clojure.string/join \" \")
                       (format \"[%%s]\")
                       list)"
                     symbol))))
                 ((eq sym 'keyword)
                  (list "[map]"))
                 ((eq sym 'undefined)
                  (error "Undefined"))
                 ((and (listp sym) (eq (car sym) 'variable))
                  (list "variable"))
                 ((null sym)
                  (read
                   (lispy--eval-clojure
                    (format
                     "(let [[_ cname mname] (re-find #\"(.*)/(.*)\" \"%s\")
                           methods (and cname
                                     (try (load-string (format \"(.getMethods %%s)\" cname))
                                          (catch Exception e)))
                           methods (filter #(= (.getName %%) mname) methods)]
                       (if (= 0 (count methods))
                           \"method not found\"
                         (map (fn [m]
                                  (->> m
                                    .getParameterTypes
                                    (map #(.toString %%))
                                    (clojure.string/join \" \")))
                              (filter #(java.lang.reflect.Modifier/isStatic
                                        (.getModifiers %%))
                                      methods))))"
                     symbol))))
                 (t
                  (read (lispy--eval-clojure
                         (format
                          "(let [args (map str (:arglists (meta #'%s)))]
                            (if (empty? args)
                                (eval '(list
                                        (condp #(%%1 %%2) %s
                                         map? \"[key]\"
                                         set? \"[key]\"
                                         vector? \"[idx]\"
                                         \"is uncallable\")))
                              args))"
                          sym
                          sym)))))))
    (if (listp args)
        (format
         "(%s %s)"
         (propertize symbol 'face 'lispy-face-hint)
         (mapconcat
          #'identity
          (mapcar (lambda (x) (propertize (downcase x)
                                          'face 'lispy-face-req-nosel))
                  args)
          (concat "\n"
                  (make-string (+ 2 (length symbol)) ?\ ))))
      (propertize args 'face 'lispy-face-hint))))

(defun lispy--describe-clojure-java (sym)
  "Return description for Clojure Java symol SYM."
  (read
   (lispy--eval-clojure
    (format
     "(let [[_ cname mname] (re-find #\"(.*)/(.*)\" \"%s\")
          methods (and cname
                    (try (load-string (format \"(.getMethods %%s)\" cname))
                         (catch Exception e)))
          methods (filter #(= (.getName %%) mname) methods)]
      (if (= 0 (count methods))
          nil
        (clojure.string/join
         \"\\n\" (map (fn [m] (.toString m))
                   methods))))"
     sym))))

(defun lispy--clojure-jump (symbol)
  "Jump to Clojure SYMBOL."
  (let* ((dict (nrepl-send-sync-request
                (list
                 "op" "info"
                 "session" (nrepl-current-session)
                 "ns" (cider-current-ns)
                 "symbol" symbol)))
         (file (nrepl-dict-get dict "file"))
         (line (nrepl-dict-get dict "line"))
         (col (nrepl-dict-get dict "column")))
    (switch-to-buffer (cider-find-file file))
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char (1- col))))

(provide 'le-clojure)

;;; le-clojure.el ends here
