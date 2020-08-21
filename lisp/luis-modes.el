;; -*- lexical-binding: t; -*-

(require 'luis-elisp)
(require 'luis-markdown)
(require 'luis-latex)
(require 'luis-shell)
(require 'luis-swift)
(require 'luis-apache)
(require 'luis-python)
(require 'luis-c)
(require 'luis-org)
(require 'luis-asm)
(require 'luis-lua)
(require 'luis-java)
(require 'luis-scala)
(require 'luis-make)
(require 'luis-xml)
(require 'luis-rust)

;; ESS (i.e. R)
(unless (eq system-type 'darwin)
  ;; For some reason ess causes a cyclic dependency when installed using
  ;; straight.el on macOS. Error message is 'Symbol’s chain of function
  ;; indirections contains a loop: R-mode'.
  (use-package ess
    :defer t))

;; Haskell
(use-package haskell-mode
  :defer t)

;; PHP
(use-package php-mode
  :defer t)

(provide 'luis-modes)
