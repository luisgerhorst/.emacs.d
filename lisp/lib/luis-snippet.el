(require-package 'yasnippet)
(require 'yasnippet)

(setq yas-snippet-dirs
      `(,(expand-file-name "snippets" user-emacs-directory)))

(yas-global-mode 1)

(provide 'luis-snippet)
