(defun my/latex-mode-hook ()
  (setq buffer-face-mode-face '(:family "Input Serif"))
  (buffer-face-mode))
(add-hook 'latex-mode-hook 'my/latex-mode-hook)
