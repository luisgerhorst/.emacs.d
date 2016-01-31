(defun my/nxc-mode ()
  (c-mode)
  (setq-local c-basic-offset tab-width)
  (my/comment-auto-fill))

(add-to-list 'auto-mode-alist '("\\.nxc\\'" . my/nxc-mode))
