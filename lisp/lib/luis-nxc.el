(defun luis-nxc-mode ()
  (c-mode)
  (setq-local c-basic-offset tab-width)
  (luis-comment-auto-fill))

(add-to-list 'auto-mode-alist '("\\.nxc\\'" . luis-nxc-mode))


(provide 'luis-nxc)
