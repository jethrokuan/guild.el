(defun guild--pop-to-buffer (name)
  (unless (get-buffer name)
    (get-buffer-create name))
  (pop-to-buffer name))

(provide 'guild-utils)
