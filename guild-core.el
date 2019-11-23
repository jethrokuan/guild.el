(require 's)
(require 'transient)

(defun guild-run-guild (action &rest args)
  "Execute \"`guild' ACTION ARGS\"."
  (let* ((command (s-join " " (-remove 's-blank? (-flatten (list guild-command (guild-arguments) action args))))))
    (message command)
    (docker-utils-shell-command-to-string command))
  (docker-utils-with-sudo
    ))

;;;###autoload (autoload 'guild "guild" nil t)
(define-transient-command guild ()
  "Transient for guild."
  :man-page "guild"
  ["Guild"
   ("r" "runs" guild-runs)])

(provide 'guild-core)
