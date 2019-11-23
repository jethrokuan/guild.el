(require 'dash)

(defvar guild-runs-format [("id" 16 t) ("op" 30 t) ("label" 50 t) ("status" 16 t) ("timestamp" 20 t)]
  "Guild Run tabular list default columns.")

(defun guild-runs-refresh ()
  "Refresh the guild runs list."
  (interactive)
  (setq tabulated-list-entries (guild-run-entries)))

(defun guild-run-entries ()
  (let* ((guild-runs-json (shell-command-to-string "guild runs --json"))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-from-string guild-runs-json)))
    (-map #'guild-run-parse-entries json)))

(defun guild-run-parse-entries (entry)
  (let* ((id (gethash "id" entry))
         (op (guild-run-parse-opref (gethash "opref" entry)))
         (status (gethash "status" entry))
         (label (or (gethash "label" entry) ""))
         (timestamp (guild-run-parse-timestamp (gethash "started" entry)))
         )
    `(nil [,id ,op ,label ,status ,timestamp])))

(defun guild-run-parse-timestamp (timestamp)
  (let* ((ts (/ timestamp 1000000))
         (output-format "%Y-%m-%dT%T"))
    (format-time-string output-format (seconds-to-time ts))))

(defun guild-run-parse-opref (opref)
  (string-join (-take-last 2 (split-string opref)) ":"))

(define-derived-mode guild-runs-mode tabulated-list-mode "Guild Runs"
  "Major mode for handling guild runs."
  (setq tabulated-list-format guild-runs-format)
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'guild-runs-refresh nil t)
  (tabulated-list-init-header))

(defun guild-runs ()
  (interactive)
  (pop-to-buffer "*guild-runs*" nil)
  (guild-runs-mode)
  (tabulated-list-print t))

(provide 'guild-runs)
