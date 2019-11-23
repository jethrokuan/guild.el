(require 'dash)
(require 'guild-utils)

(defvar guild-runs-format [("id" 16 t) ("op" 30 t) ("label" 50 t) ("status" 16 t) ("timestamp" 20 t)]
  "Guild Run tabular list default columns.")

(defun guild-runs-refresh ()
  "Refresh the guild runs list."
  (interactive)
  (let ((default-directory guild-directory))
    (setq tabulated-list-entries (guild-run-entries))))

(defun guild-run-entries (&optional directory)
  (unless directory
    (setq directory default-directory))
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
         (contents (vector id op label status timestamp)))
    (list id contents)))

(defun guild-run-parse-timestamp (timestamp)
  (let* ((ts (/ timestamp 1000000))
         (output-format "%Y-%m-%dT%T"))
    (format-time-string output-format (seconds-to-time ts))))

(defun guild-run-parse-opref (opref)
  (string-join (-take-last 2 (split-string opref)) ":"))

(defun guild--get-run-under-cursor ()
  (string-trim (aref (tabulated-list-get-entry) 0)))

(defun guild-runs--run-details ()
  "Get the details of the run under cursor."
  (interactive)
  (let* ((run (guild--get-run-under-cursor))
         (buffer-name (format "*guild-run - run - %s*" run)))
    (guild--exec buffer-name nil (list "runs" "info" run))))

(defun guild--exec (buffer-name async args)
  "Executes commands, and recreates buffer where necessary."
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name))
  (if async
      (apply #'start-process buffer-name buffer-name "guild" (append '() args))
    (apply #'call-process "guild" nil buffer-name nil (append '() args)))
  (guild--pop-to-buffer buffer-name))

(defun guild-runs--tensorboard ()
  "Starts a process for tensorboard."
  (interactive)
  (let* ((run (guild--get-run-under-cursor))
         (buffer-name (format "*guild-tensorboard - %s*" run)))
    (guild--exec buffer-name t (list "tensorboard" run))))

(define-transient-command guild-runs--help-popup ()
  "Guild Runs Menu"
  ["Actions"
   ("ENTER" "Run details" guild-runs--run-details)
   ("t" "Tensorboard" guild-runs--tensorboard)
   ])

(setq guild-runs-mode-map (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") 'guild-runs--run-details)
                            (define-key map (kbd "t") 'guild-runs--tensorboard)
                            (define-key map (kbd "?") 'guild-runs--help-popup)
                            map))

(defvar-local guild-directory nil "Directory for guild run.")

(defvar guild-runs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'guild-runs--run-details)
    (define-key map (kbd "?") 'guild-runs--help-popup)
    map)
  "Keymap for guild-runs-mode.")

(define-derived-mode guild-runs-mode tabulated-list-mode "Guild-Runs"
  "Major mode for handling guild runs."
  (setq tabulated-list-format guild-runs-format)
  (setq tabulated-list-padding 2)
  (use-local-map guild-runs-mode-map)
  (setq tabulated-list-entries #'guild-run-entries)
  (setq guild-directory default-directory)
  (add-hook 'tabulated-list-revert-hook 'guild-runs-refresh nil t)
  (tabulated-list-init-header))

(defun guild-runs ()
  (interactive)
  (let ((buffer-name (format "*guild-runs : %s*" default-directory)))
    (guild--pop-to-buffer buffer-name)
    (guild-runs-mode)
    (tabulated-list-print t)))


(provide 'guild-runs)
