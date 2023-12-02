;;; fstore --- Emacs major mode for editing .fstore files
;;;
;;; Author: Ranjeeth Mahankali <ranjeethmahankali@gmail.com>
;;;
;;; Commentary:
;;; It has several convenience features:
;;; - Auto completion for tags.
;;; - Audo completion for file paths of untracked files.
;;; - Ability to preview files at point.
;;; - Follow-mode, which always shows a preview of the file
;;;   at point as you move around the buffer.
;;;
;;; Code:

(defun fstore-looking-at (regexp n pos)
  "Check if POS is within the N th match of REGEXP."
  (and (looking-at regexp)
       (>= pos (match-beginning n))
       (<= pos (match-end n))))

(defun fstore-untracked-files (dir)
  "Get the untracked files in the DIR directory."
  (let ((default-directory dir))
    (split-string
     ;; Remove newlines and white spaces before splitting.
     (string-trim (shell-command-to-string "fstore untracked")) "\n")))

(defun fstore-tags (dir)
  "Get all tags for documents in the DIR directory."
  (let ((default-directory dir))
    (split-string
     ;; Remove newlines and white spaces before splitting.
     (string-trim (shell-command-to-string "fstore tags")) "\n")))

(defun fstore-under-header (name)
  "Check if the point is directly under the NAME header."
  (and (save-excursion
         (let ((pattern "^\\[\\(.*\\)\\]"))
           (and (re-search-backward pattern nil t)
                (progn (beginning-of-line) (looking-at pattern))
                (string-equal name (buffer-substring (match-beginning 1) (match-end 1))))))))

(defun fstore-completion-at-point ()
  "Return the completion data relevant for the text at point.
This completes filepaths with untracked files, and tags with known tags."
  (save-excursion
    (save-match-data
      (let ((pos (point))
            (dirpath (file-name-directory (buffer-file-name))))
        (cond
         ;; Untracked file paths
         ((fstore-under-header "path")
          (list (progn (beginning-of-line) (point)) (progn (end-of-line) (point))
                (fstore-untracked-files dirpath)))
         ;; Tags
         ((fstore-under-header "tags")
          (when (progn (backward-word) (fstore-looking-at "\\(\\<.*\\>\\)" 1 pos))
            (list (match-beginning 1) (match-end 1)
                  (fstore-tags dirpath)))))))))

(defun fstore-file-at-point ()
  "Get the path of the file at the current point."
  (save-excursion
    (save-match-data
      (let ((pattern-1 "^\\[path\\]")
            (pattern-2 "^\\[path\\][\n\s]*\\(.*\\)\n"))
        (beginning-of-line)
        (when (or (looking-at pattern-2)
                  (and (re-search-backward pattern-1 nil t)
                       (looking-at pattern-2)))
          (string-trim (buffer-substring (match-beginning 1) (match-end 1))))))))

(defvar fstore-last-previewed-file nil
  "Last file previewed.")

(defvar fstore-async-preview-thread nil
  "The thread on which async file previews are done for performance.")

(defun fstore-preview-file ()
  "Preview the file at point in another window."
  (interactive)
  (let* ((filepath (fstore-file-at-point))
         (abbrev-filepath (abbreviate-file-name (or filepath "")))
         (last-buffer (when fstore-last-previewed-file
                        (get-file-buffer fstore-last-previewed-file))))
    (message "Previewing file: %s" filepath)
    (cond
     ;; Bail out if there is no filepath.
     ((not filepath) nil)
     ;; Bail out if a preview for the same file is already queued.
     ((and fstore-async-preview-thread
           (thread-live-p fstore-async-preview-thread)
           (equal fstore-last-previewed-file abbrev-filepath))
      nil)
     ;; Check if the preview buffer exits.
     ((and last-buffer (equal abbrev-filepath fstore-last-previewed-file))
      (unless (get-buffer-window last-buffer) ;; Only display it if it isn't already visible.
        (display-buffer last-buffer)))
     ;; Preview the file if the preview buffer doesn't already exist.
     ((and filepath (file-exists-p filepath))
      (progn
        ;; If a file has been previewed already and its buffer exists, kill it.
        (when (and fstore-last-previewed-file last-buffer)
          (kill-buffer last-buffer))
        ;; Set the file we're about to preview as the last previewed.
        (setq fstore-last-previewed-file abbrev-filepath)
        ;; Wait for previously queued up previews to finish.
        (when fstore-async-preview-thread
          (thread-join fstore-async-preview-thread))
        (setq fstore-async-preview-thread ;; Queue up the preview in a different thread.
              (make-thread `(lambda ()
                              (display-buffer (find-file-noselect ,filepath))))))))))

(defun fstore-track-untracked-files ()
  "Track all untracked files by inserting them into the current .fstore file."
  (interactive)
  ;; Insertion should happen at the end of the file.
  (save-excursion
    (goto-char (point-max))
    (forward-line)
    (let* ((dirpath (file-name-directory (buffer-file-name)))
           (untracked-files (sort (fstore-untracked-files dirpath) 'string-lessp))
           (text (string-join
                  (mapcar (lambda (file) (concat "[path]\n" file "\n")) untracked-files)
                  "\n")))
      (insert text))))

(defvar fstore-mode-name "fstore"
  "The name of the fstore-mode shown in the mode line.")

(defvar fstore-follow-mode-enabled nil
  "Indicates whether follow mode is active.")

(defun fstore-set-mode-name ()
  "Update the `mode-name' and mode line."
  (let ((new-name
         (concat fstore-mode-name
                 (if fstore-follow-mode-enabled "-follow" ""))))
    (setq mode-name new-name)
    (force-mode-line-update)))

(defvar current-line-number nil
  "Tracks the line number of the point.
This is useful in follow mode.")

(defvar fstore-line-changed-hook nil
  "This hook is run when the cursor changes line.")

(defun fstore-update-line-number ()
  "Update the line number if needed and run the `fstore-line-changed-hook'."
  (let ((new-line-num (line-number-at-pos)))
    (when (not (equal new-line-num current-line-number))
      (setq current-line-number new-line-num)
      (run-hooks 'fstore-line-changed-hook))))

(add-hook 'post-command-hook #'fstore-update-line-number)

(defun fstore-follow-mode ()
  "Toggle follow mode."
  (interactive)
  (setq fstore-follow-mode-enabled (not fstore-follow-mode-enabled))
  (if fstore-follow-mode-enabled
      (progn
        (add-hook 'fstore-line-changed-hook #'fstore-preview-file nil t)
        (message "Follow mode is enabled."))
    (progn
      (remove-hook 'fstore-line-changed-hook #'fstore-preview-file t)
      (message "Follow mode is disabled.")))
  (fstore-set-mode-name))

;;;###autoload
(defun fstore-query (dirpath query)
  "QUERY fstore at DIRPATH and output the paths to resulting files."
  (let* ((default-directory (file-name-as-directory dirpath)))
    (mapcar (lambda (file)
              (concat default-directory file))
            (seq-filter
             (lambda (str) (not (string-equal "" str)))
             (split-string (string-trim
                            (shell-command-to-string
                             (format "fstore -q \"%s\"" query)))
                           "\n")))))

(defvar fstore-mode-map-prefix [(control c)]
  "The prefix key used to bind fstore commands in Emacs.")

(defvar fstore-mode-map
  (let ((map (make-sparse-keymap))
        (p fstore-mode-map-prefix))
    (define-key map (vconcat p [(control c)]) #'fstore-preview-file)
    (define-key map (vconcat p [(control f)]) #'fstore-follow-mode)
    map)
  "Keymap used in `fstore-mode' buffers.")

(defvar fstore-font-lock-keywords
  '(("^\\[.*\\]" . font-lock-builtin-face)))

;;;###autoload
(define-derived-mode fstore-mode text-mode fstore-mode-name
  "Simple mode for editing .fstore files.
\\{fstore-mode-map}"
  (add-hook 'completion-at-point-functions #'fstore-completion-at-point 0 t)
  (setq font-lock-defaults '(fstore-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fstore$" . fstore-mode))

(add-hook 'fstore-mode-hook 'company-mode)

(provide 'fstore)
;;; fstore.el ends here
