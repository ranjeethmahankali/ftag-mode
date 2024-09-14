;;; ftag --- Emacs major mode for editing .ftag files
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

(defun ftag-looking-at (regexp n pos)
  "Check if POS is within the N th match of REGEXP."
  (and (looking-at regexp)
       (>= pos (match-beginning n))
       (<= pos (match-end n))))

(defun ftag-untracked-files (dir)
  "Get the untracked files in the DIR directory."
  (let ((default-directory dir))
    (split-string
     ;; Remove newlines and white spaces before splitting.
     (string-trim (shell-command-to-string "ftag untracked")) "\n")))

(make-variable-buffer-local (defvar ftag-tags-cache nil
  "List of tags of the parent directory."))

(defun ftag-tags (dir)
  "Get all tags for documents in the DIR directory."
  (or ftag-tags-cache
      (setq ftag-tags-cache
            (let ((default-directory dir))
              (split-string
               ;; Remove newlines and white spaces before splitting.
               (string-trim (shell-command-to-string "ftag tags")) "\n")))))

(defun ftag-invalidate-tags-cache ()
  "Set the cached list of tags to nil.  This will cause them to be queried again."
  (setq ftag-tags-cache nil))

(defun ftag-under-header (name)
  "Check if the point is directly under the NAME header."
  (save-match-data
    (save-excursion
      (let ((pattern "^\\[\\(.*\\)\\]"))
        (and (or
              (progn (beginning-of-line) (looking-at pattern))
              (re-search-backward pattern nil t))
             (string-equal name (buffer-substring (match-beginning 1) (match-end 1))))))))

(defun ftag-completion-at-point ()
  "Return the completion data relevant for the text at point.
This completes filepaths with untracked files, and tags with known tags."
  (save-excursion
    (save-match-data
      (let* ((pos (point))
             (dirpath (file-name-directory (buffer-file-name)))
             (parent-dir (or
                          (file-name-directory
                           (directory-file-name dirpath))
                          dirpath)))
        (cond
         ;; Untracked file paths
         ((ftag-under-header "path")
          (list (progn (beginning-of-line) (point)) (progn (end-of-line) (point))
                (ftag-untracked-files dirpath)))
         ;; Tags
         ((ftag-under-header "tags")
          (when (progn (backward-word) (ftag-looking-at "\\(\\<.*\\>\\)" 1 pos))
            (list (match-beginning 1) (match-end 1)
                  (ftag-tags parent-dir)))))))))

(defun ftag-file-at-point ()
  "Get the path of the file at the current point."
  (save-excursion
    (save-match-data
      (let* ((under-path (ftag-under-header "path"))
             (current-line (string-trim (thing-at-point 'line t)))
             (pattern-1 "^\\[path\\]")
             (pattern-2 "^\\[path\\][\n\s]*\\(.*\\)\n"))
        (beginning-of-line)
        (when (or (looking-at pattern-2)
                  (and (re-search-backward pattern-1 nil t)
                       (looking-at pattern-2)))
          (or
           (when (and under-path (> (length current-line) 0)
                      (not (string-equal current-line "[path]"))) ;; If the cursor is on one of the paths, preview that file.
             current-line)
           ;; Otherwise return the first path under the path header.
           (string-trim (buffer-substring (match-beginning 1) (match-end 1)))))))))

(make-variable-buffer-local
 (defvar ftag-last-previewed-file nil
   "Last file previewed."))

(defvar ftag-async-preview-thread nil
  "The thread on which async file previews are done for performance.")

(defun my/any (items)
  "Check if at least one of ITEMS is non-nil."
  (and items
       (or (car items)
           (my/any (cdr items)))))

(defun ftag-preview-file ()
  "Preview the file at point in another window."
  (interactive)
  (let* ((filepath (ftag-file-at-point))
         (abbrev-filepath (abbreviate-file-name (or filepath "")))
         (last-buffer (when ftag-last-previewed-file
                        (get-file-buffer ftag-last-previewed-file))))
    (message "Previewing file: %s" filepath)
    (cond
     ;; Bail out if there is no filepath.
     ((not filepath) nil)
     ((let ((exts '(".avi" ".mp4" ".mkv" ".wmv" ".3gp" ".vob" ".mov" ".mp3"))
            (fname (downcase filepath)))
        (my/any (mapcar (lambda (ext) (string-suffix-p ext fname)) exts)))
      (progn (message "Skipping preview of video file.") nil))
     ;; Bail out if a preview for the same file is already queued.
     ((and ftag-async-preview-thread
           (thread-live-p ftag-async-preview-thread)
           (equal ftag-last-previewed-file abbrev-filepath))
      nil)
     ;; Check if the preview buffer exits.
     ((and last-buffer (equal abbrev-filepath ftag-last-previewed-file))
      (unless (get-buffer-window last-buffer) ;; Only display it if it isn't already visible.
        (display-buffer last-buffer)))
     ;; Preview the file if the preview buffer doesn't already exist.
     ((and filepath (file-exists-p filepath))
      (progn
        ;; If a file has been previewed already and its buffer exists, kill it.
        (when (and ftag-last-previewed-file last-buffer)
          (kill-buffer last-buffer))
        ;; Set the file we're about to preview as the last previewed.
        (setq ftag-last-previewed-file abbrev-filepath)
        ;; Wait for previously queued up previews to finish.
        (when ftag-async-preview-thread
          (thread-join ftag-async-preview-thread))
        (setq ftag-async-preview-thread ;; Queue up the preview in a different thread.
              (make-thread `(lambda ()
                              (display-buffer (find-file-noselect ,filepath))))))))))

(defun ftag-open-file-at-point ()
  "Open the file at point in the default program."
  (interactive)
  (let* ((folder (file-name-directory (buffer-file-name)))
         (filepath (concat folder (ftag-file-at-point))))
    (let ((cmd (if (using-windows) "explorer" "xdg-open")))
      (shell-command (concat cmd " \"" filepath "\"")))))

(defun ftag-insert-tags ()
  "Insert an empty tags header at point."
  (interactive)
  (insert "[tags]\n\n")
  (forward-line -1))

(defun ftag-track-untracked-files ()
  "Track all untracked files by inserting them into the current .ftag file."
  (interactive)
  ;; Insertion should happen at the end of the file.
  (save-excursion
    (goto-char (point-max))
    (forward-line)
    (let* ((dirpath (file-name-directory (buffer-file-name)))
           (untracked-files (sort (ftag-untracked-files dirpath) 'string-lessp))
           (text (string-join
                  (mapcar (lambda (file) (concat "[path]\n" file "\n")) untracked-files)
                  "\n")))
      (insert text))))

(defvar ftag-mode-name "ftag"
  "The name of the ftag-mode shown in the mode line.")

(make-variable-buffer-local
 (defvar ftag-follow-mode-enabled nil
   "Indicates whether follow mode is active."))

(defun ftag-set-mode-name ()
  "Update the `mode-name' and mode line."
  (let ((new-name
         (concat ftag-mode-name
                 (if ftag-follow-mode-enabled "-follow" ""))))
    (setq mode-name new-name)
    (force-mode-line-update)))

(make-variable-buffer-local
 (defvar current-line-number nil
   "Tracks the line number of the point.
This is useful in follow mode."))

(make-variable-buffer-local
 (defvar ftag-line-changed-hook nil
   "This hook is run when the cursor changes line."))

(defun ftag-update-line-number ()
  "Update the line number if needed and run the `ftag-line-changed-hook'."
  (let ((new-line-num (line-number-at-pos)))
    (when (not (equal new-line-num current-line-number))
      (setq current-line-number new-line-num)
      (run-hooks 'ftag-line-changed-hook))))

(defun ftag-follow-mode ()
  "Toggle follow mode."
  (interactive)
  (setq ftag-follow-mode-enabled (not ftag-follow-mode-enabled))
  (if ftag-follow-mode-enabled
      (progn
        (add-hook 'ftag-line-changed-hook #'ftag-preview-file nil t)
        (message "Follow mode is enabled."))
    (progn
      (remove-hook 'ftag-line-changed-hook #'ftag-preview-file t)
      (message "Follow mode is disabled.")))
  (ftag-set-mode-name))

;;;###autoload
(defun ftag-query (dirpath query)
  "QUERY ftag at DIRPATH and output the paths to resulting files."
  (let* ((default-directory (file-name-as-directory dirpath)))
    (mapcar (lambda (file)
              (concat default-directory file))
            (seq-filter
             (lambda (str) (not (string-equal "" str)))
             (split-string (string-trim
                            (shell-command-to-string
                             (format "ftag -q \"%s\"" query)))
                           "\n")))))

(defvar ftag-mode-map-prefix [(control c)]
  "The prefix key used to bind ftag commands in Emacs.")

(defvar ftag-mode-map
  (let ((map (make-sparse-keymap))
        (p ftag-mode-map-prefix))
    (define-key map (vconcat p [(control c)]) #'ftag-preview-file)
    (define-key map (vconcat p [(control f)]) #'ftag-follow-mode)
    (define-key map (vconcat p [(control t)]) #'ftag-insert-tags)
    (define-key map (vconcat p [(control o)]) #'ftag-open-file-at-point)
    map)
  "Keymap used in `ftag-mode' buffers.")

(defvar ftag-font-lock-keywords
  '(("^\\[.*\\]" . font-lock-builtin-face)))

;;;###autoload
(define-derived-mode ftag-mode text-mode ftag-mode-name
  "Simple mode for editing .ftag files.
\\{ftag-mode-map}"
  (add-hook 'completion-at-point-functions #'ftag-completion-at-point 0 t)
  (add-hook 'after-save-hook #'ftag-invalidate-tags-cache nil t)
  (add-hook 'post-command-hook #'ftag-update-line-number)
  (setq font-lock-defaults '(ftag-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ftag$" . ftag-mode))

(add-hook 'ftag-mode-hook 'company-mode)

(provide 'ftag)
;;; ftag.el ends here
