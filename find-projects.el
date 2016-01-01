(defvar find-projects-patterns nil)

(defvar find-projects-completing-read-function 'completing-read)

(defvar find-projects-default-action 'find-projects-dired)

(defvar find-projects-actions nil)

(defun find-projects-dired ()
  (dired default-directory))

(defun find-projects-matched-dirs (pattern)
  (cl-remove-if-not 'file-directory-p (file-expand-wildcards pattern)))

(defun find-projects-assoc-action (project)
  (or (loop for (wildcard . action) in find-projects-actions do
            (let ((regexp (wildcard-to-regexp wildcard)))
              (when (string-match-p regexp project)
                (return action))))
      find-projects-default-action))

(defun find-projects ()
  "Select a project from `find-projects-patterns'"
  (interactive)
    (let* ((projects
            (cl-reduce 'append (mapcar 'find-projects-matched-dirs find-projects-patterns)))
           (selected-project
            (funcall find-projects-completing-read-function "Projects: " projects))
           (action (find-projects-assoc-action selected-project)))
      (with-temp-buffer
        (cd selected-project)
        (funcall action))))

(provide 'find-projects)
