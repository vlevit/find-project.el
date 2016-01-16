;;; find-project.el --- find project

;; Copyright (C) 2016 Vyacheslav Levit

;; Author: Vyacheslav Levit <dev@vlevit.org>
;; Maintainer: Vyacheslav Levit <dev@vlevit.org>
;; Version: 0.0.0
;; Created: 1 January 2016
;; Keywords: projects convenience
;; URL: http://github.com/vlevit/find-project.el

;; This file is NOT part of Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Before using the package you must specify patterns of your project
;; paths. For example:
;;
;;     (setq find-project-patterns
;;           '("~/.emacs.d"
;;             "~/.emacs.d/site-lisp/*"
;;             "~/projects/*"
;;             "~/work/*/*"))
;;
;; Now if you call `M-x find-project' all matching projects will show
;; up. After you choose the project `find-project-default-action' will
;; be executed which is by default a list of pre-defined actions like
;; magit or dired.
;;
;; You can customize default action by specifying a list of actions:
;;
;;     (setq find-project-default-action '(find-file-in-repository find-file))
;;
;; Or a single action:
;;
;;     (setq find-project-default-action 'magit-status)
;;
;; in which case it will be executed immediately.
;;
;; You can customize completion functions for projects and actions
;; separately:
;;
;;     (setq find-project-completing-read-projects 'ivy-completing-read)
;;     (setq find-project-completing-read-actions 'ido-completing-read)
;;
;; Examples of valid completing functions are `completing-read',
;; `ido-completing-read', `helm-comp-read', `ivy-completing-read'.
;;
;; You can have more fine-grained control of where your projects are
;; located and what actions to execute by using the property list in
;; patterns:
;;
;;     (setq find-project-patterns
;;           '((:pattern "~/.emacs.d/site-lisp/*" :action find-file)
;;             (:pattern "~/work/*/*" :exclude "~/work/*hat/*")))
;;
;; Valid properties are `:pattern', `:function', `:exclude' and
;; `:action'.
;;
;; `function' can be used when wildcards are not sufficient to find
;; your projects' locations.
;;
;; `:exclude' can be used for filtering out some directories. It can
;; be a wildcard, list of wildcards or a function accepting directory
;; as argument.
;;
;; In `find-project-patterns' you can mix both wildcards and plists.
;;
;; Global project filter can be provided via `find-project-exclude':
;;
;;     (setq find-project-exclude "*/.*")
;;
;; `find-project-exclude' can accept the same types as `:exclude'.
;;
;; By default, recently selected projects are suggested first. You can
;; disable this behavior by adding this line to your Emacs config:
;;
;;     (setq find-project-recent-first nil)

;;; Code:

(require 'cl-lib)

(defgroup find-project nil
  "Find project."
  :group 'convenience)

(defcustom find-project-patterns nil
  "Patterns to your project paths.
Either list of strings or list of plists. Each plist must contain
either `:pattern' or `:function' key and optionally `:action' and
`:exclude'.")

(defcustom find-project-completing-read-projects 'completing-read
  "Function which reads user input for choosing a project.
Good options are `completing-read', `ido-completing-read',
`helm-comp-read', `ivy-completing-read'."
  :type 'function)

(defcustom find-project-completing-read-actions 'completing-read
  "Function which reads user input for choosing an action.
Good options are `completing-read', `ido-completing-read',
`helm-comp-read', `ivy-completing-read'."
  :type 'function)

(defcustom find-project-default-action
  '(magit-status find-file-in-repository find-file-in-project find-file find-project-dired)
  "Default action executed for selected project.
Must be a function which doesn't accept arguments or a list of
such functions.")

(defcustom find-project-exclude nil
  "Global filter for projects.
Must be a string (wildcard) or list of strings or a function
accepting directory as argument.")

(defcustom find-project-recent-first t
  "Show recently selected projects on the top."
  :type 'boolean)

(defcustom find-project-save-file "~/.emacs.d/.find-project.el"
  "File where to save data such as history of selected projects."
  :type 'file)

(defvar find-project-history nil)

(defun find-project-dired ()
  (dired default-directory))

(defun find-project-dump-list (symbol)
  (let ((value (symbol-value symbol)))
    (insert (format "(setq %S" symbol))
    (newline-and-indent)
    (insert "\'(")
    (dolist (el value)
      (newline-and-indent)
      (insert (format "%S" el)))
    (newline-and-indent)
    (insert "))\n")))

(defun find-project-save-history ()
    (with-temp-file find-project-save-file
      (emacs-lisp-mode)
      (insert ";;; Automatically generated by `find-project'\n\n")
      (find-project-dump-list 'find-project-history)))

(defun find-project--update-history (dir)
  (setq find-project-history (delete dir find-project-history))
  (push dir find-project-history)
  (find-project-save-history))

(defun find-project--expand (wildcard)
  (cl-remove-if-not 'file-directory-p (file-expand-wildcards wildcard)))

(defun find-project--exclude (dirs pattern)
  (let ((match-p
         (cond ((stringp pattern)
                (lambda (dir)
                  (string-match-p (wildcard-to-regexp pattern) dir)))
               ((listp pattern)
                (lambda (dir)
                  (cl-loop for pat in pattern do
                           (when (string-match-p (wildcard-to-regexp pat) dir)
                             (return t)))))
               ((functionp pattern)
                pattern)
               (t
                (error ":exclude must be a wildcard, list of wildcards or a function")))))
    (cl-remove-if match-p dirs)))

(defun find-project--matched-dirs-unfiltered (pattern)
  "Return alist of directories matching the PATTERN.
PATTERN must be either a wildcard string or a plist.
`find-project-exclude' is not considered."
  (cond ((stringp pattern)
         (find-project--expand pattern))
        ((listp pattern)
         (let ((pattern (plist-get pattern :pattern))
               (function (plist-get pattern :function))
               (exclude (plist-get pattern :exclude)))
           (let ((dirs
                  (if pattern
                      (find-project--expand pattern)
                    (if function
                        (funcall function)
                      (error ":pattern or :function must be specified")))))
             (if exclude (find-project--exclude dirs exclude)
               dirs))))
        (t
         (error "Pattern must be a wildcard string or a property list"))))

(defun find-project--matched-dirs (pattern)
  "Return alist of directories matching the PATTERN.
PATTERN must be either a wildcard string or a plist.
Filter against `find-project-exclude' value."
  (let ((matched-dirs (find-project--matched-dirs-unfiltered pattern)))
    (if find-project-exclude
        (find-project--exclude matched-dirs find-project-exclude)
      matched-dirs)))

(defun find-project--sort-recent (dirs)
  (let ((size (length dirs)))
    (dolist (hist-dir find-project-history)
      (setq dirs (delete hist-dir dirs))
      (if (= (length dirs) size)
          (setq find-project-history (delete hist-dir find-project-history))
        (setq size (length dirs))))
    (append find-project-history dirs)))

(defun find-project--matched-projects (pattern)
  "Return alist of (DIR . ACTION) matching the PATTERN."
  (let ((action (or (and (listp pattern) (plist-get pattern :action))
                    find-project-default-action)))
    (mapcar (lambda (dir) (cons dir action))
            (find-project--matched-dirs pattern))))

(defun find-project--read-action (action-list)
  (let* ((bound-actions
          (cl-remove-if-not 'fboundp action-list))
         (actions-alist
          (mapcar (lambda (action)
                    (cons (symbol-name action) action)) bound-actions))
         (action-strings
          (mapcar 'car actions-alist))
         (selected-action
          (funcall find-project-completing-read-actions
                   "Run: " action-strings)))
    (cdr (assoc selected-action actions-alist))))

(defun find-project ()
  "Select a project from `find-project-patterns' and run action
on it."
  (interactive)
  (let* ((projects
          (cl-reduce 'append (mapcar 'find-project--matched-projects find-project-patterns)))
         (directories
          (mapcar 'car projects))
         (directories
          (if find-project-recent-first
              (find-project--sort-recent directories)
            directories))
         (selected-directory
          (funcall find-project-completing-read-projects "Projects: " directories))
         (action (cdr (assoc selected-directory projects)))
         (action
          (if (listp action)
              (find-project--read-action action)
            action)))
    (with-temp-buffer
      (cd selected-directory)
      (if (commandp action)
          (call-interactively action)
        (funcall action))
      (find-project--update-history selected-directory))))

(when find-project-recent-first
    (load find-project-save-file t))

(eval-after-load "ivy"
  '(add-to-list 'ivy-sort-functions-alist
                '(find-project . nil)))

(provide 'find-project)
