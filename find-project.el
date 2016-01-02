;;; find-project.el --- ask for project and then switch to it

;; Copyright (C) 2016 Vyacheslav Levit

;; Author: Vyacheslav Levit <dev@vlevit.org>
;; Maintainer: Vyacheslav Levit <dev@vlevit.org>
;; Version: 0.0.0
;; Created: 1 January 2016
;; Keywords: projects
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

;; To make the package useful it's needed to specify wildcard path
;; patterns to your project places. For example:
;;
;;     (setq find-project-patterns
;;           '("~/.emacs.d"
;;             "~/.emacs.d/site-lisp/*"
;;             "~/projects/*"
;;             "~/work/*/*"))
;;
;; Once patterns are specified `M-x find-project' will present all the
;; projects matching the specified patterns. After project is selected
;; the default action will be executed in the project's root.
;;
;; Patterns can also be property lists with such keys `:pattern',
;; `:function', `:exclude' and `:action'. For example:
;;
;;     (setq find-project-patterns
;;           '("~/.emacs.d"
;;             (:pattern "~/.emacs.d/site-lisp/*" :action find-file)
;;             (:pattern "~/work/*/*" :exclude "~/work/*hat/*")))
;;
;; Completion can be customized by specifying one of the completing
;; function (`completing-read' by default):
;;
;;     (setq find-project-completing-read-function 'ido-completing-read)
;;     (setq find-project-completing-read-function 'ivy-completing-read)
;;
;; Function executed after project is selected can be customized by
;; setting `find-projects-default-action' variable (`dired' by default):
;;
;;     (setq find-projects-default-action 'find-file)
;;
;;     ;; requires `find-file-in-repository' package
;;     (setq find-projects-default-action 'find-file-in-repository)
;;
;;     ;; requires `find-file-in-project' package
;;     (setq find-projects-default-action 'find-file-in-project)

;;; Code:

(require 'cl-lib)

(defvar find-project-patterns nil)

(defvar find-project-completing-read-function 'completing-read)

(defvar find-project-default-action 'find-project-dired)

(defvar find-project-actions nil)

(defun find-project-dired ()
  (dired default-directory))

(defun find-project--expand (wildcard)
  (cl-remove-if-not 'file-directory-p (file-expand-wildcards wildcard)))

(defun find-project--exclude (dirs pattern)
  (let ((match-p
         (if (stringp pattern)
             (lambda (dir)
               (string-match-p (wildcard-to-regexp pattern) dir))
           (if (functionp pattern)
               pattern
             (error ":exclude must be a wildcard string or a function")))))
    (cl-remove-if match-p dirs)))

(defun find-project--matched-dirs (pattern)
  "Return alist of directories matching the PATTERN.
PATTERN can be either a wildcard string or a plist."
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

(defun find-project--matched-projects (pattern)
  "Return alist of (DIR . ACTION) matching the PATTERN."
  (let ((action (or (and (listp pattern) (plist-get pattern :action))
                    find-project-default-action)))
    (mapcar (lambda (dir) (cons dir action))
            (find-project--matched-dirs pattern))))

(defun find-project ()
  "Select a project from `find-project-patterns' and run action
on it."
  (interactive)
  (let* ((projects
          (cl-reduce 'append (mapcar 'find-project--matched-projects find-project-patterns)))
         (directories
          (mapcar 'car projects))
         (selected-directory
          (funcall find-project-completing-read-function "Projects: " directories))
         (action (cdr (assoc selected-directory projects))))
    (with-temp-buffer
      (cd selected-directory)
      (if (commandp action)
          (call-interactively action)
        (funcall action)))))

(provide 'find-project)
