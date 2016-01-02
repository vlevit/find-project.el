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
;; the default action (`dired') will be executed in the project's
;; root.
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

(defvar find-project-patterns nil)

(defvar find-project-completing-read-function 'completing-read)

(defvar find-project-default-action 'find-project-dired)

(defvar find-project-actions nil)

(defun find-project-dired ()
  (dired default-directory))

(defun find-project-matched-dirs (pattern)
  (cl-remove-if-not 'file-directory-p (file-expand-wildcards pattern)))

(defun find-project-assoc-action (project)
  (or (loop for (wildcard . action) in find-project-actions do
            (let ((regexp (wildcard-to-regexp wildcard)))
              (when (string-match-p regexp project)
                (return action))))
      find-project-default-action))

(defun find-project ()
  "Select a project from `find-project-patterns'"
  (interactive)
    (let* ((projects
            (cl-reduce 'append (mapcar 'find-project-matched-dirs find-project-patterns)))
           (selected-project
            (funcall find-project-completing-read-function "Projects: " projects))
           (action (find-project-assoc-action selected-project)))
      (with-temp-buffer
        (cd selected-project)
        (if (commandp action)
            (call-interactively action)
          (funcall action)))))

(provide 'find-project)
