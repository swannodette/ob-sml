;;; ob-sml.el --- org-babel functions for template evaluation

;; Copyright (C) David Nolen

;; Author: David Nolen
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Requirements:

;; sml-mode http://www.smlnj.org/doc/Emacs/sml-mode.html

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(add-to-list 'org-babel-tangle-lang-exts '("sml" . "sml"))

(defvar org-babel-default-header-args:sml '())

(defvar org-babel-sml-eoe "val it = \"org-babel-sml-eoe\" : string")

(defun org-babel-get:sml (alist key)
  (cdr (assoc key alist)))

(defun org-babel-expand-body:sml (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'sml-mode)
  (let ((vars (org-babel-get:sml (or processed-params
                       (org-babel-process-params params))
                   :vars)))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-sml-var-to-sml (cdr pair))))
      vars "\n") "\n" body "\n")))

(defun org-babel-execute:sml (body params)
  "Execute a block of Standard ML code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Standard ML source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-sml-initiate-session
                   (org-babel-get:sml processed-params :session)))
         (vars (org-babel-get:sml processed-params :vars))
         (result-params (org-babel-get:sml processed-params :result-params))
         (result-type (org-babel-get:sml processed-params :result-type))
         (full-body (org-babel-expand-body:sml
                     body params processed-params)))
    (nth 1
     (org-babel-comint-with-output (session org-babel-sml-eoe t body)
       (mapc
        (lambda (line)
          (insert (org-babel-chomp line))
          (comint-send-input nil t))
        (list body "\"org-babel-sml-eoe\";"))))))

(defun org-babel-prep-session:sml (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  session)

(defun org-babel-sml-var-to-sml (var)
  "Convert an elisp var into a string of Standard ML source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-sml-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read results))

(defun org-babel-sml-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (require 'sml-mode)
  (or (get-buffer "*sml*")
      (save-window-excursion
        (call-interactively 'run-sml)
        (sleep-for 0.25)
        (current-buffer))))

(provide 'ob-sml)
