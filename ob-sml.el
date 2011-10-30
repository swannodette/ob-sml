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
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("sml" . "sml"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:sml '())

(defun get (alist key)
  (cdr (assoc key alist)))

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:sml' function below.
(defun org-babel-expand-body:sml (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'sml-mode)
  (let ((vars (get (or processed-params
                       (org-babel-process-params params))
                   :vars)))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-sml-var-to-sml (cdr pair))))
      vars "\n") "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:sml (body params)
  "Execute a block of Standard ML code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Standard ML source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         (session (org-babel-sml-initiate-session
                   (get processed-params :session)))
         ;; variables assigned for use in the block
         (vars (get processed-params :vars))
         (result-params (get processed-params :result-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (get processed-params :result-type))
         ;; expand the body with `org-babel-expand-body:sml'
         (full-body (org-babel-expand-body:sml
                     body params processed-params)))
    (with-current-buffer (sml-proc-buffer)
      (sml-send-string full-body))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
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
        (run-sml)
        (sleep-for 0.25)
        (current-buffer))))

(provide 'ob-sml)
