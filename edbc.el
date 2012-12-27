;;;; EDBC is Emacs-Lisp-based data access technology.
;;;; It is an acronym Emacs-Lisp Database Connectivity, with DB being
;;;; universally recognized as the abbreviation for database. This
;;;; technology is an API for the Emacs-Lisp programming language that
;;;; defines how a client may access a data. It provides methods for
;;;; querying and updating data in a database. EDBC is oriented towards
;;;; relational databases.
;;;;
;;;; Author: Zhang, Zepeng (Joe) <redraiment@gmail.com>
;;;; Time-stamp: <2012-12-11 CST>
;;;; Copyright: (C) 2012 Zhang, Zepeng
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 2, or (at
;;;; your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program ; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA 02111-1307, USA.

(require 'cl)

(defconst *edbc-process-name* "*edbc-process-name*"
  "The name of edbc process in Emacs.")

(defconst *edbc-buffer-name* "*edbc-process-output-buffer*"
  "The name of buffer which store edbc process's output.")

(defvar *edbc-process-id* nil
  "EDBC process id")


;;; Required parameters in driver

(defvar edbc-cmd-quit ""
  "Command to quit database process.")

(defvar edbc-alive-sql-hook nil
  "Hooks to format the SQL statement.")

(defvar edbc-output-format-hook nil
  "Hooks to parse the SQL output.")


;;; Required interface
(defun edbc-command-options (options)
  options)


(defun string-trim (s)
  "Returns a copy of the string, with leading and trailing whitespace omitted."
  (replace-regexp-in-string "^\s+" ""
    (replace-regexp-in-string "\s+$" "" s)))

(defmacro keyword->symbol (keyword)
  "Converts keyword to symbol without prefix `:'"
  `(intern (substring (symbol-name ,keyword) 1)))

(defvar *edbc-eval-content?* nil
  "Eval embedded s-expression in SQL statement.")

(defun edbc-alive-sql-parse (e)
  "Convert s-expression to a SQL statement string."
  (cond
   ((eq e ':)
    ;; embed lisp expression
    (setf *edbc-eval-content?* t)
    "")
   ((keywordp e)
    ;; embed lisp variable
    (edbc-alive-sql-parse (symbol-value (keyword->symbol e))))
   (*edbc-eval-content?*
    ;; eval the expression
    (setf *edbc-eval-content?* nil)
    (edbc-alive-sql-parse (eval e)))
   ((stringp e)
    (concat "'" (replace-regexp-in-string "'" "''" e) "'"))
   ((consp e)
    (let ((sql (mapconcat #'edbc-alive-sql-parse e " ")))
      (if (eq (car e) '\,)
          sql
        (concat "(" sql ")"))))
   (t (format "%s" e))))

(defmacro edbc-reduce-hooks (init hooks)
  `(reduce #'(lambda (e fn) (funcall fn e)) ,hooks :initial-value ,init))

(defun edbc-exec (sql)
  ""
  (unless *edbc-process-id*
    (error "EDBC process was not setup!"))
  (let ((cmd (edbc-reduce-hooks
              (substring (edbc-alive-sql-parse sql) 1 -1)
              edbc-alive-sql-hook)))
    (with-current-buffer (process-buffer *edbc-process-id*)
      (erase-buffer)
      (process-send-string *edbc-process-id* cmd)
      (accept-process-output *edbc-process-id*)
      (edbc-reduce-hooks (buffer-substring 1 (buffer-size))
                         edbc-output-format-hook))))

(defmacro edbc (&rest sql)
  `(edbc-exec ',sql))

(defmacro edbc-with-connect (options &rest body)
  "Connect database with options.
In body, you can use `edbc' to execute SQL statement. Use `:' to refer sexp."
  (declare (indent 1))
  (let ((var-result (gensym "result")))
    `(let ((*edbc-process-id* (start-process *edbc-process-name*
                                             *edbc-buffer-name*
                                             ,@(edbc-command-options options))))
       (setf ,var-result (progn ,@body))
       (process-send-string *edbc-process-id* edbc-cmd-quit)
       ,var-result)))

(provide 'edbc)
