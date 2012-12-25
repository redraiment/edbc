;;;; edbc-sqlite -- Emacs Database Connectivity for sqlite
;;;;
;;;; Author: Zhang, Zepeng (Joe) <redraiment@gmail.com>
;;;; version: 0.1
;;;; Time-stamp: <2012-09-04 CST>
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

(require 'edbc)

(defadvice edbc-command-options
  (after edbc-sqlite-command-options (options) activate)
  (setf ad-return-value `("sqlite3" "-batch" "-line"
                          ,(second (assoc 'url options)))))

(setf edbc-cmd-quit ".quit\n")

(defun sqlite-sql-format (sql)
  (concat sql ";\n"))

(add-hook 'edbc-alive-sql-hook #'sqlite-sql-format)

(defmacro if-let (binding then &optional else)
  "(if-let binding then else?)
binding => binding-form test

If test is true, evaluates then with binding-form bound to the value of
test, if not, yields else"
  `(let (,binding)
     (if ,(car binding)
       ,then
       ,else)))

(defun sqlite-output-format (output)
  (mapcar (lambda (paragraph)
            (mapcar (lambda (line)
                      (if-let (idx (string-match "=" line))
                        (cons (string-trim (substring line 0 idx))
                              (string-trim (substring line (1+ idx))))
                        (list (string-trim line))))
             (delq "" (split-string paragraph "\r?\n"))))
          (split-string output "\r?\n\r?\n")))

(add-hook 'edbc-output-format-hook #'sqlite-output-format)

(provide 'edbc-sqlite)
