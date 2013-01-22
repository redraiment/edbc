;;; edbc-mysql.el --- Emacs Database Connectivity for MySQL

;; Copyright: (C) 2012 Zhang, Zepeng

;; Author: Zhang, Zepeng (Joe) <redraiment@gmail.com>
;; Version: 0.1
;; Time-stamp: <2012-12-25 CST>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'edbc)

(defadvice edbc-command-options
  (after edbc-mysql-command-options (options) activate)
  (setf ad-return-value `("mysql" "-B"
                          ,@(mapcar
                             (lambda (o)
                               (concat "--" (symbol-name (first o))
                                       "=" (second o)))
                             options))))

(setf edbc-cmd-quit "quit\n")

(defun mysql-sql-format (sql)
  (concat sql ";\n"))

(add-hook 'edbc-alive-sql-hook #'mysql-sql-format)

(defun mysql-output-format (output)
  (destructuring-bind
      (head . content)
      (mapcar (lambda (line) (split-string line "\t"))
              (split-string output "\r?\n"))
    (mapcar (lambda (record)
              (loop for title in head
                    for field in record
                    collect (cons title field)))
            content)))

(add-hook 'edbc-output-format-hook #'mysql-output-format)

(provide 'edbc-mysql)
