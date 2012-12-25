edbc
====

EDBC is Emacs-Lisp-based data access technology.

It is an acronym Emacs-Lisp Database Connectivity, with DB being
universally recognized as the abbreviation for database. This
technology is an API for the Emacs-Lisp programming language that
defines how a client may access a data. It provides methods for
querying and updating data in a database. EDBC is oriented towards
relational databases.

A sample to connect sqlite:

```Lisp
(edbc-with-connect ((url "users.db"))
  (let ((id 1)
        (name "Joe")
        (nickname "redraiment"))
    ; Purge table
    (edbc delete from users)
    ; Equals insert into users (id, name) values (1, 'Joe') on sqlite
    (edbc insert into users (id, name) values (:id, :name))
    ; Equals update users set name = 'redraiment' where id = 1 on sqlite
    (edbc update users set name = :nickname where id = :(identity id))
    ; Returns ((("id" . "1") ("name" . "redraiment")))
    (edbc select * from users)))
```
