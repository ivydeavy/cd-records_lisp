;;Made by Deepraj Das
;;declaring global variables
(defvar *alist-id* "id-")
(defvar *id-count* 0)
(defvar *db* nil)
(defvar *name* nil)
(defvar *artist* nil)
(defvar *rating* nil)
(defvar *record* nil)
(defvar *fileNM* nil)
(defvar *fileDR* nil)
(defvar *fileEXT* nil)
(defvar *non-matched-data* nil)
(defparameter *choice* nil)

;;print formatted strings
(defun print-format(i)
(format t "~%~{~a:~10t~a~%~{~a:~10t~a~%~}~}~%" i))

;;assign unique id's
(defun make-id()
(concatenate 'string *alist-id* (write-to-string (setq *id-count*(+ 1 *id-count*)))))

;;make list
(defun make-cd (name artist rating ripped)
(list :id (make-id)(list :name name :artist artist :rating rating :ripped ripped)))

;;push to *db*
(defun add-cd (prompt)
  (push prompt *db*))

;;prompt for user input
(defun prompt()
  (terpri)
  (princ "Enter CD name: ")
  (setq *name* (read-line))
  (princ "Enter Artist name: ")
  (setq *artist* (read-line))
  (princ "Enter rating: ")
  (setq *rating* (or (parse-integer (read-line) :junk-allowed t) 0))
  (make-cd *name* *artist* *rating* (y-or-n-p "Ripped [y/n]: ")))

;;addlist function
(defun add-list ()
  (loop (add-cd (prompt))
  (terpri)
  (if (not (y-or-n-p "Another?[y/n]: ")) (return)))
  (print-string "Registered successful!")
  (interface))

;;search by ID
(defun list-by-id(id)
(dolist (x *db*)
(setq *record* (getf x :id))
(if (string-equal *record* id) (print-format x) (print-string "Not found!")))
(interface))

;;print all records
(defun print-all()
(if (equal *db* nil) (print-string "No record found!")
(dolist (x *db*)
(print-format x)))
(interface))

;;write to db
(defun write-db()
(if (equal *db* nil) (print-string "No record found! Please create atleast one.")
(progn 
(princ "Enter your Filename: ")
(setq *fileNM* (write-to-string (read)))
(princ "Enter destination ['./' for current directory]: ")
(setq *fileDR* (or (write-to-string (read)) "./"))
(princ "Enter file extension [e.g. '.db']: ")
(setq *fileEXT* (or (write-to-string (read)) ".db"))
(with-open-file (out (concatenate 'string *fileDR* *fileNM* *fileEXT* ) :direction :output :if-exists :supersede)
  (print *db* out))
 (print-string "Write to file successful!")))
 (interface))

;;remove list
(defun remove-list(id)
(if (equal *db* nil) (print-string "No record found! Please create atleast one.")
(progn 
(print-string "Enter * to DELETE all the records! Or by ID.")
(if (string-equal id "*") (progn (setq *db* nil) (print-string "All Records are Deleted successful!"))
(dolist (x *db*)
(if (equal (getf x :id) id) (print-string (concatenate 'string "This list is deleted:" (write-to-string x))) ())
(if (not (equal (getf x :id) id)) (push x *non-matched-data*))))
(setf *db* nil)))
(interface))

;;print simple string
(defun print-string(str)
(format t "~a~%" str))

;;main interface
(defun interface()
(terpri)
(terpri)
(print-string "환영하다! Welcome to CD Record Management!")
(print-string "Made by Deepraj Das!")
(print-string "1. Create CD record.")
(print-string "2. Delete CD record.")
(print-string "3. View record By ID.")
(print-string "4. View All.")
(print-string "5. Write a DB file.")
(print-string "6. Exit.")
(print-string "**Updation of CD records is not available.")
(princ "Please enter your choice: ")
(setq *choice* (read))
(case *choice*
(1 (add-list))
(2 (progn (write-string "Enter ID to delete: ")(remove-list (read))))
(3 (progn (write-string "Enter ID to Find: ")(list-by-id (read))))
(4 (print-all))
(5 (write-db))
(6 (progn (write-string "안녕! annyeong!") (exit)))))

(interface)