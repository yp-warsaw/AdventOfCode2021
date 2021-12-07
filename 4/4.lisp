(ql:quickload :cl-ppcre)

(defpackage :day4
  (:use :cl
   :cl-ppcre))
(in-package :day4)

(defparameter *file* "input")

(defun split-token (token l)
  (cl-ppcre:split token l))


(defun file->lst (file)
  (let ((table (make-array '(6 6) :initial-element 0))
        values-to-check (res ()) tmp)
    (with-open-file (stream-input file)
      (setf tmp (read-line stream-input nil))
      (setf values-to-check (mapcar #'parse-integer (split-token "," tmp)))
      (loop for val = (read-line stream-input nil)                      
            while val do
              (dotimes (j 5)
                (setf val (read-line stream-input nil))
                ;; (format t "~& val=~A tmp=~A" val tmp)
                (setf tmp (map 'vector  #'parse-integer (ppcre:all-matches-as-strings "\\S+" val)))
                ;; (format t "~& val=~A tmp=~A" val tmp)
                (dotimes (i 5)
                  (setf (aref table i j) (aref tmp i))))
              (format t "~& table=~A" table)
              (push table res)))
    (list values-to-check res)))
    ;;         while val do
    ;;           (let* ((action (split-token " " val))
    ;;                  (mvt (string-trim '(#\Space)(first action)))
    ;;                  (value (parse-integer (second action))))
    ;;             (push (list mvt value) res))))
    ;; (reverse res)))

(length (second (file->lst *file*)))
(first (second (file->lst *file*)))
 (second (file->lst *file*))

(let ((a (make-array '(3 3))))
  (aref a 1 2))

(let ((a ()))
  (push (vector 1 2 3) a)
  a)
