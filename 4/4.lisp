(ql:quickload :cl-ppcre)

(defpackage :day4
  (:use :cl
   :cl-ppcre))
(in-package :day4)

(defparameter *file* "input")
(defparameter *file-test* "input-test")

(defun read-table (stream)
  (let ((table (make-array '(6 6) :initial-element 0))
        val)
    (dotimes (i 5)
      (setf val (map 'vector  #'parse-integer
                     (ppcre:all-matches-as-strings "\\S+" (read-line stream nil))))
      (dotimes (j 5)
        (setf (aref table i j) (aref val j))))
    table))

(defun split-token (token l)
  (cl-ppcre:split token l))

(defun file->lst (file)
  (let ((res ())
        values-to-check)
    (with-open-file (stream-input file)
      (setf values-to-check
            (mapcar #'parse-integer (split-token "," (read-line stream-input nil))))
      (loop for val = (read-line stream-input nil)                      
            while val do
              (push (read-table stream-input) res)))
    (list values-to-check (reverse res))))

(defun search-mark-value (val board)
  (dotimes (i 5)
    (dotimes (j 5)
      (when  (and (numberp (aref board i j))(= val (aref board i j)))
        (setf (aref board i j) t)
        (incf (aref board i 5))
        (incf (aref board 5 j))))))

(defun win? (table)
  (let ((win nil))
    (dotimes (i 5)
      (when (or (= 5 (aref table i 5)) (= 5 (aref table 5 i)))
        (setf win t)))
    win))

(defun search-winning-board-value (values boards)
  (let ((winning-board())
        last-value)
    (do ((value (pop values) (pop values)))
        ((not (null winning-board)) (list last-value winning-board))
      (mapcar #'(lambda (board)
                  (search-mark-value value board)
                  (when (and (null winning-board) (win? board))
                    (setf winning-board board)
                    (setf last-value value)))
              boards))))

(defun sum-rest-numbers (board)
  (let ((sum 0))
    (dotimes (i 5)
      (dotimes (j 5)
      (when (numberp (aref board i j))
        (incf sum (aref board i j)))))
    sum))

(defun resolve (problem)
  (let* ((numbers (first problem))
         (boards (second problem))
         (winner (search-winning-board-value numbers boards)))
    (* (first winner) (sum-rest-numbers (second winner)))))

;; Testing values
(resolve (file->lst *file-test*));; 4512


;; Real computations
(resolve (file->lst *file*));; 63424

;; Second part
(defun search-winning-board-value-2nd (values boards)
  (let ((winning-board())
        last-value)
    (do ((value (pop values) (pop values)))
        ((= (length boards) (length winning-board)) (list last-value (first winning-board)))
      (mapcar #'(lambda (board)
                  (when (not (find board winning-board))
                    (search-mark-value value board)
                    (when (win? board)
                      (push board winning-board)
                      (setf last-value value))))
              boards))))

(defun resolve-2nd (problem)
  (let* ((numbers (first problem))
         (boards (second problem))
         (winner (search-winning-board-value-2nd numbers boards)))
    (* (first winner) (sum-rest-numbers (second winner)))))

;;Test
(resolve-2nd (file->lst *file-test*));; 1924

;;Real
(resolve-2nd (file->lst *file*));; 23541
