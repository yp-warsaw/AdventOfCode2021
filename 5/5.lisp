(ql:quickload :cl-ppcre)

(defpackage :day5
  (:use :cl
   :cl-ppcre))
(in-package :day5)

(defparameter *file* "input")
(defparameter *file-test* "input-test")

(defun split-token (token l)
  (cl-ppcre:split token l))


(defun file->lst (file)
  (let (res)
    (with-open-file (stream-input file)
      (loop for val = (read-line stream-input nil)
            while val do
              (let* ((action (split-token "->" val))
                     (first-coord (mapcar #'parse-integer
                                          (split-token ","
                                                       (string-trim '(#\Space)(first action)))))
                     (second-coord (mapcar #'parse-integer
                                           (split-token ","
                                                        (string-trim '(#\Space)
                                                                     (second action))))))
                (push (append first-coord second-coord) res))))
    (reverse res)))

(defun lines (matrix coords)
  (let ((begin-x (min (first coords) (third coords)))
        (end-x (max (first coords) (third coords)))
        (begin-y (min (second coords) (fourth coords)))
        (end-y (max (second coords) (fourth coords))))
    (when (or (= begin-x end-x) (= begin-y end-y))
      (do ((i begin-x (1+ i)))
          ((> i end-x))
        (do ((j begin-y (1+ j)))
            ((> j end-y))
          (incf (gethash (list i j) matrix 0)))))))

(defun count-crit (hash-table)
  (let ((cnt 0))
    (maphash #'(lambda (k v) (when (> v 1)
                               (incf cnt)))
             hash-table)
    cnt))

(defun resolve (coords fn-line)
  (let ((a (make-hash-table :test #'equalp)))
    (mapcar #'(lambda (x)
                (funcall fn-line a x))
            coords)
    (count-crit a)))

;; Test
(resolve (file->lst *file-test*) #'lines);; 5

;; Real
(resolve (file->lst *file*) #'lines);; 4655

;; 2nd part
(defun tester (fn x y)
  (funcall fn x y))

(defun compute-line (coords matrix)
  (let ((i1 (first coords))
         (i2 (third coords))
         (j1 (second coords))
         (j2 (fourth coords))
        dy dx exit-cond cond-y)
    (when (< i1 i2)
      (setf dx 1)
      (setf exit-cond #'>))
    (when (> i1 i2)
      (setf dx -1)
      (setf exit-cond #'<))
    (when (< j1 j2)
      (setf dy 1)
      (setf cond-y #'<=))
    (when (> j1 j2)
      (setf dy -1)
      (setf cond-y #'>=))
    (do* ((i i1 (+ i dx))
          (j j1 (+ j dy)))
         ((tester exit-cond i i2))
      (when (tester cond-y j j2)
        (incf (gethash (list i j) matrix 0))))))

(defun lines-2 (matrix coords)
  (let ((begin-x (min (first coords) (third coords)))
        (end-x (max (first coords) (third coords)))
        (begin-y (min (second coords) (fourth coords)))
        (end-y (max (second coords) (fourth coords))))
    (if (or (= begin-x end-x) (= begin-y end-y))
      (do ((i begin-x (1+ i)))
          ((> i end-x))
        (do ((j begin-y (1+ j)))
            ((> j end-y))
          (incf (gethash (list i j) matrix 0))))
      (compute-line coords matrix))))

;; Test
(resolve (file->lst *file-test*) #'lines-2);; 12

;; Real
(resolve (file->lst *file*) #'lines-2) ;; 20500
