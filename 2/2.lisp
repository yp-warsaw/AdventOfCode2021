(ql:quickload :cl-ppcre)

(defpackage :day2
  (:use :cl
   :cl-ppcre))
(in-package :day2)

(defparameter *file* "input")
(defparameter input-test (list 
                          (list "forward"    5)
                          (list "down"    5)
                          (list "forward"    8)
                          (list "up"    3)
                          (list "down"    8)
                          (list "forward"    2)))

(defun split-token (token l)
  (cl-ppcre:split token l))


(defun file->lst (file)
  (let (res)
    (with-open-file (stream-input file)
      (loop for val = (read-line stream-input nil)
            while val do
              (let* ((action (split-token " " val))
                     (mvt (string-trim '(#\Space)(first action)))
                     (value (parse-integer (second action))))
                (push (list mvt value) res))))
    (reverse res)))

(defun lst->vec (lst)
  (coerce lst 'vector))

(defun mov-pos (action)
  (let ((mvt (first action))
        (val (second action)))
    (cond ((string= mvt "forward") (list val 0))
          ((string= mvt "down") (list 0 val))
          ((string= mvt "up") (list 0 (- val))))))

(defun sum-mvt (lst fn-key)
  (reduce #'+ lst :key fn-key))

;; Testing if for examples values we get the good answers

(mov-pos (list "forward" 3)) ;; (3 0)
(mov-pos (list "down" 3)) ;; (0 3)
(mov-pos (list "up" 3)) ;; (0 -3)

(sum-mvt (mapcar #'mov-pos input-test) #'first) ;; 15
(sum-mvt (mapcar #'mov-pos input-test) #'second) ;; 10
(* (sum-mvt (mapcar #'mov-pos  input-test) #'first)
   (sum-mvt (mapcar #'mov-pos  input-test) #'second)) ;; 150

;; Real computations

(let ((submarine (file->lst *file*)))
  (* (sum-mvt (mapcar #'mov-pos submarine) #'first)
   (sum-mvt (mapcar #'mov-pos submarine) #'second))) ;; 2102357

;; Second part
(defun mov-pos (action pos)
  (let ((mvt (first action))
        (val (second action))
        (horizont (first pos))
        (depth (second pos))
        (aim (third pos)))
    (cond ((string= mvt "forward") (list (+ horizont val) (+ depth (* aim val)) aim))
          ((string= mvt "down") (list horizont depth (+ aim val)))
          ((string= mvt "up") (list horizont depth (- aim val))))))

(reduce #'(lambda (pos action)
            (setf pos (mov-pos action pos))) input-test :initial-value (list 0 0 0)) ;; (15 60 10)
(let ((res (reduce #'(lambda (pos action)
                       (setf pos (mov-pos action pos))) input-test :initial-value (list 0 0 0))))
  (* (first res) (second res))) ;;900

(reduce #'(lambda (pos action)
            (setf pos (mov-pos action pos))) (file->lst *file*) :initial-value (list 0 0 0)) ;;(1927 1090312 1091) 

(let ((res (reduce #'(lambda (pos action)
                       (setf pos (mov-pos action pos))) (file->lst *file*) :initial-value (list 0 0 0))))
  (* (first res) (second res))) ;; 2101031224
