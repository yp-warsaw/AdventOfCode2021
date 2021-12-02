(defparameter *file* "input")

(defun file->lst (file)
  (let (res)
    (with-open-file (stream-input file)
      (loop for val = (read-line stream-input nil)
            while val do
              (push (parse-integer val) res)))
    (reverse res)))

(defun lst->vec (lst)
  (coerce lst 'vector))

(defun count-incr (vect)
  (let ((cnt 0))
    (dotimes (i (1- (length vect) ))
      (when (< (aref vect i) (aref vect (1+ i)))
        (incf cnt)))
    cnt))

(defun gen-windowd (vec &optional (step 3))
  (let ((length-vec (length vec))
        (lst ()))
    (dotimes (i (- length-vec  (1- step)))
    (push (+ (aref vec i) (aref vec (1+ i)) (aref vec (+ 2 i))) lst))
    (lst->vec (reverse lst))))

(let ((vect (lst->vec (file->lst *file*))))
      (count-incr vect))
(let ((vect (lst->vec (file->lst *file*))))
  (gen-windowd vect))

(let ((vect (lst->vec (list 199 200 208 210 200 207 240 269 260 263))))
  (gen-windowd vect)
  (format t "~& len=~A vec=~A incr=~A" (length vect)  (gen-windowd vect) (count-incr (gen-windowd vect))))

(let ((vect (lst->vec (file->lst *file*))))
  (format t "~& len=~A vec=~A" (length vect) (count-incr (gen-windowd vect))))
