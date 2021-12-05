(defparameter *file* "input")

(defparameter input-test (list
                          "00100"
                          "11110"
                          "10110"
                          "10111"
                          "10101"
                          "01111"
                          "00111"
                          "11100"
                          "10000"
                          "11001"
                          "00010"
                          "01010"))

(defun file->lst (file)
  (let (res)
    (with-open-file (stream-input file)
      (loop for val = (read-line stream-input nil)
            while val do
              (push val res)))
    (reverse res)))

(defun lst->vec (lst)
  (coerce lst 'vector))

(defun extract-carac-pos (lst pos)
  (mapcar #'(lambda(x)
              (digit-char-p (aref x pos)))
          lst))

(defun nr-of-val (val lst)
  (count val lst :test #'=))

(defun most-of-val (nr-of-values len)
  (>= nr-of-values (- len nr-of-values)))

(defun convert-to-number (lst)
  (parse-integer (map 'string
                      #'(lambda (x) (if x #\1 #\0))
                      lst)
                 :radix 2))

(defun compute (input)
  (let ((len (length input))
      (nr-length (length (first input)))
      (lst ()))
    (dotimes (i nr-length lst)
    ;; (format t "~& extracted=~A nr-of-1=~A most-of-val=~A"
    ;;         (extract-carac-pos input i)
    ;;         (nr-of-val 1 (extract-carac-pos input i))
    ;;         (most-of-val (nr-of-val 1 (extract-carac-pos input i)) len))
    (push (most-of-val (nr-of-val 1 (extract-carac-pos input i)) len) lst))
  (* (convert-to-number (reverse lst)) (convert-to-number (mapcar #'not (reverse lst))))))


;;Testing
(compute input-test) ;; 198

;; Real computation
(compute (file->lst *file*)) ;; 2972336



;; Second part
(defun nr-occurence-0-1-criteria-at-pos (lst pos)
  "Return of list with the nb of occurence of 0 and 1"
  (let ((nr-of-0  (nr-of-val 0 (extract-carac-pos lst pos))))
    (list nr-of-0 (- (length lst) nr-of-0))))

(defun find-numbers-vs-criteria-at-pos (lst cri pos)
  (remove-if-not #'(lambda (x)
                     (char= cri (aref x pos)))
                 lst))
(defun resolve-oxygen (input)
  (let ((length-inputs (length (first input)))
        (oxygen input))
    (do ((i 0 (1+ i)))
        ((or (= i length-inputs) (= 1 (length oxygen))) (first oxygen))
      (let ((occurences (nr-occurence-0-1-criteria-at-pos oxygen i)))
        (if (> (first occurences) (second occurences))
            (setf oxygen (find-numbers-vs-criteria-at-pos oxygen #\0 i))
            (setf oxygen (find-numbers-vs-criteria-at-pos oxygen #\1 i)))))))

(defun resolve-co2 (input)
  (let ((length-inputs (length (first input)))
        (co2 input))
    (do ((i 0 (1+ i)))
        ((or (= i length-inputs) (= 1 (length co2))) (first co2))
      (let ((occurences (nr-occurence-0-1-criteria-at-pos co2 i)))
        (if (<= (first occurences) (second occurences))
            (setf co2 (find-numbers-vs-criteria-at-pos co2 #\0 i))
            (setf co2 (find-numbers-vs-criteria-at-pos co2 #\1 i)))))))


(nr-occurence-0-1-criteria-at-pos input-test 0)

(find-numbers-vs-criteria-at-pos input-test #\0 4)

(remove-if-not #'(lambda(x) (string= "1" x))
               (list "1" "a" "b" "1" "c"))




(resolve-oxygen input-test) ;; "10111"
(resolve-co2 input-test) ;; "01010"
(reduce #'* (mapcar #'(lambda (x) (parse-integer x :radix 2)) (list (resolve-oxygen input-test) (resolve-co2 input-test))))

(let ((input (file->lst *file*)))
 (reduce #'* (mapcar #'(lambda (x) (parse-integer x :radix 2)) (list (resolve-oxygen input) (resolve-co2 input))))) ;;3368358



;;ugly testing
;; (defun count-1 (str)
;;   (map 'list #'(lambda (x)
;;                    (if (char= #\1 x)
;;                        1
;;                        0))
;;        str))

;; (reduce #'(lambda (v str)
;;               (setf v (mapcar #'+ v (count-1 str))))
;;         input-test :initial-value (list 0 0 0 0 0)) ;; (7 5 8 7 5)


;; (let* ((mid (ash (length input-test) -1))
;;        (nr
;;          (parse-integer (map 'string #'(lambda(x)
;;                            (if (< x mid)
;;                                #\0
;;                                #\1))
;;                (list 7 5 8 7 5)) :radix 2) ))
;;   (reduce #'* (list nr (- #b11111 nr))))


;; (let* ((lst (file->lst *file*))
;;        (mid (ash (length lst) -1))
;;        (wynik (reduce #'(lambda (v str)
;;                           (setf v (mapcar #'+ v (count-1 str) )))
;;         lst :initial-value (list 0 0 0 0 0 0 0 0 0 0 0 0 )))
;;        (nr
;;          (parse-integer (map 'string #'(lambda(x)
;;                            (if (< x mid)
;;                                #\0
;;                                #\1))
;;                              wynik) :radix 2) ))
;;   (reduce #'* (list nr (- #b111111111111 nr)))) ;; 2972336
