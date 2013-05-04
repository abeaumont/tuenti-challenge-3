#!/usr/bin/sbcl --script

(defun split (string)
  (let ((tokens
         (loop for i = 0 then (1+ j)
            as j = (position #\Space string :start i)
            collect (subseq string i j)
            while j)))
    (remove "" tokens :test #'equalp)))

(defun read-number-list ()
  (mapcar #'parse-integer (split (read-line))))

(defun read-number ()
  (parse-integer (read-line)))

(defun read-numbers (n)
  (let ((line (read-line))
        (numbers (make-array (list n) :element-type 'fixnum)))
    (loop for i from 0 below n
       for j = 0 then j
       do (multiple-value-bind (k r) (parse-integer line :start j :junk-allowed t)
            (setf j r)
            (setf (aref numbers i) k))
       finally (return numbers))))

(defun build-repetitions (numbers)
  (let ((h (make-hash-table :test #'eq)))
    (dotimes (i (length numbers))
      (incf (gethash (aref numbers i) h 0)))
    h))

(defun order-repetitions (repetitions)
  (let ((number-repetitions (make-array '(0) :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v) (vector-push-extend (cons k v) number-repetitions)) repetitions)
    (sort number-repetitions #'> :key #'cdr)))

(defun max-position (start end seq &optional (from 0))
  (position-if (lambda (x) (and (>= x start) (<= x end))) seq :key #'car :start from))

(defun main ()
  (dotimes (i (read-number))
    (format t "Test case #~d~%" (1+ i))
    (destructuring-bind (n m) (read-number-list)
      (let* ((numbers (read-numbers n))
             (repetitions (build-repetitions numbers))
             (ordered (order-repetitions repetitions)))
        (dotimes (j m)
          (destructuring-bind (start end) (read-number-list)
            (let* ((start-number (aref numbers (1- start)))
                   (end-number (aref numbers (1- end)))
                   (start-repetitions (gethash start-number repetitions))
                   (end-repetitions (gethash end-number repetitions)))
              (loop for k from (- start 2) downto 0
                 while (= (aref numbers k) start-number)
                 do (decf start-repetitions))
              (loop for k from end below (length numbers)
                 while (= (aref numbers k) end-number)
                 do (decf end-repetitions))
              (format t "~d~%"
                      (let ((pos (max-position start-number end-number ordered))
                            (size (length ordered)))
                        (cond
                          ((= (car (aref ordered pos)) start-number)
                           (let ((next-pos (max-position start-number end-number ordered (1+ pos))))
                             (cond
                               ((null next-pos)
                                start-repetitions)
                               ((= (car (aref ordered next-pos)) end-number)
                                (let ((last-pos (max-position start-number end-number ordered (1+ next-pos))))
                                  (if last-pos
                                      (max start-repetitions end-repetitions (cdr (aref ordered last-pos)))
                                      (max start-repetitions end-repetitions))))
                               (t (cdr (aref ordered pos))))))
                          ((= (car (aref ordered pos)) end-number)
                           
                           (let ((next-pos (max-position start-number end-number ordered (1+ pos))))
                             (cond
                               ((null next-pos)
                                end-repetitions)
                               ((= (car (aref ordered next-pos)) start-number)
                                (let ((last-pos (max-position start-number end-number ordered (1+ next-pos))))
                                  (if last-pos
                                      (max start-repetitions end-repetitions (cdr (aref ordered last-pos)))
                                      (max start-repetitions end-repetitions))))
                               (t (cdr (aref ordered pos))))))
                           (t (cdr (aref ordered pos)))))))))))))

(main)
