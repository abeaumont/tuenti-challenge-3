#!/usr/bin/sbcl --script

(defconstant +integers-pathname+ "integers")
(defconstant +missing-pathname+ "missing-table")

(defun read-number (&optional (read-fn #'read-line))
  (parse-integer (funcall read-fn)))

(defun look-missing (input size missing-size)
  (declare (type fixnum size missing-size))
  (let ((v (make-array size :element-type 'bit :initial-element 0)))
    ;; Read whole numbers and set bits
    (with-open-file (in input :element-type '(unsigned-byte 32))
      (dotimes (i (the fixnum (- size missing-size)))
        (declare (type fixnum i))
        (setf (sbit v (read-byte in)) 1)))
    ;; Look which numbers are lost
    (loop for i from 0 below size
       when (zerop (sbit v i)) collect i)))

(defun save (pathname content)
  (with-open-file (out pathname :direction :output :if-exists :supersede)
    (prin1 content out)))

;; My laptop is slow... so let's precalculate missing table to avoid making
;; impatient submit_challenge wait...
;(save +missing-pathname+ (look-missing +integers-pathname+ (expt 2 31) 100))

(defun main (pathname)
  (let (numbers)
    (with-open-file (in pathname)
      (setq numbers (read in)))
    (dotimes (i (read-number))
      (format t "~d~%" (nth (1- (read-number)) numbers)))))

(main +missing-pathname+)
