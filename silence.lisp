#!/usr/bin/sbcl --script

(defun factorial (n)
  (let ((fact 1))
    (loop for i from 2 to n
       do (setf fact (* fact i)))
    fact))

(defun sum-digits (string)
  (loop for i from 0 below (length string)
       as j = (- (char-code (schar string i)) 48)
     for acc = j then (+ acc j)
     finally (return acc)))

(defun main ()
  (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
      ((eq line 'eof))
    (format t "~d~%" (sum-digits
                      (format nil "~d" (factorial (parse-integer line)))))))

(main)
