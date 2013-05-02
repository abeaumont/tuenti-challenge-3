#!/usr/bin/sbcl --script

(defun split (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))
  
(defun read-number ()
  (parse-integer (read-line)))

(defun read-number-list ()
  (mapcar #'parse-integer (split (read-line))))

(defun survival-time (width height soldiers)
  (when (< soldiers width)
    (let* ((area (* width height))
           (n (- width soldiers))
           (time (ceiling (/ (1+ (- area width)) n))))
      (values time
              (- (+ (* time n) width) area)))))

(defun best-battle (width height soldier-cost crematorium-cost gold)
  (loop for i from 0 to (floor (/ gold crematorium-cost))
     for remaining = (- gold (* i crematorium-cost))
     for soldiers = (floor (/ remaining soldier-cost))
     for time = (survival-time width height soldiers)
     for result = (and time (* (1+ i) time))
     for best = result then (and result (max best result))
     while result
     finally (return best)))

(defun main ()
  (dotimes (i (read-number))
    (let ((b (apply #'best-battle (read-number-list))))
      (format t "~d~%" (if b b -1)))))

(main)
