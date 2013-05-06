#!/usr/bin/sbcl --script

(defun split (string)
  (remove ""
          (loop for i = 0 then (1+ j)
             as j = (position #\# string :start i)
             collect (subseq string i j)
             while j)
          :test #'string=))

(defun numbers (tokens)
  (mapcar (lambda (s) (parse-integer s :radix 2)) tokens))

(defun calculate (numbers)
  (* (first numbers) (apply #'+ (rest numbers))))

(defun main ()
  (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
      ((eq line 'eof))
    (let* ((tokens (split line))
           (l (length (first tokens)))
           (numbers (numbers tokens)))
      (format t
              (concatenate 'string "#~" (format nil "~d" l) ",'0b#~%")
              (calculate numbers)))))

(main)     
