#!/usr/bin/sbcl --script

(defstruct point x y)

(defun new-point (x y)
  (make-point :x x :y y))

(defstruct coordinate point cost value from path)

(defun new-coordinate (point cost value &optional from path)
  (make-coordinate :point point
                   :cost cost
                   :value value
                   :from from
                   :path path))

(defun split (string &key (remove-if-empty t) (tokens '(#\Space)))
  (let ((tokens
         (loop for i = 0 then (1+ j)
            as j = (position-if (lambda (x) (member x tokens)) string :start i)
            collect (subseq string i j)
            while j)))
    (if remove-if-empty
        (remove nil tokens)
        tokens)))

(defun parse-number-list (string)
  (mapcar #'parse-integer (split string :tokens '(#\,))))

(defun read-number-list ()
  (parse-number-list (read-line)))

(defun read-number (&optional (read-fn #'read-line))
  (parse-integer (funcall read-fn)))

(defun read-gems ()
  (mapcar #'parse-number-list (split (read-line) :tokens '(#\#))))

(defun read-dungeon ()
  (let ((size (read-number-list))
        (position (read-number-list)))
    (let ((point (new-point (first position) (second position)))
          (max-cost (read-number))
          (gem-number (read-number))
          (gem-list (build-gems (read-gems))))
      (values
       (new-coordinate point 0 (gem-value point gem-list) nil (cons point nil))
       max-cost
       gem-list
       (first size)
       (second size)))))

(defun build-gems (gems)
  (mapcar (lambda (gem)
            (destructuring-bind (x y value) gem
              (cons (new-point x y) value)))
          gems))

(defun gem-value (point gems &optional path)
  (if (and path (find point path :test #'equalp))
      0
      (let ((gem (assoc point gems :test #'equalp)))
        (if gem
            (cdr gem)
            0))))

(defun adjacent-points (point from)
  (let ((x (point-x point))
        (y (point-y point)))
    (remove from
            (list (new-point (1+ x) y)
                  (new-point (1- x) y)
                  (new-point x (1+ y))
                  (new-point x (1- y)))
            :test #'equalp)))

(defun adjacents (current gems)
  (let ((point (coordinate-point current))
        (cost (coordinate-cost current))
        (value (coordinate-value current))
        (from (coordinate-from current))
        (path (coordinate-path current)))
    (mapcar (lambda (p)
              (new-coordinate p (1+ cost) (+ value (gem-value p gems path)) point (cons p path)))
            (adjacent-points point from))))

(defun find-coordinates (coordinate coordinates)
  (remove-if-not (lambda (c) (equalp (coordinate-point coordinate)
                                (coordinate-point c)))
                 coordinates))

(defun remove-coordinate-p (coordinate others)
  (find t (mapcar (lambda (other)
                    (and (>= (coordinate-value other)
                             (coordinate-value coordinate))
                         (<= (coordinate-cost other)
                             (coordinate-cost coordinate))))
                  others)))

(defun filter-adjacents (adjacents coordinates size-x size-y)
  (remove-if (lambda (c)
               (let ((x (point-x (coordinate-point c)))
                     (y (point-y (coordinate-point c)))
                     (others (find-coordinates c coordinates)))
                 (or (< x 0)
                     (< y 0)
                     (>= x size-x)
                     (>= y size-y)
                     (and others (remove-coordinate-p c others)))))
             adjacents))

(defun next-adjacents (current next gems size-x size-y)
  (let ((points (make-hash-table :test #'equalp))
        (adjacents (mapcar (lambda (adjacent)
                             (filter-adjacents (adjacents adjacent gems)
                                               current size-x size-y))
                           next)))
    (dolist (candidates adjacents)
      (dolist (c candidates)
        (let ((c-list (gethash (coordinate-point c) points)))
          (unless (remove-coordinate-p c c-list)
            (push c (gethash (coordinate-point c) points))))))
    (apply #'append (loop for value being the hash-values of points
                       collect value))))

(defun build-coordinate-list (initial max-cost gems size-x size-y)
  (loop for i from 0 to max-cost
     for current-adjacents = (list initial) then (append next current-adjacents)
     for next = current-adjacents then (next-adjacents current-adjacents
                                                       next gems size-x size-y)
     finally (return (append current-adjacents next))))


(defun max-value (initial max-cost gems size-x size-y)
  (coordinate-value (reduce (lambda (x y)
                              (if (>= (coordinate-value x)
                                      (coordinate-value y))
                                  x y))
                            (build-coordinate-list initial max-cost gems size-x size-y)
                            :initial-value initial)))

(defun main ()
  (dotimes (i (read-number))
    (format t "~d~%" (multiple-value-call #'max-value (read-dungeon)))))

(main)
