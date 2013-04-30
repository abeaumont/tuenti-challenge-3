#!/usr/bin/sbcl --script

(defstruct point x y)

(defun new-point (x y)
  (make-point :x x :y y))

(defstruct coordinate point cost from)

(defun new-coordinate (point cost &optional from)
  (make-coordinate :point point
                   :cost cost
                   :from from))

;; reader functions
(defun split (string &key (remove-if-empty t))
  (let ((tokens
         (loop for i = 0 then (1+ j)
            as j = (position #\Space string :start i)
            collect (subseq string i j)
            while j)))
    (if remove-if-empty
        (remove nil tokens)
        tokens)))

(defun read-number-list ()
  (mapcar #'parse-integer (split (read-line))))

(defun read-number ()
  (parse-integer (read-line)))

(defun read-map (x y)
  (let ((map (make-array `(,x ,y)))
        start end)
    (dotimes (j y)
      (let ((row (read-line)))
        (dotimes (i x)
          (let ((type (case (char row i)
                        ((#\#) 'wall)
                        ((#\X) 'start)
                        ((#\O) 'end)
                        (otherwise 'ice))))
            (when (eq type 'start)
              (setq start (new-point i j))
              (setq type 'ice))
            (when (eq type 'end)
              (setq end (new-point i j))
              (setq type 'ice))
            (setf (aref map i j) type)))))
    (values start end map)))

;; movement logic
(defun try-movements (coordinate)
  (remove nil (mapcar (lambda (direction)
                        (try-movement coordinate direction))
                      '(:n :e :s :w))))

(defun next-coordinates (coordinates)
  (apply #'append (mapcar #'try-movements coordinates)))

(defun distance (p1 p2)
  "Distance between two points in the same row ro column"
  (+ (abs (- (point-x p1)
             (point-x p2)))
     (abs (- (point-y p1)
             (point-y p2)))))

(defun opposite-direction (direction)
  (case direction
    (:e :w)
    (:w :e)
    (:n :s)
    (:s :n)))
  
(defun main ()
  (dotimes (i (read-number))
    (destructuring-bind (x y speed time) (read-number-list)
      (multiple-value-bind (start end map) (read-map x y)
        (let ((points (make-hash-table :test #'equalp))
              (initial (new-coordinate start 0)))

          ;; make use of lexical closures to avoid passing so many parameters
          (defun find-movement (point direction)
            (let ((i (point-x point))
                  (j (point-y point)))
              (case direction
                (:e (loop for index from (1+ i)
                       while (and (< index x)
                                  (eq (aref map index j) 'ice))
                       finally (return (new-point (1- index) j))))
                (:w (loop for index from (1- i) downto -1
                       while (and (>= index 0)
                                  (eq (aref map index j) 'ice))
                       finally (return (new-point (1+ index) j))))
                (:n
                 (loop for index from (1- j) downto -1
                    while (and (>= index 0)
                               (eq (aref map i index) 'ice))
                    finally (return (new-point i (1+ index)))))
                (:s (loop for index from (1+ j)
                       while (and (< index y)
                                  (eq (aref map i index) 'ice))
                       finally (return (new-point i (1- index))))))))

          (defun try-movement (coordinate direction)
            (block nil
              (let ((point (coordinate-point coordinate))
                    (cost (coordinate-cost coordinate))
                    (from (coordinate-from coordinate)))
                (unless (eq direction from)
                  (let* ((p (find-movement point direction))
                         (c (new-coordinate p
                                            (+ cost time (/ (distance p point) speed))
                                            (opposite-direction direction))))
                    (when (equalp p point) (return))
                    (let ((prev (gethash p points)))
                      (when (and prev (<= (coordinate-cost prev) (coordinate-cost c)))
                        (return))
                      (setf (gethash p points) c)
                      (if (equal p end)
                          nil
                          c)))))))

          (setf (gethash start points) initial)
          (loop for coordinates = (list initial) then (next-coordinates coordinates)
             while coordinates)
          (format t "~d~%" (round (coordinate-cost (gethash end points)))))))))

(main)
