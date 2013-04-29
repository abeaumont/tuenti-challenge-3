#!/usr/bin/sbcl --script

;; helper functions
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
  (first (read-number-list)))

(defun relative-max-min (l)
  "Get relative maximum and minimum values in a list L."
  (remove nil
          (let ((test #'>))
            (mapcar (lambda (x y)
                      (unless (funcall test x y)
                        (setq test (complement test))
                        x))
                    (butlast l)
                    (cdr l)))))

(defun transactions (rates)
  "Get transaction value list"
  (let ((relative-points (relative-max-min rates)))
    (if (oddp (length relative-points))
        (append relative-points (last rates))
        relative-points)))

(defun operate (budget rates)
  "Operate with initial BUDGET to maximize profit limited by RATES."
  (flet ((floor/ (x y) (floor (/ x y))))
    (let ((operation #'floor/))
      (reduce (lambda (x y)
                (prog1 (funcall operation x y)
                  (if (eq operation #'floor/)
                      (setq operation #'*)
                      (setq operation #'floor/))))
              (transactions rates)
              :initial-value budget))))

(dotimes (i (read-number))
  (format t "~D~%" (operate (read-number) (read-number-list))))

