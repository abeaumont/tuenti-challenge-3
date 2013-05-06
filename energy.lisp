#!/usr/bin/sbcl --script

(defun split (string)
  (remove ""
          (loop for i = 0 then (1+ j)
             as j = (position #\Space string :start i)
             collect (subseq string i j)
             while j)
          :test #'string=))

(defun read-number-list ()
  (mapcar #'parse-integer (split (read-line))))

(defun read-number ()
  (parse-integer (read-line)))

(defun main ()
  (dotimes (_ (read-number))
    (let ((h (make-hash-table :size (read-number)))
          (sources nil)
          (destinations nil))

      (defun add-path (source destination energy)
        (let ((dh (gethash source h)))
          (if dh
              (let ((e (gethash destination dh)))
                (when (or (not e) (< e energy))
                  (setf (gethash destination dh) energy)))
              (let ((dh (make-hash-table)))
                (setf (gethash destination dh) energy)
                (setf (gethash source h) dh)))))

      (defun merge-path (origin source destination energy)
        (let* ((dh (gethash origin h))
               (e (gethash source dh)))
          (when e
            (add-path origin destination (* e energy)))))

      (defun search-path (origin source merged)
        (let ((d (gethash source h)))
          (when (and d (not (gethash source merged)))
            (setf (gethash source merged) t)
            (maphash (lambda (k v) (merge-path origin source k v)) d))))
      
      (defun search-paths (origin dh merged)
        (maphash (lambda (k _)
                   (search-path origin k merged)) dh))

      (dotimes (_ (read-number))
        (destructuring-bind (s d e) (read-number-list)
          (pushnew s sources)
          (pushnew d destinations)
          (add-path s d (/ (+ 100 e) 100))))

      (let ((merged (make-hash-table)))
        (dolist (k (intersection sources destinations))
          (setf (gethash k merged) t)
          (search-paths k (gethash k h) merged)))

      (let ((result "False"))
        (block nil
          (maphash (lambda (k v)
                     (let ((e (gethash k v)))
                       (when (and e (> e 1))
                         (setf result "True")
                         (return))))
                   h))
        (format t "~a~%" result)))))

(main)
