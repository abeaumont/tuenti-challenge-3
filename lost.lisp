#!/usr/bin/sbcl --script
;;; helper functions
(defun split (string &key (remove-if-empty t) (tokens '(#\Space)))
  (let ((tokens
         (loop for i = 0 then (1+ j)
            as j = (position-if (lambda (x) (member x tokens)) string :start i)
            when (> i 0) collect (subseq string (1- i) j)
            while j)))
    (if remove-if-empty
        (remove nil tokens)
        tokens)))

(defun read-script (&optional (read-fn #'read-line))
  (split (funcall read-fn) :tokens '(#\< #\> #\.)))

(defun read-number (&optional (read-fn #'read-line))
  (parse-integer (funcall read-fn)))

(defun maph (fn hash)
  (let (result)
    (maphash (lambda (k v)
               (push (funcall fn k v) result)) hash)
    result))

(defun reorder-script (strings present past future)
  (maphash (lambda (k v)
             (when (gethash k past)
               (setf (gethash k present) (- v 1/2))
               (remhash k past)
               (remhash k future)))
           future)
  (let* ((pr (mapcar #'cdr (sort (maph (lambda (k v) (cons v k)) present)
                                 (lambda (x y) (> (car x) (car y))))))
         (pa (remove nil (maph (lambda (k v)
                                 (declare (ignore v))
                                 (unless (gethash k present) k)) past)))
         (fu (remove nil (maph (lambda (k v)
                                 (declare (ignore v))
                                 (unless (gethash k present) k)) future))))
    ;; More than one way to begin or finish script
    (if (or (> (length pa) 1) (> (length fu) 1))
        "valid"
        (let ((script fu))
          (loop for item in pr
             do (push item script))
          (format nil "~{~a~^,~}" (mapcar (lambda (x) (gethash x strings)) (append pa script)))))))

(defun process-script (script)
  (let ((present (make-hash-table :test #'eq))
        (past (make-hash-table :test #'eq))
        (future (make-hash-table :test #'eq))
        (strings (make-hash-table :test #'eq))
        (index 0))
    (dolist (scene script)
      (let* ((string (subseq scene 1))
             (name (intern string)))
        (setf (gethash name strings) string)
        (case (char scene 0)
          ((#\>)
           ;; future, look if this scene happened in the past
           (when (gethash name past)
             (return-from process-script "invalid"))
           ;; future, always overwrite
           (setf (gethash name future) index))
          ((#\<)
           ;; past, set only if new
           (unless (gethash name past)
             (setf (gethash name past) index)))
          (otherwise
           ;; present, duplicated scenes? This is not Doctor Who!
           (let ((i (gethash name present)))
             (when (and i (integerp i))
               (return-from process-script "invalid")))
           (setf (gethash name present) index)
           (incf index)))))
    (cond
      ;; A past scene appears later in present
      ((find t (maph (lambda (k v) (<= v (gethash k present 0))) past))
       "invalid")
      ;; A future scene appears before in present
      ((find t (maph (lambda (k v) (> v (gethash k present (1+ v)))) future))
       "invalid")
      ;; If a scene appears both as future and past they cannot be at the same
      ;; position
      ((find t (maph (lambda (k v)
                       (let ((i (gethash k future)))
                         (and i (= i v))))
                     past))
       "invalid")
      ;; A past scene does not have a present nor a future and it is not first
      ((find t (maph (lambda (k v)
                       (not (or (gethash k present)
                                (gethash k future)
                                (<= v 1))))
                     past))
       "valid")
      ;; A future scene dos not have a present and it is not last
      ((find t (maph (lambda (k v)
                       (not (or (gethash k present)
                                (gethash k past)
                                (>= v (1- (hash-table-count present))))))
                     future))
       "valid")
      ;; If a scene appears both as future and past and they are not
      ;; contiguous ordering is ambiguous
      ((find t (maph (lambda (k v)
                       (let ((i (gethash k future)))
                         (and i (> (- v i) 1))))
                     past))
       "invalid")
      (t (reorder-script strings present past future)))))

(defun main ()
  (dotimes (i (read-number))
    (format t "~a~%" (process-script (read-script)))))

(main)
