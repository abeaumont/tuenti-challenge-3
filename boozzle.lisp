#!/usr/bin/sbcl --script

(defconstant +max-length+ 15)
(defconstant +dictionary-pathname+ "boozzle-dict.txt")

(defstruct point x y)

(defun new-point (x y)
  (make-point :x x :y y))

(defstruct w string value wm path)

(defun new-w (string value wm path)
  (make-w :string string
          :value value
          :wm wm
          :path path))

(defun word-value (word)
  (+ (* (w-value word) (w-wm word)) (length (w-string word))))

(defstruct cell ch cm wm)

(defun new-cell (ch &optional (cm 1) (wm 1))
  (make-cell :ch ch :cm cm :wm wm))

(defun split (string &key (remove-if-empty t) (tokens '(#\Space)))
  (let ((tok (loop for i = 0 then (1+ j)
                as j = (position-if (lambda (x) (member x tokens)) string :start i)
                collect (subseq string i j)
                while j)))
    (if remove-if-empty (remove "" tok :test #'equalp) tok)))

(defun maph (fn hash)
  (let (result)
    (maphash (lambda (k v)
               (push (funcall fn k v) result)) hash)
    result))

(defun read-number ()
  (parse-integer (read-line)))

(defun read-values ()
  (let ((h (make-hash-table))
        (letters (split (subseq (read-line) 1) :tokens '(#\,))))
    (dolist (l letters)
      (destructuring-bind (ch v) (split l :tokens '(#\: #\Space))
        (setf (gethash (char (first (split ch :tokens '(#\'))) 0) h)
              (parse-integer v :junk-allowed t))))
    h))

(defun parse-cell (string)
  (destructuring-bind (sch stype smultiplier) (map 'list #'string string)
    (let ((ch (char sch 0))
          (type (parse-integer stype))
          (multiplier (parse-integer smultiplier)))
      (if (= type 1)
          (values ch multiplier 1)
          (values ch 1 multiplier)))))
  
(defun read-cells (x y)
  (let ((cells (make-array (list x y))))
    (dotimes (i x)
      (let ((cell-row (split (read-line))))
        (dotimes (j y)
          (setf (aref cells i j)
                (multiple-value-call #'new-cell
                  (parse-cell (nth j cell-row)))))))
    cells))

(defun read-dictionary (pathname)
  (let ((h (make-hash-table :test #'equalp)))
    (with-open-file (in pathname)
      (do ((line (read-line in) (read-line in nil 'eof)))
          ((eql line 'eof) h)
        (setf (gethash line h) t)))
    h))

(defun read-game ()
  (let* ((wordh (make-hash-table :test #'equalp))
         (letters (read-values))
         (time (read-number))
         (x-size (read-number))
         (y-size (read-number))
         (cells (read-cells x-size y-size)))
    (values wordh letters time x-size y-size cells)))

;;; Part of the magic goes here. Not only build a dictionary of words, but for
;;; also build a dictionary of prefixes per word length (starting from 2),
;;; with a list of possible next characters.
;;; This will make the word builder much smarter.
(defun build-prefixes (dictionary size)
  (let ((v (make-array size)))
    (dotimes (i size)
      (setf (svref v i) (make-hash-table :test #'equalp)))
    (maphash (lambda (k _)
               (declare (ignore _))
               (loop for i from 2 below (length k)
                  do (pushnew (char k i) (gethash (subseq k 0 i) (svref v (- i 2))))))
             dictionary)
    v))

(defun point-char (point)
  (cell-ch (point-cell point)))

(defun adjacent-points (point path)
  (let ((x (point-x point))
        (y (point-y point)))
    (remove-if (lambda (p) (or (not (valid-point-p p)) (member p path :test #'equalp)))
               (list (new-point (1+ x) y)
                     (new-point (1- x) y)
                     (new-point x (1+ y))
                     (new-point x (1- y))
                     (new-point (1+ x) (1+ y))
                     (new-point (1+ x) (1- y))
                     (new-point (1- x) (1+ y))
                     (new-point (1- x) (1- y))))))

(defun try-words (word)
  (let* ((path (w-path word))
         (words (mapcar (lambda (p)
                          (extend-word word p))
                        (remove-if-not (lambda (p)
                                         (prefixp (w-string word) p))
                                       (adjacent-points (car path) path)))))
    (dolist (w words)
      (try-word w))
    words))

(defun next-words (words)
  (apply #'append (mapcar #'try-words words)))

(defun build-words (initial max)
  (let ((initial-list (list (create-word initial))))
    (loop for i from 1 to max
       for words = initial-list then (next-words words)
       while words)))

(defun knapsack (weight-values max-weight)
  (let* ((n (length weight-values))
         (m (make-array (list (1+ n) (1+ max-weight)) :initial-element 0)))
    (loop for i from 1 to n
       for (w v) in weight-values
       do (dotimes (j (1+ max-weight))
            (setf (aref m i j)
                  (if (>= j w)
                      (max (aref m (1- i) j) (+ (aref m (1- i) (- j w)) v))
                      (aref m (1- i) j)))))
    (aref m n max-weight)))

(defun main ()
  (let* ((dictionary (read-dictionary +dictionary-pathname+))
         (prefixes (build-prefixes dictionary (- +max-length+ 2))))
    (dotimes (i (read-number))
      (multiple-value-bind (wordh letters time x-size y-size cells) (read-game)

        (defun point-cell (point)
          (aref cells (point-x point) (point-y point)))

        (defun create-word (p)
          (let* ((cell (point-cell p))
                 (ch (cell-ch cell))
                 (s (string ch)))
            (new-w s (* (cell-cm cell) (gethash ch letters)) (cell-wm cell) (cons p nil))))

        (defun valid-point-p (p)
          (let ((x (point-x p))
                (y (point-y p)))
            (and (>= x 0)
                 (>= y 0)
                 (< x x-size)
                 (< y y-size))))

        (defun prefixp (string p)
          (let ((i (- (length string) 2)))
            (or (< i 0)
                (member (point-char p)
                        (gethash string (svref prefixes i))
                        :test #'equalp))))
          
        (defun extend-word (word point)
          (let ((cell (point-cell point)))
            (let ((s (w-string word))
                  (v (w-value word))
                  (p (w-path word))
                  (w (w-wm word))
                  (ch (cell-ch cell))
                  (cm (cell-cm cell))
                  (wm (cell-wm cell)))
              (new-w (concatenate 'string s (string ch))
                     (+ v (* (gethash ch letters) cm))
                     (max w wm)
                     (cons point p)))))

        (defun try-word (word)
          (let ((s (w-string word)))
            (when (gethash s dictionary)
              (let ((w (gethash s wordh)))
                (when (or (not w)
                          (< (word-value w) (word-value word)))
                  (setf (gethash s wordh) word))))))

        (defun word-values ()
          (maph (lambda (k v)
                  (list (1+ (length k)) (word-value v)))
                wordh))

        (defun inner-main ()
          (dotimes (i x-size)
            (dotimes (j y-size)
              (build-words (new-point i j) (min +max-length+ (1- time)))))
          (knapsack (word-values) time)))
      (format t "~d~%" (inner-main)))))

(main)
