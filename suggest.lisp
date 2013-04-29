#!/usr/bin/sbcl --script

;;;; Since dictionary files are big and there is no information about how long
;;;; will word list be, we will assume that it is better to parse dictionary
;;;; file just once. For each dictionary entry we make a series of checks to
;;;; see if that entry should be a suggestion or not for each of the words in
;;;; the input.
;;;;
;;;; To parse dictionary faster we chain shortcut checks to reduce the amount
;;;; of false positives for final comparison. Checks are done in the following
;;;; order:
;;;;
;;;; - Check that length of words matches
;;;; - Check that sum of characters in words matches (poor's man hash)
;;;; - Sort and compare words
;;;; - Check that words are not the same (doesn't make sense to suggest the
;;;;   same word)
;;;;
;;;; Some additional requisites considered:
;;;; - Comparisons are case insensitive and alphabetical order is case
;;;;   insensitive since it is not stated whether input file or dictionaries
;;;;   may contain characters in different case or not.
;;;; - Duplicates are removed, since it is not stated whether dictionaries may
;;;;   contain duplicates or not.

;;; helper functions
(defun split (string &key (remove-if-empty t))
  (let ((tokens
         (loop for i = 0 then (1+ j)
            as j = (position #\Space string :start i)
            collect (subseq string i j)
            while j)))
    (if remove-if-empty
        (remove nil tokens)
        tokens)))

(defun read-valid-line ()
  (loop for line = (read-line)
     while (and (not (zerop (length line)))
                (char= (char line 0) #\#))
     finally (return line)))

(defun read-string-list (&optional (read-fn #'read-valid-line))
  (split (funcall read-fn)))

(defun read-string (&optional (read-fn #'read-valid-line))
  (first (read-string-list read-fn)))

(defun read-number-list (&optional (read-fn #'read-valid-line))
  (mapcar #'parse-integer (split (funcall read-fn))))

(defun read-number (&optional (read-fn #'read-valid-line))
  (first (read-number-list read-fn)))

;;; task code
(defun read-input ()
  (let ((dictionary (read-string))
        (n (read-number)))
    (values dictionary
            (loop for i from 1 to n
               collect (read-string)))))

(defun string-value (string)
  (apply #'+ (map 'list #'char-code string)))

(defun string-sort (string)
  (sort (copy-seq string) #'char-lessp))

(defun word-parameters (word)
  (values (length word)
          (string-value word)
          (string-sort word)
          word))

;; This is the time consuming task.
(defun process-entry (entry parameters suggestions)
  (dotimes (i (array-dimension parameters 0))
    (let ((word-parameters (aref parameters i)))
      (destructuring-bind (l v s w) word-parameters
        (when (and (= (length entry) l)
                   (= (string-value entry) v)
                   (string-equal (string-sort entry) s)
                   (not (string-equal entry w)))
          (push entry (aref suggestions i)))))))

(defun process-dictionary (dict fn)
  (with-open-file (in dict)
    (loop for line = (read-line in nil)
       while line do (funcall fn line))))

(defun main ()
  (multiple-value-bind (dict words) (read-input)
    (let* ((size (length words))
           (parameters (make-array size :element-type 'list :initial-element nil))
           (suggestions (make-array size :element-type 'list :initial-element nil)))
      (loop for i from 0
         for word in words
         do (setf (aref parameters i) (multiple-value-list (word-parameters word))))
      (process-dictionary dict (lambda (entry) (process-entry entry parameters suggestions)))
      (loop for i from 0
         for word in words
         do (format t "~a -> ~{~a~^ ~}~%" word
                    (sort (remove-duplicates (aref suggestions i) :test #'string-equal)
                          #'string-lessp))))))

(main)
