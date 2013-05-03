#!/usr/bin/sbcl --script

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(with-output-to-string (*standard-output*)
  (ql:quickload :ironclad)
  (ql:quickload :babel))

(defconstant +bufsize+ (expt 2 16))

(defun multiply (list times)
  (reduce #'append (loop for i from 1 to times collect list)))

(defun vector-to-list (vector)
  (loop for i from 0 below (length vector) collect (aref vector i)))

(defun make-buffer (size)
  (let ((buffer (make-array (list size) :element-type 'character))
        (digester (ironclad:make-digest :md5))
        (i 0))
    (defun write-char-to-buffer (ch)
      (when (= i size)
        (flush-buffer))
      (setf (aref buffer i) ch)
      (incf i))

    (defun flush-buffer ()
      (ironclad:update-digest digester (ironclad:ascii-string-to-byte-array (subseq buffer 0 i)))
      (setf i 0))

    (defun digest ()
      (ironclad:produce-digest digester))))

(defun parse (string)
  (make-buffer +bufsize+)
  (labels ((read-aux (s l i)
             (declare (type fixnum i)
                      (type fixnum l)
                      (type string s))
             (when (and i (< i l))
               (let ((ch (schar s i)))
                 (cond
                   ((alpha-char-p ch)
                    (write-char-to-buffer ch)
                    (read-aux s l (1+ i)))
                   ((digit-char-p ch)
                    (multiple-value-bind (n r)
                        (parse-integer s :start i :junk-allowed t)
                      (let (pos)
                        (dotimes (k n)
                          (setf pos (read-aux s l (1+ r))))
                        (read-aux s l pos))))
                   ((char= ch #\]) (1+ i)))))))
    (read-aux string (length string) 0))
  (flush-buffer)
  (digest))

(defun main ()
  (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
      ((eq line 'eof))
    (format t "~{~(~2,'0x~)~}~%" (vector-to-list (parse line)))))

(main)
