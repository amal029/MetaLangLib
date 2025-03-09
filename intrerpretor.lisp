(defpackage :interpretor
  (:use :clang :rules :cl))

(in-package :interpretor)


;; Trying the type of class I want for the interpreter .GF is the hash
;; table of symbol name to function body. GV is is the hash map from
;; variable name (symbol) to (type . value)
(defclass interpret (rewrite)
  ((GF :accessor :gf :initform (make-hash-table))
   (GV :accessor :gv :initform (make-hash-table))
   (Others :accessor :others :initform '() :type list)))


;; Example of reading a file with the code
(defvar value)
(with-open-file (fd (merge-pathnames "test.mlisp") :direction :input)
  (setq value (loop for i = (read-preserving-whitespace fd nil nil)
		    while i collect i into value
		    finally (return value))))
(print (car value))
;; Now start processing the read value
