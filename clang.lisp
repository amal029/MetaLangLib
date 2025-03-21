(defpackage :clang
  (:use :rules :cl)
  (:export
   #:rewrite #:eval-seq #:ctx))

(in-package :clang)

;; --------------- Generic functions for eval------------
(defgeneric eval-var (ctx obj backend))
(defgeneric eval-lvar (ctx obj backend))
(defgeneric eval-seq (ctx obj backend))
(defgeneric eval-assign (ctx obj backend))
(defgeneric eval-type (ctx obj backend))
(defgeneric eval-prttype (ctx obj backend))
(defgeneric eval-structype (ctx obj backend))
(defgeneric eval-plus (ctx obj backend))
(defgeneric eval-sub (ctx obj backend))
(defgeneric eval-mult (ctx obj backend))
(defgeneric eval-div (ctx obj backend))
(defgeneric eval-mod (ctx obj backend))
(defgeneric eval-brackets (ctx obj backend))
(defgeneric eval-deref (ctx obj backend))
(defgeneric eval-address (ctx obj backend))
(defgeneric eval-defvar (ctx obj backend))
(defgeneric eval-defstruct (ctx obj backend))
(defgeneric eval-deftype (ctx obj backend))
(defgeneric eval-block (ctx obj backend))
(defgeneric eval-defun (ctx obj backend))
(defgeneric eval-funcall (ctx obj backend))
(defgeneric eval-if-else (ctx obj backend))
(defgeneric eval-and (ctx obj backend))
(defgeneric eval-or (ctx obj backend))
(defgeneric eval-not (ctx obj backend))
(defgeneric eval-lt (ctx obj backend))
(defgeneric eval-leq (ctx obj backend))
(defgeneric eval-gt (ctx obj backend))
(defgeneric eval-geq (ctx obj backend))
(defgeneric eval-leq (ctx obj backend))
(defgeneric eval-nothing (ctx obj backend))
(defgeneric eval-while (ctx obj backend))
(defgeneric eval-return (ctx obj backend))

;; Make a class on which the methods will be specialised
(defclass debug-backend ()
  ((my-stream :accessor my-stream
	      :initarg :my-stream
	      :initform (make-string-output-stream)
	      :documentation "Stream to produce the output to")
   (my-name :accessor my-name :initarg :my-name
	    :initform "Debug"
	    :documentation "What type of backend am i?")
   (my-map :accessor my-map
	   :initform (make-hash-table :size 100)
	   :initarg :my-map)))

;; Example of rewrite with inheriting from the debug class
(defclass rewrite (debug-backend) ())
(defparameter my-rewrite (make-instance 'rewrite))

;; -------------------------- Types --------------------
;; Normal types in the language -- specialised on the 3rd arg
;; Example (type int) => int
(defmethod eval-type (ctx obj (backend debug-backend))
  (declare (ignorable ctx))
  (format (my-stream backend) "~S " (car obj)))

;; Pointer types in the language
;; Example: (ptrtype (structtype T)) => struct T*
(defmethod eval-ptrtype (ctx obj (backend debug-backend))
  (apply-rule-set ctx 'types (car obj) backend)
  (format (my-stream backend) "*"))

;; Example (structtype T) => struct T
(defmethod eval-structype (ctx obj (backend debug-backend))
  (format (my-stream backend) "struct ~S" (car obj)))

;; ------------------------ Expressions --------------
;; Now write the rules for processing the expression above
(defmethod eval-plus (ctx obj (backend debug-backend))
  "Rule to print the + function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format (my-stream backend) "~S" '+)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

;; Now write the rules for processing the expression above
(defmethod eval-sub (ctx obj (backend debug-backend))
  "Rule to print the - function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format (my-stream backend) "~S" '-)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

;; Now write the rules for processing the expression above
(defmethod eval-mult (ctx obj (backend debug-backend))
  "Rule to print the * function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format (my-stream backend) "~S" '*)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

;; Now write the rules for processing the expression above
(defmethod eval-div (ctx obj (backend debug-backend))
  "Rule to print / function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format (my-stream backend) "~S" '/)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

;; Now write the rules for processing the expression above
(defmethod eval-mod (ctx obj (backend debug-backend))
  "Rule to print the % function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format (my-stream backend) "~S" '%)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

(defmethod eval-val (ctx obj (backend debug-backend))
  "Print the value"
  (format (my-stream backend) "~S" (car obj)))

(defmethod eval-var (ctx obj (backend debug-backend))
  "Print the variable name"
  (format (my-stream backend) "~S" (car obj)))

(defmethod eval-lvar (ctx obj (backend debug-backend))
  "Print the variable name"
  (format (my-stream backend) "~S" (car obj)))

(defmethod eval-brackets (ctx obj (backend debug-backend))
  "Print the brackets"
  (format (my-stream backend) "(")
  (apply-rule-set ctx 'expr (car obj) backend)
  (format (my-stream backend) ")"))

;; Example (deref x) => *x
(defmethod eval-deref (ctx obj (backend debug-backend))
  (format (my-stream backend) "*")
  ;; (print (car obj))
  (apply-rule-set ctx 'simple-expr (car obj) backend))

(defmethod eval-address (ctx obj (backend debug-backend))
  (format (my-stream backend) "&")
  (apply-rule-set ctx 'simple-expr (car obj) backend))

;; -----------------Logical Expressions-----------
(defmethod eval-and (ctx obj backend)
  (apply-rule-set ctx 'expr (car obj) backend)
  (format (my-stream backend) " && ")
  (apply-rule-set ctx 'expr (cadr obj) backend))

(defmethod eval-or (ctx obj backend)
  (apply-rule-set ctx 'expr (car obj) backend)
  (format (my-stream backend) " || ")
  (apply-rule-set ctx 'expr (cadr obj) backend))

(defmethod eval-not (ctx obj backend)
  (format (my-stream backend) " !( ")
  (apply-rule-set ctx 'expr (cadr obj) backend)
  (format (my-stream backend) ")"))

(defmethod eval-lt (ctx obj backend)
  (apply-rule-set ctx 'expr (car obj) backend)
  (format (my-stream backend) " < ")
  (apply-rule-set ctx 'expr (cadr obj) backend))

(defmethod eval-leq (ctx obj backend)
  (apply-rule-set ctx 'expr (car obj) backend)
  (format (my-stream backend) " <= ")
  (apply-rule-set ctx 'expr (cadr obj) backend))

(defmethod eval-gt (ctx obj backend)
  (apply-rule-set ctx 'expr (car obj) backend)
  (format (my-stream backend) " > ")
  (apply-rule-set ctx 'expr (cadr obj) backend))

(defmethod eval-geq (ctx obj backend)
  (apply-rule-set ctx 'expr (car obj) backend)
  (format (my-stream backend) " >= ")
  (apply-rule-set ctx 'expr (cadr obj) backend))

;;------------------ Statements -----------------
(defmethod eval-while (ctx obj (backend debug-backend))
  (format (my-stream backend) "while (")
  (apply-rule-set ctx 'expr (car obj) backend)
  (format (my-stream backend) ")")
  (apply-rule-set ctx 'stmt (cadr obj) backend))


(defmethod eval-return (ctx obj (backend debug-backend))
  (format (my-stream backend) "return ")
  (apply-rule-set ctx 'expr (car obj) backend)
  ;; (format (my-stream backend) ";~%")
  )

(defmethod eval-for (ctx obj (backend rewrite))
  "This method rewrites the for loop into a while loop"
  ;; First we make the initial variable
  (let* ((for-init (first (car obj)))
	 ;; Then the conditional
	 (for-cond (second (car obj)))
	 ;; Then the step
	 (for-step (third (car obj)))
	 ;; This is the body
	 (for-body `(block (seq ,(cadr obj) ,for-step)))
	 (my-while `(block (seq
			    ,for-init
			    (while ,for-cond ,for-body)))))
    (apply-rule-set ctx 'stmt my-while backend)))


(defmethod eval-nothing (ctx obj (backend debug-backend))
  ;; (format (my-stream backend) ";~%")
  )

(defmethod eval-assign (ctx obj (backend debug-backend))
  (apply-rule-set ctx 'expr (nth 0 obj) backend)
  (format (my-stream backend) " = ")
  (apply-rule-set ctx 'expr (nth 1 obj) backend)
  ;; (format (my-stream backend) ";~%")
  )

(defmethod eval-seq (ctx obj (backend debug-backend))
  (apply-rule-set ctx 'stmt (nth 0 obj) backend)
  (format (my-stream backend) ";~%")
  (apply-rule-set ctx 'stmt (nth 1 obj) backend)
  (format (my-stream backend) ";~%"))

(defmethod eval-defvar (ctx obj (backend debug-backend))
  ;; types is {type, ptrtype, structtype}
  (apply-rule-set ctx 'types (car obj) backend)
  (apply-rule-set ctx 'expr (cadr obj) backend))

(defmethod eval-defstruct (ctx obj (backend debug-backend))
  ;; This should be only struct-type
  (apply-rule-set ctx 'struct-type (car obj) backend)
  (apply-rule-set ctx 'stmt (cadr obj) backend)
  ;; (format (my-stream backend) ";~%")
  )

(defmethod eval-deftype (ctx obj (backend debug-backend))
  (apply-rule-set ctx 'struct-type (car obj) backend)
  (format (my-stream backend) " ~S " (cadr obj))
  ;; (format (my-stream backend) ";~%")
  )

(defmethod eval-block (ctx obj (backend debug-backend))
  (format (my-stream backend) "{~%")
  (apply-rule-set ctx 'stmt (car obj) backend)
  (format (my-stream backend) "}"))

(defmethod eval-defun (ctx obj (backend debug-backend))
  ;; Define the output type
  (apply-rule-set ctx 'types (third obj) backend)
  (format (my-stream backend) " ~S"(first obj))
  ;; Get all the parameters in a string
  (format (my-stream backend) "(")
  (loop for i in (second obj) do
    (progn
      (apply-rule-set ctx 'types (car i) backend)
      (apply-rule-set ctx 'expr (cadr i) backend)
      (when (not (eq i (car (last (second obj)))))
	(format (my-stream backend) ", "))))
  (format (my-stream backend) ")")
  ;; define the body of the function
  (apply-rule-set ctx 'stmt (car (last obj)) backend))

;; This should be an expression
(defmethod eval-funcall (ctx obj (backend debug-backend))
  (format (my-stream backend) "~S" (car obj))
  ;; loop through the function call arguments
  (format (my-stream backend) "(")
  (loop for i in (second obj)
	for count from 0
	if (< count (1- (length (second obj)))) do
	  (apply-rule-set ctx 'expr i backend)
	  (format (my-stream backend) ", ")
	else do
	  (apply-rule-set ctx 'expr i backend))
  (format (my-stream backend) ")")
  ;; (apply-rule-set ctx 'stmt (last obj) backend)
  )

(defmethod eval-if-else (ctx obj (backend debug-backend))
  (format (my-stream backend) "if (")
  (apply-rule-set ctx 'logical-expr (car obj) backend)
  (format (my-stream backend) ")")
  (apply-rule-set ctx 'stmt (second obj) backend)
  (format (my-stream backend) " else ")
  (apply-rule-set ctx 'stmt (third obj) backend))

;; We first instantite the context
(defvar ctx)
(setq ctx (rules::make-context :ruleset '() :union-kw 'union))

;; Now we register these rules with the context
(register-rule-set ctx `((math-expr . ((+ . eval-plus)
				       (- . eval-sub)
				       (* . eval-mult)
				       (/ . eval-div)
				       (% . eval-mod)))
			 (logical-expr . ((>= . eval-geq)
					  (> . eval-gt)
					  (<= . eval-leq)
					  (< . eval-lt)
					  (or . eval-or)
					  (and . eval-and)
					  (not . eval-not)))
			 (decl-expr . ((defvar . eval-defvar)))
			 ;; (func-expr . ((funcall . eval-funcall)))
			 ;; (if-else-expr . ((if-else . eval-if-else)))
			 (types . ((type . eval-type)
				   (ptrtype . eval-ptrtype)
				   (struct . eval-structype)))
			 (add-expr . ((address . eval-address)
				      (deref . eval-deref)))
			 (simple-expr . ((number . eval-val)
					 (symbol . eval-var)
					 (lvar . eval-lvar)
					 (brackets . eval-brackets)))
			 (stmt . ((= . eval-assign)
				  (seq . eval-seq)
				  (defstruct . eval-defstruct)
				  (deftype . eval-deftype)
				  (block . eval-block)
				  (if-else . eval-if-else)
				  (funcall . eval-funcall)
				  (defun . eval-defun)
				  (deftype . eval-deftype)
				  (nothing . eval-nothing)
				  (for . eval-for)
				  (while . eval-while)
				  (return . eval-return)))
			 ;; This is the sum (union) type of the two
			 ;; different types -- can have as many as you
			 ;; want summed together.
			 (expr . ((,(rules::context-union-kw ctx) .
				    (math-expr simple-expr add-expr
					       logical-expr decl-expr))))))

;; Example of the language
(defvar example1)
(setq example1 `(seq
		 (= h (% z 100))
		 (seq
		  (= z
		     (* (brackets (+ z x)) (/ y 100)))
		  (= z (+ x 10)))))


;; ;; Make the backend instance
(defvar my-debug)
(setq my-debug (make-instance 'debug-backend))

;; Now we apply and eval the example within the context
(apply-rule-set ctx 'stmt example1 my-debug)

;; Example-2 (a simpler example)
(defvar e2)
(setq e2 `(+ x 10))

(apply-rule-set ctx 'math-expr e2 my-debug)

;; Print the output from the debug
(format t (get-output-stream-string (my-stream my-debug)))

;; Example of a function definition
(setq e2 `(seq
	   (if-else (and h
			 (brackets (or (>= x 10) (< t 100))))
		    (block (=  x 100))
		    (block (nothing)))
	   (seq (defun F
		    (((type int) (symbol x))
		     ((ptrtype (type float)) y)) ;the input params
		  (type void)			;the output type
		  (block (seq (= x (address y))
			      (= (deref x) 10))))
		(funcall F
			 (10 (address h)))))) ;the body

(apply-rule-set ctx 'stmt e2 my-debug)
(format t "~%")
(format t (get-output-stream-string (my-stream my-debug)))

;TODO: Test how the for statement performs -- rewrite and produce c-code
(defvar e3)
(setq e3 `(for ((= (defvar (type int) i) 0)
		(< i 10)
		(= i (+ i 1)))
	       ;; The body
	       (= j (* j 100))))
(defparameter my-rewrite (make-instance 'rewrite))
(apply-rule-set ctx 'stmt e3 my-rewrite)
(format t "~%")
(format t (get-output-stream-string (my-stream my-rewrite)))
