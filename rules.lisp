(defpackage :rules (:export #:register-rule-set #:apply-rule-set)
	    (:use :cl))

;; First we define a structure that will be the context for all the rules
(in-package :rules)

;; (declaim (optimize (speed 3) (safety 0)))
(defstruct context (ruleset '() :type list)
	         (union-kw 'sum :type symbol :read-only t))

;; We should be able to register rules into the ruleset
(defun register-rule-set (ctx rule-assoc-list)
  "ctx: The context structure
   rule-symbol: The rule-set to register the rules
   rules-assoc-list: The association list of rules
     `((rule-set-name . ((symbol .func) (symbol2 . func2)...(symboln . funcn))))"
  (setf (context-ruleset ctx) rule-assoc-list))

;; We should be able to apply a function to the given list using the
;; registered ruleset
(defun func-search (ctx rule-set-name obj searched)
  "This function searches for the function to be applied in the
     type-hierarchy"
  (let* ((rule-set (context-ruleset ctx))
	 (union-kw (context-union-kw ctx))
	 (func
	   (cdr (assoc (car obj)
		       (cdr (assoc rule-set-name rule-set))))))
    (setq searched
	  (cons (car (assoc rule-set-name (context-ruleset ctx))) searched))
    (cond
      ((eq func nil) ;condition
       (let ((union-val (assoc rule-set-name rule-set))
	     (temp nil))
	 ;; Check if this is a union of types
	 (if (eq (caadr union-val) union-kw)
	     ;; Walk through all the items in the type set
	     (progn
	       (block RET
		 (loop for item in (cdadr union-val) do
		   (when (not (find item searched))
		     (setq temp (func-search ctx item obj searched))
		     (setq func (car temp))
		     (setq searched (cdr temp))
		     (when func (return-from RET)))))
	       (cons func searched))
	     ;; Return the func in else
	     (cons func searched))
	 ;; Return whatever is returned from the 
	 (cons func searched)))
      ((cons func searched)))))

(defun apply-rule-set (ctx rule-set-name obj backend)
  (let* ((obj (if (listp obj) obj
		  (cond
		    ;; Fix this to be something that comes from clang
		    ((symbolp obj) `(symbol ,obj))
		    ((numberp obj) `(number ,obj)))))
	 (func (car (progn
		      (func-search ctx rule-set-name obj '())))))
    ;; Expected func signature is: (<nam> ctx obj &rest args)
    (assert func (rule-set-name obj)
	          "Cannot find function to apply to ~S~% with rule: ~S"
	          obj rule-set-name)
    (funcall func ctx (cdr obj) backend)))
