;; First we define a structure that will be the context for all the rules

(defstruct context (ruleset '() :type list)
	         (union-kw 'sum :type symbol :read-only t))

;; We should be able to register rules into the ruleset
(defun register-rule-set (ctx rule-assoc-list)
  "ctx: The context structure
   rule-symbol: The rule-set to register the rules
   rules-assoc-list: The association list of rules
     `((rule-set-name . ((symbol .func) (symbol2 . func2)...(symboln . funcn))))"
  (setf (context-ruleset ctx) rule-assoc-list))

;; ;; Apply to funclist
;; (defun func-apply (fn ctx obj &rest args)
;;   ;; apply takes a list of arguments, which is what args is!
;;   (cond
;;     ((= (length args) 0) (funcall fn ctx obj))
;;     ((funcall fn ctx obj args))))

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
		                 ;; (setq func (func-search ctx item obj searched))
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

(defun apply-rule-set (ctx rule-set-name obj args)
  (let ((func (car (progn
		     (func-search ctx rule-set-name obj '())))))
    ;; Expected func signature is: (<nam> ctx obj &rest args)
    (assert func (rule-set-name obj)
	          "Cannot find function to apply to ~S~% with rule: ~S"
	          obj rule-set-name)
    (funcall func ctx (cdr obj) args)))

;; --------------- Generic functions for eval------------
(defgeneric eval-var (ctx obj backend))
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
(defgeneric eval-ptr (ctx obj backend))
(defgeneric eval-address (ctx obj backend))
(defgeneric eval-defvar (ctx obj backend))
(defgeneric eval-defstruct (ctx obj backend))
(defgeneric eval-deftype (ctx obj backend))
(defgeneric eval-block (ctx obj backend))

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

;; -------------------------- Types --------------------
;; Normal types in the language -- specialised on the 3rd arg
(defmethod eval-type (ctx obj (backend debug-backend))
  (declare (ignorable ctx))
  (format t "~S " (car obj)))

;; Pointer types in the language
(defmethod eval-ptrtype (ctx obj (backend debug-backend))
  (format t "~S *" (car obj)))

(defmethod eval-structype (ctx obj (backend debug-backend))
  (format t "struct ~S" (car obj)))

;; ------------------------ Expressions --------------
;; Now write the rules for processing the expression above
(defmethod eval-plus (ctx obj (backend debug-backend))
  "Rule to print the + function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format t "~S" '+)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

;; Now write the rules for processing the expression above
(defmethod eval-sub (ctx obj (backend debug-backend))
  "Rule to print the - function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format t "~S" '-)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

;; Now write the rules for processing the expression above
(defmethod eval-mult (ctx obj (backend debug-backend))
  "Rule to print the * function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format t "~S" '*)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

;; Now write the rules for processing the expression above
(defmethod eval-div (ctx obj (backend debug-backend))
  "Rule to print / function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format t "~S" '/)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

;; Now write the rules for processing the expression above
(defmethod eval-mod (ctx obj (backend debug-backend))
  "Rule to print the % function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj) backend)
    (format t "~S" '%)
    (apply-rule-set ctx 'expr (nth 1 obj) backend)))

(defmethod eval-val (ctx obj (backend debug-backend))
  "Print the value"
  (format t "~S" (car obj)))

(defmethod eval-var (ctx obj (backend debug-backend))
  "Print the variable name"
  (format t "~S" (car obj)))

(defmethod eval-brackets (ctx obj (backend debug-backend))
  "Print the brackets"
  (format t "(")
  (apply-rule-set ctx 'expr (car obj) backend)
  (format t ")"))

(defmethod eval-ptr (ctx obj (backend debug-backend))
  (format t "* ")
  (apply-rule-set ctx 'simple-expr (car obj) backend))

(defmethod eval-address (ctx obj (backend debug-backend))
  (format t "& ")
  (apply-rule-set ctx 'simple-expr (car obj) backend))

;;------------------ Statements -----------------
(defmethod eval-assign (ctx obj (backend debug-backend))
  (apply-rule-set ctx 'expr (nth 0 obj) backend)
  (format t " = ")
  (apply-rule-set ctx 'expr (nth 1 obj) backend))

(defmethod eval-seq (ctx obj (backend debug-backend))
  (apply-rule-set ctx 'stmt (nth 0 obj) backend)
  (format t ";~%")
  (apply-rule-set ctx 'stmt (nth 1 obj) backend))

(defmethod eval-defvar (ctx obj (backend debug-backend))
  ;; types is {type, ptrtype, structtype}
  (apply-rule-set ctx 'types (car obj) backend)
  (apply-rule-set ctx 'expr (cadr obj) backend))

(defmethod eval-defstruct (ctx obj (backend debug-backend))
  ;; This should be only struct-type
  (apply-rule-set ctx 'struct-type (car obj) backend)
  (apply-rule-set ctx 'stmt (cadr obj) backend))

(defmethod eval-deftype (ctx obj (backend debug-backend))
  (apply-rule-set ctx 'struct-type (car obj) backend)
  (format t " ~S " (cadr obj)))

(defmethod eval-block (ctx obj (backend debug-backend))
  (format t "{~%")
  (apply-rule-set ctx 'stmt (car obj) backend)
  (format t "}~%"))

;; We first instantite the context
(defvar ctx)
(setq ctx (make-context :ruleset '() :union-kw 'union))

;; Now we register these rules with the context
(register-rule-set ctx `((math-expr . ((+ . eval-plus)
				       (- . eval-sub)
				       (* . eval-mult)
				       (/ . eval-div)
				       (% . eval-mod)))
			 (simple-expr . ((val . eval-val)
					 (var . eval-var)
					 (brackets . eval-brackets)))
			 (stmt . ((= . eval-assign)
				  (seq . eval-seq)))
			 ;; This is the sum (union) type of the two
			 ;; different types -- can have as many as you
			 ;; want summed together.
			 (expr . ((,(context-union-kw ctx) .
				  (math-expr simple-expr ))))))
;; Check if the rules have been registered
;; (print (context-ruleset ctx))
;; (print (context-union-kw ctx))




;; Example of the language
(defvar example1)
(setq example1 `(seq
		 (= (var h) (% (var z) (val 100)))
		 (seq
		  (= (var z)
		     (* (brackets (+ (var z) (var x))) (/ (var y) (val 100))))
		  (= (var z) (+ (var x) (val 10))))))

;; ;; Make the backend instance
(defvar my-debug)
(setq my-debug (make-instance 'debug-backend))

;; Now we apply and eval the example within the context
(apply-rule-set ctx 'stmt example1 my-debug)

;; Example-2 (a simpler example)
(defvar e2)
(setq e2 `(+ (var x) (val 10)))

(apply-rule-set ctx 'math-expr e2 my-debug)
