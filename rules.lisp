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

(defun apply-rule-set (ctx rule-set-name obj &rest args)
  (let ((func (car (progn
		                 (func-search ctx rule-set-name obj '())))))
    ;; Expected func signature is: (<nam> ctx obj &rest args)
    (assert func (rule-set-name obj)
	          "Cannot find function to apply to ~S~% with rule: ~S"
	          obj rule-set-name)
    (cond
      ((= (length args) 0)
       (funcall func ctx (cdr obj)))
      ((funcall func ctx (cdr obj) args)))))


;; -------------------------- Types --------------------
;; Normal types in the language
(defun print-type (ctx obj)
  (format t "~S " (car obj)))

;; Pointer types in the language
(defun print-ptrtype (ctx obj)
  (format t "~S *" (car obj)))

(defun print-structype (ctx obj)
  (format t "struct ~S" (car obj)))

;; ------------------------ Expressions --------------
;; Now write the rules for processing the expression above
(defun print-plus (ctx obj)
  "Rule to print the + function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj))
    (format t "~S" '+)
    (apply-rule-set ctx 'expr (nth 1 obj))))

;; Now write the rules for processing the expression above
(defun print-sub (ctx obj)
  "Rule to print the - function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj))
    (format t "~S" '-)
    (apply-rule-set ctx 'expr (nth 1 obj))))

;; Now write the rules for processing the expression above
(defun print-mult (ctx obj)
  "Rule to print the * function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj))
    (format t "~S" '*)
    (apply-rule-set ctx 'expr (nth 1 obj))))

;; Now write the rules for processing the expression above
(defun print-div (ctx obj)
  "Rule to print / function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj))
    (format t "~S" '/)
    (apply-rule-set ctx 'expr (nth 1 obj))))

;; Now write the rules for processing the expression above
(defun print-mod (ctx obj)
  "Rule to print the % function in the language"
  (progn
    (apply-rule-set ctx 'expr (nth 0 obj))
    (format t "~S" '%)
    (apply-rule-set ctx 'expr (nth 1 obj))))


(defun print-var (ctx obj)
  "Print the variable name"
  (format t "~S" (car obj)))

(defun print-val (ctx obj)
  "Print the value"
  (format t "~S" (car obj)))

(defun print-brackets (ctx obj)
  "Print the brackets"
  (format t "(")
  (apply-rule-set ctx 'expr (car obj))
  (format t ")"))

(defun print-ptr (ctx obj)
  (format t "* ")
  (apply-rule-set ctx 'simple-expr (car obj)))

(defun print-address (ctx obj)
  (format t "& ")
  (apply-rule-set ctx 'simple-expr (car obj)))

;;------------------ Statements -----------------
(defun print-assign (ctx obj)
  (apply-rule-set ctx 'expr (nth 0 obj))
  (format t " = ")
  (apply-rule-set ctx 'expr (nth 1 obj)))

(defun print-seq (ctx obj)
  (apply-rule-set ctx 'stmt (nth 0 obj))
  (format t ";~%")
  (apply-rule-set ctx 'stmt (nth 1 obj)))

(defun print-defvar (ctx obj)
  ;; types is {type, ptrtype, structtype}
  (apply-rule-set ctx 'types (car obj))
  (apply-rule-set ctx 'expr (cadr obj)))

(defun print-defstruct (ctx obj)
  ;; This should be only struct-type
  (apply-rule-set ctx 'struct-type (car obj))
  (apply-rule-set ctx 'stmt (cadr obj)))

(defun print-deftype (ctx obj)
  (apply-rule-set ctx 'struct-type (car obj))
  (format t " ~S " (cadr obj)))

(defun print-block (ctx obj)
  (format t "{~%")
  (apply-rule-set ctx 'stmt (car obj))
  (format t "}~%"))

;; We first instantite the context
(defvar ctx)
(setq ctx (make-context :ruleset '() :union-kw 'union))

;; Now we register these rules with the context
(register-rule-set ctx `((math-expr . ((+ . print-plus)
				                               (- . print-sub)
				                               (* . print-mult)
				                               (/ . print-div)
				                               (% . print-mod)))
			                   (simple-expr . ((val . print-val)
					                               (var . print-var)
					                               (brackets . print-brackets)))
			                   (stmt . ((= . print-assign)
				                          (seq . print-seq)))
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
;; Now we apply and eval the example within the context
(apply-rule-set ctx 'stmt example1)
