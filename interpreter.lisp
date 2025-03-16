;; (load (merge-pathnames "rules.lisp"))
;; (load (merge-pathnames "clang.lisp"))

(in-package :clang)
;; (declaim (optimize (speed 3) (safety 0)))
;; Trying the type of class I want for the interpreter .GF is the hash
;; table of symbol name to function body. GV is is the hash map from
;; variable name (symbol) to (type . value)
(defclass interpret (rewrite)
  ((GF :initform (make-hash-table))
   (GV  :initform (make-hash-table))
   (Others :initform '() :type list)))

;; This is the structure that will be kept inside the current function
(defclass func-vars () ((funcs :initform (make-hash-table))
			(vars :initform (make-hash-table))
			(func-name)))

;; Now start writing the methods to write the interpreter

(defmethod eval-defun (ctx obj (backend interpret))
  (cond
    ((endp (slot-value backend 'others))
     ;; No shadowing allowed
     (assert (not (gethash (car obj) (slot-value backend 'gf))))
     (setf (gethash (car obj) (slot-value backend 'gf)) obj))
    (t (let ((current-func (car (slot-value backend 'others))))
	 ;; The assert guarantees no shadowing
	 (assert (not (gethash (car obj) (slot-value current-func 'funcs))))
	 (setf (gethash (car obj) (slot-value current-func 'funcs)) obj)))))

(defmethod eval-defvar (ctx obj (backend interpret))
  (cond
    ((endp (slot-value backend 'others))
     ;XXX: No shadowing from the global variables
     (let ((var-is (if (not (symbolp (cadr obj))) (cadr(cadr obj)) (cadr obj))))
       (assert (not (gethash var-is (slot-value backend 'gv))))
       (setf (gethash var-is (slot-value backend 'gv)) obj)
       var-is))
    (t (let ((current-func (car (slot-value backend 'others)))
	     (var-is (if (not (symbolp (cadr obj))) (cadr(cadr obj)) (cadr obj))))
	 ;; No shadowing in the local variables
	 (assert (not (gethash var-is (slot-value current-func 'vars))))
	 (setf (gethash var-is (slot-value current-func 'vars)) obj)
	 var-is))))


(defmethod eval-var (ctx obj (backend interpret))
  (let ((val (cond
	       ((endp (slot-value backend 'others))
		(cadr (gethash (car obj) (slot-value backend 'gv))))
	       (t (let ((current-func (car (slot-value backend 'others))))
		    (cadr (gethash (car obj) (slot-value current-func 'vars))))))))
    val))

(defmethod eval-if-else (ctx obj (backend interpret))
  (let ((bool-val (apply-rule-set ctx 'logical-expr (first obj) backend)))
    (if bool-val (apply-rule-set ctx 'stmt (second obj) backend)
	(apply-rule-set ctx 'stmt (third obj) backend))))

(defmethod eval-while (ctx obj (backend interpret))
  (loop while (apply-rule-set ctx 'expr (car obj) backend)
	do (apply-rule-set ctx 'stmt (cadr obj) backend)))

(defmethod eval-and (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (and left right)))

(defmethod eval-or (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (or left right)))

(defmethod eval-leq (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (<= left right)))

(defmethod eval-geq (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (>= left right)))

(defmethod eval-gt (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (> left right)))

(defmethod eval-lt (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (< left right)))

(defmethod eval-not (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend)))
    (not left)))

(defmethod eval-and (ctx obj backend)
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (and left right)))

(defmethod eval-or (ctx obj backend)
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (or left right)))

(defmethod eval-lvar (ctx obj (backend interpret))
  (let* ((val (eval-var ctx obj backend)))
    (if (not (eq val nil)) (car obj) nil)))

(defmethod eval-val (ctx obj (backend interpret))
  (car obj))

(defun assign-to-var (var val backend)
  (cond
    ((endp (slot-value backend 'others))
     (setf (cadr (gethash var (slot-value backend 'gv))) val))
    (t (let ((current-func (car (slot-value backend 'others))))
	 (setf (cadr (gethash var (slot-value current-func 'vars))) val)))))

(defmethod eval-plus (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in context of ~S" obj)
    (assert left (left) "Lvalue not found in context of ~S" obj)
    (+ left right)))

(defmethod eval-minus (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in context of ~S" obj)
    (assert left (left) "Lvalue not found in context of ~S" obj)
    (- left right)))

(defmethod eval-mult (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in context of ~S" obj)
    (assert left (left) "Lvalue not found in context of ~S" obj)
    (* left right)))

(defmethod eval-div (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in context of ~S" obj)
    (assert left (left) "Lvalue not found in context of ~S" obj)
    (/ left right)))

(defmethod eval-mod (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in context of ~S" obj)
    (assert left (left) "Lvalue not found in context of ~S" obj)
    (mod left right)))

(defmethod eval-assign (ctx obj (backend interpret))
  (let ((right (apply-rule-set ctx 'expr (cadr obj) backend))
	(left (apply-rule-set ctx 'expr (car obj) backend)))
    (assert right (right) "Rvalue not a number in context ~S" obj)
    (assert left (left) "Lvalue not found in context of ~S" obj)
    ;; assign to the varible the right value
    (assign-to-var left right backend)))

(defmethod eval-return (ctx obj (backend interpret))
  (let ((toret (apply-rule-set ctx 'expr (car obj) backend)))
    toret))

(defmethod eval-seq (ctx obj (backend debug-backend))
  (apply-rule-set ctx 'stmt (nth 0 obj) backend)
  (apply-rule-set ctx 'stmt (nth 1 obj) backend))

(defmethod eval-block (ctx obj (backend interpret))
  (apply-rule-set ctx 'stmt (car obj) backend))

(defmethod eval-funcall (ctx obj (backend interpret))
  (cond
    ((eq (car obj) 'print)
     (loop for i in (cdr obj)
	   do (format t "Var ~S value is: ~S" (cadar i)
		      (apply-rule-set ctx 'expr (car i) backend))))
    (t (let* ((func-struct (make-instance 'func-vars))
	      (body (if (not (endp (slot-value backend 'others)))
			(gethash (car obj)
				 (slot-value
				  (car (slot-value backend 'others))
				  'funcs))
			nil))
	      (body (when (not body)
		      (gethash (car obj) (slot-value backend 'gf))))
	      (params (second body))
	      (argvals (loop for arg in (second obj)
			     collect (apply-rule-set ctx 'expr arg backend)))
	      (dvars (loop for p in params
			   for arg in argvals
			   collect `(= (defvar ,(car p) ,(cadr p)) ,arg)))
	      (toret nil))
	 (push func-struct (slot-value backend 'others))
	 ;; Now enter the body and interpret
	 (assert body (body) "Body of function ~S not found" (car obj))
	 ;TODO: Map the arguments to the parameters of the function body
	 (assert (= (length params) (length (second obj))))
					;XXX: make the body with the new variables
	 (setq body (reduce (lambda (x y) `(seq ,x ,y)) dvars
			    :from-end t :initial-value (fourth body)))
	 ;; (format t "Body for the function ~S is: ~S~%" (car obj) body)
	 (setq toret (apply-rule-set ctx 'stmt body backend))
	 ;; (format t "returned from function: ~S~%" toret)
	 ;; Pop the func struct out, since the function is done
	 (pop (slot-value backend 'others))
	 toret))))

;; We want to add or change the type/object tree in the rules file

;; Push the if-else rule
(setf (rules::context-ruleset ctx)
      (append '((if-else-expr . ((if-else . eval-if-else))))
	      (rules::context-ruleset ctx)))
;; We want to push a new rule called the func-rule
(setf (rules::context-ruleset ctx)
      (append '((func-expr . ((funcall . eval-funcall))))
	      (rules::context-ruleset ctx)))

;; Add these rules as expressions too
(setf (cadr (assoc 'expr (rules::context-ruleset ctx)))
      (append
       (cadr (assoc 'expr (rules::context-ruleset ctx)))
       '(func-expr if-else-expr)))

;; Example of reading a file with the code
(defvar value)
(with-open-file (fd (merge-pathnames "test_while.mlisp") :direction :input)
  (setq value (loop for i = (read-preserving-whitespace fd nil nil)
		    while i collect i into value
		    finally (return value))))

;; Now start processing the read value
(defparameter inter (make-instance 'interpret))

;; Compute
(loop for i in value
      do (apply-rule-set ctx 'stmt i inter))

(format t "~%")
(format t (get-output-stream-string (my-stream inter)))
