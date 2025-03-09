;; (load (merge-pathnames "rules.lisp"))
;; (load (merge-pathnames "clang.lisp"))

(in-package :clang)

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
					;XXX: No shadowing
     (assert (not (gethash (cadr (cadr obj)) (slot-value backend 'gv))))
     (setf (gethash (cadr (cadr obj)) (slot-value backend 'gv)) obj))
    (t (let ((current-func (car (slot-value backend 'others))))
	 (assert (not (gethash (cadr (cadr obj)) (slot-value current-func 'vars))))
	 (setf (gethash (cadr (cadr obj)) (slot-value current-func 'vars)) obj))))
  ;; always send back the variable name
  (cadr (cadr obj)))


(defmethod eval-var (ctx obj (backend interpret))
  (let ((val (cond
	       ((endp (slot-value backend 'others))
		(cadr (gethash (car obj) (slot-value backend 'gv))))
	       (t (let ((current-func (car (slot-value backend 'others))))
		    (cadr (gethash (car obj) (slot-value current-func 'vars))))))))
    val))


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
    (assert right (right) "Rvalue not a number in assign ~S" obj)
    (assert left (left) "Lvalue not found in assign ~S" obj)
    (+ left right)))

(defmethod eval-minus (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in assign ~S" obj)
    (assert left (left) "Lvalue not found in assign ~S" obj)
    (- left right)))

(defmethod eval-mult (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in assign ~S" obj)
    (assert left (left) "Lvalue not found in assign ~S" obj)
    (* left right)))

(defmethod eval-div (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in assign ~S" obj)
    (assert left (left) "Lvalue not found in assign ~S" obj)
    (/ left right)))

(defmethod eval-mod (ctx obj (backend interpret))
  (let ((left (apply-rule-set ctx 'expr (car obj) backend))
	(right (apply-rule-set ctx 'expr (cadr obj) backend)))
    (assert right (right) "Rvalue not a number in assign ~S" obj)
    (assert left (left) "Lvalue not found in assign ~S" obj)
    (mod left right)))

(defmethod eval-assign (ctx obj (backend interpret))
  (let ((right (apply-rule-set ctx 'expr (cadr obj) backend))
	(left (apply-rule-set ctx 'expr (car obj) backend)))
    (assert right (right) "Rvalue not a number in assign ~S" obj)
    (assert left (left) "Lvalue not found in assign ~S" obj)
    ;; assign to the varible the right value
    (assign-to-var left right backend)))


(defmethod eval-funcall (ctx obj (backend interpret))
  (let* ((func-struct (make-instance 'func-vars))
	 (body (if (not (endp (slot-value backend 'others)))
		   (gethash (car obj)
			    (slot-value
			     (car (slot-value backend 'others))
			     'funcs))
		   nil))
	 (body (when (not body)
		 (gethash (car obj) (slot-value backend 'gf)))))
    (push func-struct (slot-value backend 'others))
    ;; Now enter the body and interpret
    (assert body (body) "Body of function ~S not found" (car obj))
    ;TODO: Map the arguments to the parameters of the function body
    (apply-rule-set ctx 'stmt (fourth body) backend)
    ;; Pop the func struct out, since the function is done
    (pop (slot-value backend 'others))))

;; Example of reading a file with the code
(defvar value)
(with-open-file (fd (merge-pathnames "test.mlisp") :direction :input)
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


