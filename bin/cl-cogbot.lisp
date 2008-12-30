; Note: to find event generators in C# code look for
;   enqueueLispTask("(on

(defun SYMBOL-JOBJECT (sname osymbol oclass &optional (depth 3))
 (let* ((wasbound (boundp osymbol))(lastbound (if wasbound (symbol-value osymbol))))
    (setq osymbol osymbol)
    (let* ((ofs (seq-to-list (jcall "getDeclaredFields" oclass)))
	 (oms (seq-to-list (jcall "getDeclaredMethods" oclass))))
	(dolist (fs ofs)
	  (let* ((mn (jcall "getName" fs))
	         (sym (intern  (string-upcase (concatenate 'string sname "." mn))))
		 (isstatic (jstatic "isStatic" "java.lang.reflect.Modifier" (jcall "getModifiers" fs)))
		 jff)
           (jcall "setAccessible" fs T)
    	   (setq jff 
	    (if isstatic 
             `(jfield (jcall "getClass" ,osymbol) ,mn)
	     `(jfield (jcall "getClass" ,osymbol) ,mn ,osymbol)))
	   (print `(define-symbol-macro ,sym ,jff))
	   ;;(when (> depth 0) (symbol-jobject (symbol-name sym) sym  (jcall "getType" fs) (- depth 1)))
	   (eval `(define-symbol-macro ,sym ,jff))
	      
	   ))
	
	(dolist (fs oms)
          (let* ((mn (jcall "getName" fs))
	         (sym (intern  (string-upcase (concatenate 'string sname "." mn))))
		 (isstatic (jstatic "isStatic" "java.lang.reflect.Modifier" (jcall "getModifiers" fs)))
		 jff)
           (jcall "setAccessible" fs T)
    	   (setq jff 
	    (if isstatic 
             `(defun ,sym (&rest args) (eval `(jstatic ,,mn (jcall "getClass" ,,osymbol) ',@args )))
             `(defun ,sym (&rest args) (eval `(jcall ,,mn ,,osymbol ',@args )))
	     ))
	   (print jff)
	   (eval jff)
	   ))
    (if wasbound (setq osymbol lastbound) (makunbound osymbol))
     )))


(defun seq-to-list (seq)
  (let ((len (- (length seq) 1)) result)
     (do ((elnum len (1- elnum))) ((< elnum 0))
        (setq result (cons (aref seq elnum) result))) result)) 

;;(symbol-jobject "test" 'test (make-immediate-object "test123"))

(defun str (form) (if (stringp form) form (if (java-object-p form) (jcall "toString" form)(write-to-string form))))

(defmacro block (&rest bod) `(progn ,@bod))

(dolist (var (seq-to-list (jcall "getMethods" (jclass "java.lang.String"))))
   (jcall "getName" var))


(defun (setf jfield)
  (newvalue class-ref-or-field field-or-instance &optional ( instance :noinst) (value :novalue))
  (if (eq instance :noinst)
    (jfield class-ref-or-field field-or-instance newvalue)
    (jfield class-ref-or-field field-or-instance instance newvalue)))

(define-symbol-macro %iscold (jfield "org.armedbear.lisp.Lisp" "cold"))

(defmacro @ (string &rest body)
  (if body
  `(jstatic "Format" "cli.System.String" ,string ,@body)
    string))

(cl::defmacro def-macro (m-params-pattern &rest exprs)
   `(cl::defmacro ,(car m-params-pattern) ,(cdr m-params-pattern) ,@exprs))

(cl::defmacro def (m-params-pattern &rest exprs)
   `(defun ,(car m-params-pattern) ,(cdr m-params-pattern) ,@exprs))

(dolist (var (seq-to-list (jcall "getMethods" (jclass "java.lang.String"))))
   (print (jcall "getName" var)))


(defmacro cloj (call &rest args)
  `(print (list ',call ,@args)))

(load "cogbot.lisp")

