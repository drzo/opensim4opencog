;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  DOTLISP / CLOJURE COMPATIBLITY    ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STR In DotLisp/Clojure is a write-to-string except for strings
;;; ( if is a system object it will call ToString() )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun str (form) 
  (if (stringp form) form 
   (if (java-object-p form) 
   (jcall "toString" form)(write-to-string form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ In DotLisp/Clojure is a Formatted Write to string
;;; .NET only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro @ (string &rest body) 
   (if body `(jstatic "Format" "cli.System.String" ,string ,@body) string))

;; (jstatic "Format" "cli.System.String" "abc {0}" 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; == In DotLisp/Clojure is #'EQUALP
;;; might need to call Object.Equals?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro == (left right) `(equalp ,left ,right))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EQL? In DotLisp/Clojure is #'EQL
;;; might need to call RT ==?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eql? (left right) `(eql ,left ,right))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Helper for DotLisp.Intern(sname,someObject)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun symbol-jobject (sname osymbol oclass &optional (depth 3))
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
	   ;; (print `(define-symbol-macro ,sym ,jff))
	   ;;(when (> depth 0) (symbol-jobject (symbol-name sym) sym  (jcall "getType" fs) (- depth 1)))
	   (eval `(define-symbol-macro ,sym ,jff))
	      
	   ))	
		
	(dolist (fs oms)
          (let* ((mn (jcall "getName" fs))	         
		 (isstatic (jstatic "isStatic" "java.lang.reflect.Modifier" (jcall "getModifiers" fs)))
		 (sym (intern  (string-upcase (concatenate 'string sname (if isstatic "/" ".") mn))))
		 jff)
    	   (setq jff 
	    (if isstatic 
             `(defun ,sym (&rest args) (eval `(jstatic ,,mn (jcall "getClass" ,,osymbol) ,@args )))
             `(defun ,sym (&rest args) (eval `(jcall ,,mn ,,osymbol ,@args )))
	     ))	     
          ;;(setq jff `(defun ,sym (&rest args) (eval `(jinvoke ,,mn (jcall "getClass" ,,osymbol) ,,osymbol ',@args ))))
	 ;;  (print jff)
	   (eval jff)
	 ))

	(if wasbound (setq osymbol lastbound) (makunbound osymbol))
     )))

(defun seq-to-list (seq)
  (let ((len (- (length seq) 1)) result)
     (do ((elnum len (1- elnum))) ((< elnum 0))
        (setq result (cons (aref seq elnum) result))) result)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETJ is a Alias to DotLisp SET and a CLs SETQ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro setj (osymbol value)
  `(progn (setq ,osymbol ,value)
    (symbol-jobject ,(symbol-name osymbol) ',osymbol 
       (jclass-immeadiate ,osymbol ))
      ,osymbol))

;; helper
(defun jclass-immeadiate (obj)
  (jcall "getClass" (make-immediate-object (make-immediate-object obj))))

;; helper - should probly be (java::%find-java-class (jclass-name (jclass-immeadiate obj)))
(defun jtype-of (obj) 
  (jstatic "findJavaClass" "org.armedbear.lisp.JavaClass" (jclass-immeadiate obj)))

;; dotLisp compat helper
(defvar *set-funvalue* (symbol-function 'set))
'(defmacro set (sym value)
  (if (symbolp sym) 
    `(setq ,sym ,value)
    `(setq ,(eval sym) ,value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  def-macro in DotLisp is like defmacro in Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl::defmacro def-macro (m-params-pattern &rest exprsTilde)
  (let ((exprs (untildy exprsTilde)))
   `(cl::defmacro ,(car m-params-pattern) ,(cdr m-params-pattern) ,@exprs)))

; helper
(defun untildy (form)
  (if (symbolp form)
     form ;; (write-to-string form)
     (if (consp form)
       (cons (untildy (car form))(untildy (cdr form)))
	form)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  def in DotLisp is like define in Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl::defmacro def (m-params-pattern &rest exprs)
   `(defun ,(car m-params-pattern) ,(cdr m-params-pattern) ,@exprs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  SETF expander for JFIELD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun (setf jfield)
  (newvalue class-ref-or-field field-or-instance &optional ( instance :noinst) (value :novalue))
  (if (eq instance :noinst)
    (jfield class-ref-or-field field-or-instance newvalue)
    (jfield class-ref-or-field field-or-instance instance newvalue)))

#|  

------------- So now things like this can work ---------------

 (define-symbol-macro %iscold (jfield "org.armedbear.lisp.Lisp" "cold"))
  %iscold ;; ==> NIL
  (setq %iscold T) ;; ==> T
  %iscold ;; ==> T  
  (setq %iscold NIL) ;; ==> NIL

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOJ is a DotLisp Compatibility for Common Lisp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cloj (call &rest args) `(print (list ',call ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCKJ is a Alias to DotLisp BLOCK and a CLs PROGN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro blockj (&rest bod) `(progn ,@bod))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK is a Alias to DotLisp BLOCK and a CLs PROGN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro block (first &rest bod)
  (if (symbolp first)
  `(progn first ,@bod) ;; use the CL Named Block? 
  `(progn first ,@bod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load the Cogbot Lisp file now 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "cogbot.lisp")

(defun prim-check (primUUID)
  (print (list primUUID (type-of primUUID))))

 ;--------------------------------------
 ; Here the bot is officially connected (I think), so you could 
 ; have it perform a initial inworld tasks like wearing some clothes
 ;-------------------------------------
 (defun on-network-connected (&rest optreasonmessage)
    (thisClient.msgClient (@"(on-network-connected )" ) )
    (thisClient.ExecuteCommand (@"say Hello World"))   
    (thisClient.ExecuteCommand (@"use HMG to wear"))
 )


;; (ON-CHAT (@ "My Bot") (@ "hi"))
;;
;;  (let* 
;;     ((val (eval value))
;;      (oclass (jcall "getClass" (MAKE-IMMEDIATE-OBJECT   (MAKE-IMMEDIATE-OBJECT  val))))
;;   (SYMBOL-JOBJECT (symbol-name osymbol) osymbol oclass)
;;  (symbol-jobject "test" 'test (make-immediate-object "test123"))
;; (dolist (var (seq-to-list (jcall "getMethods" (jclass "java.lang.String"))))   (print (jcall "getName" var)))

;  (on-avatar-pos agent vector) -> "(position (agent) vector)";
;;(defun on-avatar-pos (agent vector)
;;  (progn (thisClient.msgClient (@"(position ({0}) '{1}')" (str agent)(str vector)) )))

