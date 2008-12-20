; Note: to find event generators in C# code look for
;   enqueueLispTask("(on

(defun symbol-object (osymbol javaObject) 
  (setq osymbol javaObject)
  (let* ((oclass (jclass-of javaObject))
        (sname (symbol-name osymbol))
	(ofs (jcall "getFields" oclass))
	(oms (jcall "getMethods" oclass)))
     ))


(defun str (form) (if (stringp form) form (if (java-object-p form) (jcall "toString" form)(write-to-string form))))

(defmacro block (&rest bod) `(progn ,@bod))

(defun seq-to-list (seq)
  (let ((len (- (length seq) 1)) result)
     (do ((elnum len (1- elnum))) ((< elnum 0))
        (setq result (cons (aref seq elnum) result))) result)) 

(dolist (var (seq-to-list (jcall "getMethods" (jclass "java.lang.String"))))
   (jcall "getName" var))

#|
(defun jo-value (object) (jcall "get" object))(defun (setf jo-value) (value object) (jcall "set" object value))
(define-symbol-macro %myfieldval (jo-value *myJO*))

(defmacro define-symbol-jfield (sym class field)
 `(progn
    (defun ,sym (&optional (value :missing)) 
        (if (eq value :missing) 
	    (jfield ,class ,field)
	    (jfield ,class ,field value)))
     (defun (setf ,sym ',sym) (object value) (jfield ,class ,field value))
     (define-symbol-macro ,sym (,sym ',sym))))


(defmacro define-symbol-jfield (sym class field)
 `(progn
    (defun ,sym (&optional (value :missing)) 
        (if (eq value :missing) 
	    (jfield ,class ,field)
	    (jfield ,class ,field value)))
     (defun (setf ,sym ',sym) (object value) (jfield ,class ,field value))
     (define-symbol-macro ,sym (,sym ',sym))))
|#
(define-symbol-macro %iscold (jfield "org.armedbear.lisp.Lisp" "cold"))




;;(ARGLIST (SYMBOL-FUNCTION 'JFIELD)) => (EXTENSIONS::CLASS-REF-OR-FIELD EXTENSIONS::FIELD-OR-INSTANCE &OPTIONAL EXTENSIONS::INSTANCE EXTENSIONS::VALUE)

;; or even 
#|
CYC(1):
(EXTENSIONS::CLASS-REF-OR-FIELD EXTENSIONS::FIELD-OR-INSTANCE) ;; &OPTIONAL ( EXTENSIONS::INSTANCE :NOINST) (EXTENSIONS::VALUE :NOVALUE)
  (NEWVALUE) ;;VALUE SUPPLIED
    `(JFIELD ,EXTENSIONS::CLASS-REF-OR-FIELD ,EXTENSIONS::FIELD-OR-INSTANCE ,NEWVALUE))

JFIELD
CYC(2): (get-setf-expansion '(jfield "org.armedbear.lisp.Lisp" "cold"))
COMMON-LISP:NIL
COMMON-LISP:NIL
(#:G3964)
(JFIELD "org.armedbear.lisp.Lisp" "cold" #:G3964)
(JFIELD "org.armedbear.lisp.Lisp" "cold")


CYC(4):|#
 (DEFSETF JFIELD 
  (EXTENSIONS::CLASS-REF-OR-FIELD EXTENSIONS::FIELD-OR-INSTANCE &OPTIONAL ( EXTENSIONS::INSTANCE :NOINST) (EXTENSIONS::VALUE :NOVALUE))
    (NEWVALUE) ;;VALUE SUPPLIED
  (IF (EQ EXTENSIONS::INSTANCE :NOINST)
    `(JFIELD ,EXTENSIONS::CLASS-REF-OR-FIELD ,EXTENSIONS::FIELD-OR-INSTANCE ,NEWVALUE)
    `(JFIELD ,EXTENSIONS::CLASS-REF-OR-FIELD ,EXTENSIONS::FIELD-OR-INSTANCE  ,EXTENSIONS::INSTANCE ,NEWVALUE)))
#|
JFIELD
CYC(5): (get-setf-expansion '(jfield "org.armedbear.lisp.Lisp" "cold"))
;; makeInstance public static final com.cyc.tool.subl.jrtl.nativeCode.type.core.SubLObject com.cyc.tool.subl.jrtl.nativeCode.subLisp.Equality.eq(com.cyc.tool.subl.jrtl.nativeCode.type.core.SubLObject,com.cyc.tool.subl.jrtl.nativeCode.type.core.SubLObject)
COMMON-LISP:NIL
COMMON-LISP:NIL
(#:G5809)
(JFIELD "org.armedbear.lisp.Lisp" "cold" #:G5809)
(JFIELD "org.armedbear.lisp.Lisp" "cold")
CYC(6):


(define-symbol-jfield %iscold "org.armedbear.lisp.Lisp" "cold")

%iscold   =>   #<JAVA-OBJECT Boolean true>
(setq %iscold (jfield "java.lang.Boolean" "TRUE"))
%iscold   =>   #<JAVA-OBJECT Boolean false>

(jfield "org.armedbear.lisp.Lisp" "cold" (jfield "java.lang.Boolean" "TRUE"))
(jfield "org.armedbear.lisp.Lisp" "cold" (jfield "java.lang.Boolean" "FALSE"))
|#
(defmacro @ (string &rest body) `(jstatic "Format" "cli.System.String" ,string ,@body))

(cl::defmacro def-macro (m-params-pattern &rest exprs)
   `(cl::defmacro ,(car m-params-pattern) ,(cdr m-params-pattern) ,@exprs))

(cl::defmacro def (m-params-pattern &rest exprs)
   `(defun ,(car m-params-pattern) ,(cdr m-params-pattern) ,@exprs))

(dolist (var (seq-to-list (jcall "getMethods" (jclass "java.lang.String"))))
   (print (jcall "getName" var)))

(defmacro dotlisp (call &rest args)
   `(dotj ',call ,args))

(defun dotj (call args)
  (print (list call args)))


;----------------------------------
; Login and Network events
;----------------------------------


#|

 (def (on-login-fail  login description)
  (block
    (dotlisp thisClient.msgClient (@"(on-login-fail ({0}) ({1}))" (str login)(str description)) )
    )
 )

(def (on-login-success  login description)
  (block
    (thisClient.msgClient (@"(on-login-success ({0}) ({1}))" (str login)(str description)) )
    )
 )

 (def (on-network-disconnected reason message)
  (block
    (dotlisp thisClient.msgClient (@"(on-network-disconnected ({0}) ({1}))" (str reason)(str message)) )
    )
 )
 
 ;--------------------------------------
 ; Here the bot is officially connected (I think), so you could 
 ; have it perform a initial inworld tasks like wearing some clothes
 ;-------------------------------------
 (def (on-network-connected reason message)
  (block
    (thisClient.msgClient (@"(on-network-connected )" ) )
    (thisClient.ExecuteCommand “say Hello World”)   
    (thisClient.ExecuteCommand “use HMG to wear”)
    )
 )

(def (on-simulator-connected simulator)
  (block
    (thisClient.msgClient (@"(on-simulator-connected ({0}) )" (str simulator)) )
    )
 )
 
;----------------------------------
; Avatars and objects
;----------------------------------
(def (on-new-avatar  avatar-name avatar-uuid)
  (block
    (thisClient.msgClient (@"(on-new-avatar ({0}) ({1}))" (str avatar-name)(str avatar-uuid)) )
    )
 )

(def (on-new-prim  prim-name prim-uuid prim-description)
  (block
    (thisClient.msgClient (@"(on-new-prim ({0}) ({1}) ({2}))" (str prim-name)(str prim-uuid)(str prim-description)) )
    )
 )
(def (on-new-foliage  foliage-name foliage-uuid foliage-description)
  (block
    (thisClient.msgClient (@"(on-new-prim ({0}) ({1}) ({2}))" (str foliage-name)(str foliage-uuid)(str foliage-description)) )
    )
 )


;-----------------------------
; In World Events
;-----------------------------
;  (on-chat agent message) -> "(heard (agent) message)";
(def (on-chat agent message)
  (block
    (thisClient.msgClient (@"(heard ({0}) '{1}')" (str agent)(str message)) )
    ;(thisClient.ExecuteCommand (@"say I heard ({0}) '{1}')" (str agent)(str message)) )
    ;(thisClient.ExecuteCommand (@"say I heard ({0}) '{1}')" (str agent)(str message)) )
    ; (thisClient.ExecuteCommand (@"{0}" (str message)) )
    )
     (set mymsg (str message))
    ; (set mymsgList (into nil (mymsg.Split(" ") )))
     
    ; (when (member mymsgList wamo )
    ;  (thisClient.ExecuteCommand "say I just saw wamo")
    ;  )
      
     (when (>= (mymsg.IndexOf "hello" ) 0) 
        (thisClient.ExecuteCommand (@"say Hello there {0} !!!" (str agent) ) )
     )
      (when (>= (mymsg.IndexOf "use" ) 0) 
        (thisClient.ExecuteCommand (@"{0}" (str message)) )
     )
     (when (>= (mymsg.IndexOf "follow" ) 0) 
        (thisClient.ExecuteCommand (@"{0}" (str message)) )
     )
     (when (>= (mymsg.IndexOf "cogbot" ) 0)
       (block 
        (set mycommand (str message))
        (set mycommand1 (mycommand.Replace "cogbot " ""))
        (thisClient.ExecuteCommand  mycommand1 )
        )
     )
    
 )
 
 ;  (on-instantmessage agent message) -> "(heard (agent) message)";
(def (on-instantmessage agent message)
  (block
    (thisClient.msgClient (@"(heard-in-im ({0}) '{1}')" (str agent)(str message)) )
    )
 )

 
;  (on-meanCollision perp victim) -> "(collision (perp) (victim) )";
(def (on-meanCollision perp victim)
  (block
    (thisClient.msgClient (@"(collision ({0}) ({1}))" (str perp)(str victim)) )
    )
 )
 
 ;----------------------------------
; Looking and Pointing events
; on-self-look-target occurs when someone looks or mouses at the Cogbot avatar
;----------------------------------
 (def (on-self-look-target  source description)
  (block
    (thisClient.msgClient (@"(on-self-look-target ({0}) ({1}))" (str source)(str description)) )
    )
 )
 
 (def (on-self-point-target  source description)
  (block
    (thisClient.msgClient (@"(on-self-point-target ({0}) ({1}))" (str source)(str description)) )
    )
 )

 (def (on-avatar-point  source  dest description)
  (block
    (thisClient.msgClient (@"(on-avatar-point ({0}) ({1}))" (str source)(str description)) )
    )
 )

 (def (on-avatar-look  source  dest description)
  (block
    (thisClient.msgClient (@"(on-avatar-look ({0}) ({1}))" (str source)(str description)) )
    )
 )
 
;---------------------------------
; avatar descriptions
;---------------------------------
;  (on-avatar-dist agent dist) -> "(distance (agent) distance)";
(def (on-avatar-dist agent dist)
  (block
    (thisClient.msgClient (@"(distance-from ({0}) {1})" (str agent)(str dist)) )
    )
 )

;  (on-avatar-pos agent vector) -> "(position (agent) vector)";
(def (on-avatar-pos agent vector)
  (block
    (thisClient.msgClient (@"(position ({0}) '{1}')" (str agent)(str vector)) )
    )
 )

;  (on-avatar-posture agent sitstand) -> "(posture (agent) sitstand)";
(def (on-avatar-posture agent sitstand)
  (block
    (thisClient.msgClient (@"(posture ({0}) '{1}')" (str agent)(str sitstand)) )
    )
 )

;---------------------------------
; prim descriptions
;---------------------------------
;  (on-prim-description obj primID description) -> "(prim-description (obj) 'description' )";
(def (on-prim-description  obj primID description)
  (block
    (thisClient.msgClient (@"(prim-description ({0}) ({1}) ({2}))" (str obj)(str primID)(str description)) )
    )
 )

;  (on-prim-dist prim-name primID dist) -> "(distance-from-prim (prim-name) (primID) distance)";
(def (on-prim-dist prim-name primID dist)
  (block
    (thisClient.msgClient (@"(distance-from-prim ({0})({1}) {2})" (str prim-name)(str primID)(str dist)) )
    )
 )

;  (on-prim-pos prim-name primID vector) -> "(prim-position (prim-name) (primID) vector)";
(def (on-prim-pos prim-name primID vector)
  (block
    (thisClient.msgClient (@"(prim-position ({0})({1}) '{2}')" (str prim-name)(str primID)(str vector)) )
    )
 )
|#
