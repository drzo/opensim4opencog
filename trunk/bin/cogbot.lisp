; Note: to find event generators in C# code look for
;   enqueueLispTask("(on

(def False false)
(def True true)

(def (create-botclient &opt (first "") (last "") (pass "") (simurl "") (start ""))
    (clientManager.CreateBotClient first last pass simurl start))

(def (create-httpserver port &opt (botname ""))
    (clientManager.CreateHttpServer port botname))

(def-macro (enqueue lispTask)
    `(thisClient.enqueueLispTask ~lispTask))
        
;----------------------------------
; Login and Network events
;----------------------------------
 (def (on-login-fail  login description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-login-fail {0} {1})" (str login)(str description)) )
    )
 )

(def (on-login-success  login description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-login-success {0} {1})" (str login)(str description)) )
    )
 )

 (def (on-network-disconnected reason message)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-network-disconnected {0} {1})" (str reason)(str message)) )
    )
 )
 ;;Nephrael Rae: [on-object-animation '(avatar "Candie Brooks") "TALK"][on-object-animation '(avatar "Candie Brooks") "STAND_1"][on-object-animation '(avatar "Candie Brooks") "e45fbdc9-af8f-9408-f742-fcb8c341d2c8"]
 ;--------------------------------------
 ; Here the bot is officially connected (I think), so you could 
 ; have it perform a initial inworld tasks like wearing some clothes
 ;-------------------------------------
 (def (on-network-connected &opt reason message)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-network-connected )" ) )
    ;; annoys people sometimes in SL  (thisClient.ExecuteCommand (@"say Hello World"))   
    ;;(thisClient.ExecuteCommand (@"teleport Nakama/128.08/111.95/22.06"))
    ;; this works but the next is faster (thisClient.ExecuteCommand (@"use HMG to wear"))
    (thisClient.ExecuteCommand (@"wear Clothing/HMG"))
    ;; interesting places
    ;; (thisClient.ExecuteCommand (@"teleport Desperation Andromeda/175/211/330")
    (thisClient.ExecuteCommand (@"thread appearance"))   
    )
 )

(def (on-simulator-connected simulator)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-simulator-connected {0} )" (str simulator)) )
    )
 )
 
;----------------------------------
; Avatars and objects
;----------------------------------
(def (on-new-avatar  avatar-name avatar-uuid)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-new-avatar {0} {1})" (str avatar-name)(str avatar-uuid)) )
    )
 )

(def (on-new-prim  primID prim-uuid prim-description)
  (progn
    (prim-check prim-uuid)
    ;; (thisClient.output (@"fromLispExample: (on-new-prim {0} {1})" (str primID)(str prim-uuid)(str prim-description)) )
    )
 )
(def (on-new-foliage  foliage-name foliage-uuid foliage-description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-new-prim {0} {1})" (str foliage-name)(str foliage-uuid)(str foliage-description)) )
    )
 )

;; DotLisp/Clojure the == is equalp
(def (notme agent)
   ;; TODO: actually compare it to login name...
   (not (== (str agent) Client.Self.Name)))

(def (prim-check primUUID) ) 
;-----------------------------
; In World Events
;-----------------------------
;  (on-chat agent message) -> "(heard (agent) message)";
(def (on-chat agent message)
    ;; (thisClient.output (@"fromLispExample: (heard {0} '{1}')" (notme agent)(str message)))
    ;(thisClient.ExecuteCommand (@"say I heard {0} '{1}')" (str agent)(str message))
    ;(thisClient.ExecuteCommand (@"say I heard {0} '{1}')" (str agent)(str message))
    ; (thisClient.ExecuteCommand (@"{0}" (str message)))
  (if
   (notme agent)
    ;; end this progn
    (setj message (str message))
    ; (setj messageList (into nil (message.Split(" "))))     
    ; (when (member messageList wamo ) (thisClient.ExecuteCommand "say I just saw wamo"))
     (if (>= (message.IndexOf "hello" ) 0)
        (thisClient.ExecuteCommand (@"say Hello there {0} !!!" (str agent)))
     
     (if (>= (message.IndexOf "use" ) 0) 
        (thisClient.ExecuteCommand (@"{0}" (str message)))
     
     (if (>= (message.IndexOf "follow" ) 0) 
        (thisClient.ExecuteCommand (@"{0}" (str message)))

     (if (>= (message.IndexOf "cogbot" ) 0)
       (progn
        (setj mycommand (str message))
        (setj mycommand1 (mycommand.Replace "cogbot " ""))
        (thisClient.ExecuteCommand  mycommand1 ))))))))
        
 
 ;  (on-instantmessage agent message) -> "(heard (agent) message)";
(def (on-instantmessage agent message)
  (progn
    ;; (thisClient.output (@"fromLispExample: (heard-in-im {0} '{1}')" (str agent)(str message)) )
    )
 )

 
;  (on-meanCollision perp victim) -> "(collision (perp) (victim) )";
(def (on-meanCollision perp victim)
  (progn
    ;; (thisClient.output (@"fromLispExample: (collision {0} {1})" (str perp)(str victim)) )
    )
 )
 
 ;----------------------------------
; Looking and Pointing events
; on-self-look-target occurs when someone looks or mouses at the Cogbot avatar
;----------------------------------
 (def (on-self-look-target source description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-self-look-target {0} {1})" (str source)(str description)) )
    )
 )
 
 (def (on-self-point-target source description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-self-point-target {0} {1})" (str source)(str description)) )
    )
 )

 (def (on-avatar-point source dest description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-avatar-point {0} {1} {2})" (str source)(str dest)(str description)) )
    )
 )

 (def (on-avatar-look source dest description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-avatar-look {0} {1} {2})" (str source)(str dest)(str description)) )
    )
 )
 
;---------------------------------
; avatar descriptions
;---------------------------------
;  (on-avatar-dist agent dist) -> "(distance (agent) distance)";
(def (on-avatar-dist agent dist)
  (progn
    ;; (thisClient.output (@"fromLispExample: (distance-from {0} {1})" (str agent)(str dist)) )
    )
 )

;  (on-avatar-pos agent vector) -> "(position (agent) vector)";
(def (on-avatar-pos agent vector)
  (progn
    ;; (thisClient.output (@"fromLispExample: (position {0} '{1}')" (str agent)(str vector)) )
    )
 )

;  (on-avatar-posture agent sitstand) -> "(posture (agent) sitstand)";
(def (on-avatar-posture agent sitstand)
  (progn
    ;; (thisClient.output (@"fromLispExample: (posture {0} '{1}')" (str agent)(str sitstand)) )
    )
 )

;---------------------------------
; prim descriptions
;---------------------------------
;  (on-prim-description primID description) -> "(prim-description (obj) 'description' )";
(def (on-prim-description primID description)
  (progn
    (prim-check primID)
    ;; (thisClient.output (@"fromLispExample: (prim-description {0} {1})" (str primID)(str description)) )
    )
 )

;  (on-prim-dist primID dist) -> "(distance-from-prim primID distance)";
(def (on-prim-dist primID dist)
  (progn
    ;; (thisClient.output (@"fromLispExample: (distance-from-prim {0} {1})" (str primID)(str dist)) )
    )
 )

;  (on-prim-pos primID vector) -> "(prim-position primID vector)";
(def (on-prim-pos primID vector)
  (progn
    (prim-check primID)
    ;; (thisClient.output (@"fromLispExample: (prim-position {0} {1})" (str primID)(str vector)) )
    )
 )

