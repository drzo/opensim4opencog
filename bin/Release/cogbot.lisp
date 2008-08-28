;  (on-chat agent message) -> "(heard (agent) message)";
(def (on-chat agent message)
  (block
    (thisClient.msgClient (@"(heard ({0}) '{1}')" (str agent)(str message)) )
    )
 )
 
 ;  (on-instantmessage agent message) -> "(heard (agent) message)";
(def (on-instantmessage agent message)
  (block
    (thisClient.msgClient (@"(heard-in-im ({0}) '{1}')" (str agent)(str message)) )
    )
 )

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
 
;  (on-meanCollision perp victim) -> "(collision (perp) (victim) )";
(def (on-meanCollision perp victim)
  (block
    (thisClient.msgClient (@"(collision ({0}) ({1}))" (str perp)(str victim)) )
    )
 )
 
;  (on-prim-description obj description) -> "(prim-description (obj) 'description' )";
(def (on-prim-description  obj description)
  (block
    (thisClient.msgClient (@"(prim-description ({0}) ({1}))" (str obj)(str description)) )
    )
 )
