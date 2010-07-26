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
 

(def (on-login-fail login description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-login-fail {0} {1})" (str login)(str description)) )
    )
 )

(def (on-login-success  login description)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-login-success {0} {1})" (str login)(str description)) )
    )
 )


 (def (on-network-connected &opt reason message)
   ;; too early for snything usefull
   )
    
 (def (on-network-disconnected reason message)
  (progn
    (thisClient.output (@"fromLispExample: (on-network-disconnected {0} {1})" (str reason)(str message)) )
    (thisClient.output (@"Why am I disconnected ???"))
    (thisClient.ExecuteCommand @"login")
    )
 )
 ;;Nephrael Rae: [on-object-animation '(avatar "Candie Brooks") "TALK"][on-object-animation '(avatar "Candie Brooks") "STAND_1"][on-object-animation '(avatar "Candie Brooks") "e45fbdc9-af8f-9408-f742-fcb8c341d2c8"]
 ;--------------------------------------
 ; Here the bot is officially connected (I think), so you could 
 ; have it perform a initial inworld tasks like wearing some clothes
 ;-------------------------------------
 (def (on-first-sim-connected &opt reason message)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-network-connected )" ) )
    ;; annoys people sometimes in SL  
    ;; (thisClient.ExecuteCommand (@"thread say {0} joins the sim." (str Client.Self.Name)) )   
    ;;(thisClient.ExecuteCommand (@"teleport Nakama/128.08/111.95/22.06"))
    ;; this works but the next is faster (thisClient.ExecuteCommand (@"use HMG to wear"))
    ;;   (thisClient.ExecuteCommand (@"wear Clothing/CBC"))
    ;; interesting places
    ;; (thisClient.ExecuteCommand (@"teleport Desperation Andromeda/175/211/330"))
    ;;(thisClient.ExecuteCommand (@"thread appearance"))
    ;;(thisClient.ExecuteCommand (@"thread wear bake Clothing/Default/IRobot"))
    ;;(thisClient.ExecuteCommand (@"wear Clothing/NASA Vex"))
    ;; (thisClient.ExecuteCommand (@"thread wear Clothing/EXOGURL3"))

    )
 )

(def (on-simulator-connected simulator)
  (progn
    ;; (thisClient.output (@"fromLispExample: (on-simulator-connected {0} )" (str simulator)))

    )
 )
 
;;  OpenMetaverse.Simulator OpenMetaverse.NetworkManager+DisconnectType
(def (on-sim-disconnected &opt who simulator reason who2)
 (progn 
   (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator reason))
   (thisClient.ExecuteCommand "login")
   ))

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
(def (nop) )


;Kotoko Irata: [on-chat "hello" "Fully" "Normal" "Agent" "Daxxon Kinoc" '(avatar "Daxxon Kinoc") '(avatar "Daxxon Kinoc") '(Vector3 24.30815 101.2793 30.89464)]

(def (on-heardx message agent)
     (thisClient.Self.OnChat 
           (str message) 
            ChatAudibleLevel.Fully 
            ChatType.Normal 
            ChatSourceType.Agent 
            (str agent) 
            (.ID (avatar (str agent))) 
            (.ID (avatar (str agent)))
            (.GetSimPosition thisClient.WorldSystem.TheSimAvatar)
       )
)
;;;; CREATE EVENTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (avatar  "Maria BraveNewBot")
(def (avatar id &rest notused) (thisClient.WorldSystem.GetPrimitive (vector id ) 1))
;; eval (on-heard "hello there" "Maria BraveNewBot")
(def (on-heard message agent)
     (let (av () id () pos ())
        (setj av (avatar (str agent)))
        (setj pos (if av (.Position av) (.GetSimPosition thisClient.WorldSystem.TheSimAvatar)))
        (setj id (if av (.ID av) OpenMetaverse:UUID:Zero))
        (thisClient.Self.OnChat 
           (str message) 
            OpenMetaverse:ChatAudibleLevel:Fully 
            OpenMetaverse:ChatType:Normal 
            OpenMetaverse:ChatSourceType:Agent 
            (str agent) 
            id
            id
            pos	 )
       )
)

;;(def (on-chat  message a1 a2 a3 agent ~rest)
;;  System.String OpenMetaverse.ChatAudibleLevel OpenMetaverse.ChatType OpenMetaverse.ChatSourceType System.String OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3
(def (on-chat agent message audible type sourceType  position)
    ;; (thisClient.output (@"fromLispExample: (heard {0} '{1}')" (notme agent)(str message)))
    ;(thisClient.ExecuteCommand (@"say I heard {0} '{1}')" (str agent)(str message))
    ;(thisClient.ExecuteCommand (@"say I heard {0} '{1}')" (str agent)(str message))
    ; (thisClient.ExecuteCommand (@"{0}" (str message)))
  (setj agent (str agent))
 (progn    ;; end this progn

  (if
   (notme agent)
    (progn
    
    (setj message (str message))
    ; (setj messageList (into nil (message.Split(" "))))     
    ; (when (member messageList wamo ) (thisClient.ExecuteCommand "say I just saw wamo"))
     (if (>= (message.IndexOf "hello" ) 0)
        (thisClient.ExecuteCommand (@"say Hello there {0} !!!" (str agent)))
      )
     
     (if (>= (message.IndexOf "use" ) 0) 
        (thisClient.ExecuteCommand (@"{0}" (str message)))
      )
     
     (if (>= (message.IndexOf "follow" ) 0) 
        (thisClient.ExecuteCommand (@"{0}" (str message)))
	  )

     (if (>= (message.IndexOf "cogbot" ) 0)
       (progn
        (setj mycommand (str message))
        (setj mycommand1 (mycommand.Replace "cogbot " ""))
        (thisClient.ExecuteCommand  mycommand1 )
        )
     )
        
     (when (and (>= (message.IndexOf "solr" ) 0)  (>= (agent.IndexOf "Daxxon" ) 0))
       (progn 
        (setj mycommand (str message))
        (setj mycommand1 (mycommand.Replace "solr " ""))
        (thisClient.XML2Lisp2 "http://daxhub.selfip.net/solrwiki2/cogbot.php?q="  mycommand1 )
       )
     )
       
    )
    )
   )
 )
 
 ;  (on-instantmessage agent message) -> "(heard (agent) message)";
(def (on-instant-message im simulator)
  (progn
    (setj agent (.FromAgentName im))
    (setj message (.Message im))
    (thisClient.output (@"fromLispExample: (heard-in-im {0} '{1}')" (str agent)(str message)) )
    )
 )

 
;;  OpenMetaverse.MeanCollisionType OpenMetaverse.UUID OpenMetaverse.UUID System.Single System.DateTime
;  (on-meanCollision perp victim) -> "(collision (perp) (victim) )";
(def (on-mean-collision type perp victim magnitude time)
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


;;  OpenMetaverse.UUID
(def (on-group-dropped groupID)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  groupID)))
)


;;  OpenMetaverse.UUID System.Boolean
(def (on-group-member-ejected groupID success)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  groupID success)))
)


;;  OpenMetaverse.UUID OpenMetaverse.GroupNoticeList
(def (on-group-notices-list groupID notice)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  groupID notice)))
)


;;  OpenMetaverse.AssetDownload OpenMetaverse.Asset
(def (on-asset-received transfer asset)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  transfer asset)))
)


;;  OpenMetaverse.XferDownload
(def (on-xfer-received xfer)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  xfer)))
)


;;  OpenMetaverse.ImageDownload OpenMetaverse.AssetTexture
(def (on-image-received image asset)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  image asset)))
)


;;  OpenMetaverse.UUID System.Int32 System.Int32 System.Int32
(def (on-image-receive-progress image lastPacket recieved total)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  image lastPacket recieved total)))
)


;;  OpenMetaverse.AssetUpload
(def (on-asset-uploaded upload)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  upload)))
)


;;  OpenMetaverse.AssetUpload
(def (on-upload-progress upload)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  upload)))
)


;; 
(def (on-agent-wearables)
 ;; (progn (thisClient.WriteLine (@"fromLispExample: " )))
)


;;  OpenMetaverse.Primitive+TextureEntry
(def (on-appearance-updated te)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  te)))
)


;;  OpenMetaverse.InventoryItem
(def (on-item-received item)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  item)))
)


;;  OpenMetaverse.UUID
(def (on-folder-updated folderID)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  folderID)))
)


;;  OpenMetaverse.InstantMessage OpenMetaverse.AssetType OpenMetaverse.UUID System.Boolean
(def (on-object-offered offerDetails type objectID fromTask)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  offerDetails type objectID fromTask)))
)


;;  System.String OpenMetaverse.UUID
(def (on-find-object-by-path path inventoryObjectID)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  path inventoryObjectID)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.InventoryType
(def (on-task-item-received itemID folderID creatorID assetID type)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  itemID folderID creatorID assetID type)))
)


;;  OpenMetaverse.UUID System.Int16 System.String
(def (on-task-inventory-reply itemID serial assetFilename)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  itemID serial assetFilename)))
)


;;  System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+Classified]
(def (on-classified-reply classifieds)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  classifieds)))
)


;;  System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+DirectoryParcel]
(def (on-dir-land-reply dirParcels)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  dirParcels)))
)


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+AgentSearchData]
(def (on-dir-people-reply queryID matchedPeople)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID matchedPeople)))
)


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+GroupSearchData]
(def (on-dir-groups-reply queryID matchedGroups)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID matchedGroups)))
)


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+PlacesSearchData]
(def (on-places-reply queryID matchedPlaces)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID matchedPlaces)))
)


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+EventsSearchData]
(def (on-events-reply queryID matchedEvents)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID matchedEvents)))
)


;;  OpenMetaverse.DirectoryManager+EventInfo
(def (on-event-info matchedEvent)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  matchedEvent)))
)


;;  OpenMetaverse.Simulator System.Int32 System.Int32 System.Int32 System.Single[]
(def (on-land-patch simulator x y width data)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  simulator x y width data)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID System.Single System.Byte
(def (on-attach-sound soundID ownerID objectID gain flags)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  soundID ownerID objectID gain flags)))
)


;;  OpenMetaverse.UUID System.Single
(def (on-attach-sound-gain-change objectID gain)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  objectID gain)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID System.Single System.UInt64 OpenMetaverse.Vector3
(def (on-sound-trigger soundID ownerID objectID parentID gain regionHandle position)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6}"  soundID ownerID objectID parentID gain regionHandle position)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID
(def (on-preload-sound soundID ownerID objectID)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  soundID ownerID objectID)))
)


;;  OpenMetaverse.UUID OpenMetaverse.InternalDictionary`2[OpenMetaverse.UUID,System.Int32]
(def (on-avatar-animation agentID agentAnimations)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  agentID agentAnimations)))
)


;;  OpenMetaverse.LoginStatus System.String
(def (on-login login message)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  login message)))
)


;;  System.Object
(def (on-connected sender)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  sender)))
)


;;  System.Collections.Generic.List`1[OpenMetaverse.UUID]
(def (on-logout-reply inventoryItems)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  inventoryItems)))
)


;;  OpenMetaverse.Simulator
(def (on-sim-connecting simulator)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  simulator)))
)


;;  OpenMetaverse.Simulator
(def (on-sim-connected simulator)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  simulator)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.NetworkManager+DisconnectType
(def (on-sim-disconnected simulator reason)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator reason)))
)


;;  OpenMetaverse.NetworkManager+DisconnectType System.String
(def (on-disconnected reason message)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  reason message)))
)


;;  OpenMetaverse.Simulator
(def (on-current-sim-changed PreviousSimulator)
 (progn (thisClient.WriteLine (@"fromLispExample: on-current-sim-changed {0}"  PreviousSimulator)) 
 
 ))


;;  OpenMetaverse.Simulator
(def (on-event-queue-running simulator)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  simulator)))
)


;;  OpenMetaverse.UUID System.Int32 System.Single
(def (on-parcel-dwell parcelID localID dwell)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  parcelID localID dwell)))
)


;;  OpenMetaverse.ParcelInfo
(def (on-parcel-info parcel)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  parcel)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.Parcel OpenMetaverse.ParcelResult System.Int32 System.Int32 System.Boolean
(def (on-parcel-properties simulator parcel result selectedPrims sequenceID snapSelection)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  simulator parcel result selectedPrims sequenceID snapSelection)))
)


;;  OpenMetaverse.Simulator System.Int32 System.Int32 System.UInt32 System.Collections.Generic.List`1[OpenMetaverse.ParcelManager+ParcelAccessEntry]
(def (on-access-list-reply simulator sequenceID localID flags accessEntries)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  simulator sequenceID localID flags accessEntries)))
)


;;  OpenMetaverse.Simulator System.Collections.Generic.List`1[OpenMetaverse.ParcelManager+ParcelPrimOwners]
(def (on-prim-owners-list-reply simulator primOwners)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator primOwners)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.InternalDictionary`2[System.Int32,OpenMetaverse.Parcel] System.Int32[,]
(def (on-sim-parcels-downloaded simulator simParcels parcelMap)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  simulator simParcels parcelMap)))
)


;;  OpenMetaverse.Simulator System.Collections.Generic.List`1[System.UInt32] System.Boolean
(def (on-parcel-selected-objects simulator objectIDs resetList)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  simulator objectIDs resetList)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.ParcelMedia
(def (on-parcel-media-update simulator media)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator media)))
)


;;  System.String System.String OpenMetaverse.UUID OpenMetaverse.UUID System.String System.String System.Int32 System.Collections.Generic.List`1[System.String]
(def (on-script-dialog message objectName imageID objectID firstName lastName chatChannel buttons)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6} {7}"  message objectName imageID objectID firstName lastName chatChannel buttons)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.UUID OpenMetaverse.UUID System.String System.String OpenMetaverse.ScriptPermission
(def (on-script-question simulator taskID itemID objectName objectOwner questions)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  simulator taskID itemID objectName objectOwner questions)))
)


;;  System.String OpenMetaverse.UUID OpenMetaverse.UUID System.Boolean System.String System.String
(def (on-load-u-r-l objectName objectID ownerID ownerIsGroup message URL)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  objectName objectID ownerID ownerIsGroup message URL)))
)


;;  OpenMetaverse.InstantMessage OpenMetaverse.Simulator
(def (on-instant-message im simulator)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  im simulator)))
)


;;  System.String OpenMetaverse.TeleportStatus OpenMetaverse.TeleportFlags
(def (on-teleport message status flags)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  message status flags)))
)


;;  System.Int32
(def (on-balance-updated balance)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  balance)))
)


;;  OpenMetaverse.UUID System.Boolean System.Int32 System.Int32 System.Int32 System.String
(def (on-money-balance-reply-received transactionID transactionSuccess balance metersCredit metersCommitted description)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  transactionID transactionSuccess balance metersCredit metersCommitted description)))
)


;;  System.String System.String OpenMetaverse.UUID System.String OpenMetaverse.GroupPowers System.String
(def (on-agent-data-updated firstName lastName activeGroupID groupTitle groupPowers groupName)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  firstName lastName activeGroupID groupTitle groupPowers groupName)))
)


;;  OpenMetaverse.InternalDictionary`2[OpenMetaverse.UUID,System.Int32]
(def (on-animations-changed agentAnimations)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  agentAnimations)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.Simulator
(def (on-region-crossed oldSim newSim)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  oldSim newSim)))
)


;;  OpenMetaverse.UUID System.String OpenMetaverse.UUID System.Boolean
(def (on-group-chat-join groupChatSessionID sessionName tmpSessionID success)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  groupChatSessionID sessionName tmpSessionID success)))
)


;;  OpenMetaverse.UUID
(def (on-group-chat-left groupchatSessionID)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  groupchatSessionID)))
)


;;  System.String
(def (on-alert-message message)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  message)))
)


;;  OpenMetaverse.ScriptControlChange System.Boolean System.Boolean
(def (on-script-control-change controls pass take)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  controls pass take)))
)


;;  OpenMetaverse.Vector4
(def (on-camera-constraint collidePlane)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  collidePlane)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID System.String OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3 System.Single OpenMetaverse.Quaternion OpenMetaverse.ScriptSensorTypeFlags OpenMetaverse.Vector3
(def (on-script-sensor-reply requestorID groupID name objectID ownerID position range rotation type velocity)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}"  requestorID groupID name objectID ownerID position range rotation type velocity)))
)


;;  OpenMetaverse.UUID System.Boolean OpenMetaverse.Vector3 OpenMetaverse.Vector3 System.Boolean OpenMetaverse.Vector3 OpenMetaverse.Quaternion
(def (on-avatar-sit-response objectID autoPilot cameraAtOffset cameraEyeOffset forceMouselook sitPosition sitRotation)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6}"  objectID autoPilot cameraAtOffset cameraEyeOffset forceMouselook sitPosition sitRotation)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID
(def (on-chat-session-member-added sessionID agent_key)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  sessionID agent_key)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID
(def (on-chat-session-member-left sessionID agent_key)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  sessionID agent_key)))
)


;;  OpenMetaverse.UUID System.Boolean OpenMetaverse.Primitive+TextureEntryFace OpenMetaverse.Primitive+TextureEntryFace[] System.Collections.Generic.List`1[System.Byte]
(def (on-avatar-appearance avatarID isTrial defaultTexture faceTextures visualParams)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  avatarID isTrial defaultTexture faceTextures visualParams)))
)


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-avatar-names names)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  names)))
)


;;  OpenMetaverse.UUID OpenMetaverse.Avatar+Interests
(def (on-avatar-interests avatarID interests)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  avatarID interests)))
)


;;  OpenMetaverse.UUID OpenMetaverse.Avatar+AvatarProperties
(def (on-avatar-properties avatarID properties)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  avatarID properties)))
)


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.AvatarGroup]
(def (on-avatar-groups avatarID avatarGroups)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  avatarID avatarGroups)))
)


;;  OpenMetaverse.UUID System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-avatar-name-search queryID avatars)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID avatars)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3d OpenMetaverse.PointAtType System.Single OpenMetaverse.UUID
(def (on-point-at sourceID targetID targetPos pointType duration id)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  sourceID targetID targetPos pointType duration id)))
)


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3d OpenMetaverse.LookAtType System.Single OpenMetaverse.UUID
(def (on-look-at sourceID targetID targetPos lookType duration id)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  sourceID targetID targetPos lookType duration id)))
)


;;  OpenMetaverse.EffectType OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3d System.Single OpenMetaverse.UUID
(def (on-effect type sourceID targetID targetPos duration id)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  type sourceID targetID targetPos duration id)))
)


;;  OpenMetaverse.UUID System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-avatar-picks avatarid picks)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  avatarid picks)))
)


;;  OpenMetaverse.UUID OpenMetaverse.ProfilePick
(def (on-pick-info pickid pick)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  pickid pick)))
)


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-friend-names-received names)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  names)))
)


;;  OpenMetaverse.FriendInfo
(def (on-friend-online friend)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  friend)))
)


;;  OpenMetaverse.FriendInfo
(def (on-friend-offline friend)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  friend)))
)


;;  OpenMetaverse.FriendInfo
(def (on-friend-rights friend)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  friend)))
)


;;  OpenMetaverse.UUID System.String OpenMetaverse.UUID
(def (on-friendship-offered agentID agentName imSessionID)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  agentID agentName imSessionID)))
)


;;  OpenMetaverse.UUID System.String System.Boolean
(def (on-friendship-response agentID agentName accepted)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  agentID agentName accepted)))
)


;;  OpenMetaverse.UUID System.String
(def (on-friendship-terminated agentID agentName)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  agentID agentName)))
)


;;  OpenMetaverse.UUID System.UInt64 OpenMetaverse.Vector3
(def (on-friend-found agentID regionHandle location)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  agentID regionHandle location)))
)


;;  OpenMetaverse.Simulator System.Collections.Generic.List`1[OpenMetaverse.UUID] System.Collections.Generic.List`1[OpenMetaverse.UUID]
(def (on-coarse-location-update sim newEntries removedEntries)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  sim newEntries removedEntries)))
)


;;  OpenMetaverse.GridRegion
(def (on-grid-region region)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  region)))
)


;;  OpenMetaverse.GridLayer
(def (on-grid-layer layer)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  layer)))
)


;;  OpenMetaverse.GridItemType System.Collections.Generic.List`1[OpenMetaverse.GridItem]
(def (on-grid-items type items)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  type items)))
)


;;  OpenMetaverse.UUID System.UInt64
(def (on-region-handle-reply regionID regionHandle)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  regionID regionHandle)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.Primitive System.UInt64 System.UInt16
(def (on-new-prim simulator prim regionHandle timeDilation)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator prim regionHandle timeDilation)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.Primitive System.UInt64 System.UInt16
(def (on-new-attachment simulator prim regionHandle timeDilation)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator prim regionHandle timeDilation)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.Avatar System.UInt64 System.UInt16
(def (on-new-avatar simulator avatar regionHandle timeDilation)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator avatar regionHandle timeDilation)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.ObjectUpdate System.UInt64 System.UInt16
(def (on-object-updated simulator update regionHandle timeDilation)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator update regionHandle timeDilation)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.Avatar System.UInt32 System.UInt32
(def (on-avatar-sit-changed simulator avatar sittingOn oldSeat)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator avatar sittingOn oldSeat)))
)


;;  OpenMetaverse.Simulator System.UInt32
(def (on-object-killed simulator objectID)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator objectID)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.Primitive+ObjectProperties
(def (on-object-properties simulator props)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator props)))
)


;;  OpenMetaverse.Simulator OpenMetaverse.Primitive+ObjectProperties OpenMetaverse.ReportType
(def (on-object-properties-family simulator props type)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  simulator props type)))
)


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,OpenMetaverse.Group]
(def (on-current-groups groups)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  groups)))
)


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-group-names groupNames)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  groupNames)))
)


;;  OpenMetaverse.Group
(def (on-group-profile group)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  group)))
)


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,OpenMetaverse.GroupMember]
(def (on-group-members members)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  members)))
)


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,OpenMetaverse.GroupRole]
(def (on-group-roles roles)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  roles)))
)


;;  System.Collections.Generic.List`1[System.Collections.Generic.KeyValuePair`2[OpenMetaverse.UUID,OpenMetaverse.UUID]]
(def (on-group-roles-members rolesMembers)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  rolesMembers)))
)


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,OpenMetaverse.GroupTitle]
(def (on-group-titles titles)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  titles)))
)


;;  OpenMetaverse.GroupAccountSummary
(def (on-group-account-summary summary)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  summary)))
)


;;  OpenMetaverse.UUID System.Boolean System.String
(def (on-group-created groupID success message)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  groupID success message)))
)


;;  OpenMetaverse.UUID System.Boolean
(def (on-group-joined groupID success)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  groupID success)))
)


;;  OpenMetaverse.UUID System.Boolean
(def (on-group-left groupID success)
 ;; (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  groupID success)))
)
 
  

