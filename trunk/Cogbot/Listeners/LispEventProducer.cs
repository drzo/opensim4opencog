using System;
using System.Collections.Generic;
using System.Reflection;
using cogbot.ScriptEngines;
using System.IO;
using DotLisp;

namespace cogbot.Listeners
{
    public class LispEventProducer : AllEvents
    {
        private ScriptInterpreter Interpreter;
        public LispEventProducer(BotClient bc, ScriptInterpreter interp)
            : base(bc)
        {
            Interpreter = interp;
            // the subclass must now run this                
            RegisterAll();
        }

        ~LispEventProducer()
        {
            UnregisterAll();
        }

        public static bool EventArgsInDictionary = true;
        public override bool BooleanOnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters)
        {
            if (Interpreter==null) return true;
            if (EventArgsInDictionary)
            {
                Dictionary<String, object> eventArgs = new Dictionary<string, object>();
                for (int i = 0; i < paramNames.Length; i++)
                {
                    eventArgs.Add(paramNames[i], CoerceArg(parameters[i], paramTypes[i]));
                }
                object o = Interpreter.Eval(new Cons(eventName, new Cons(eventArgs, null)));
            }
            else
            {
                Cons invokeMe = null;
                Dictionary<String, object> eventArgs = new Dictionary<string, object>();
                for (int i = paramNames.Length - 1; i >= 0; i--)
                {
                    invokeMe = new Cons(CoerceArg(parameters[i], paramTypes[i]), invokeMe);
                }
                object o = Interpreter.Eval(new Cons(eventName, invokeMe));
            }
            // right now most events are void but there is one that is boolean which means we may as well eturn true for all
            return true;
        }

        public object CoerceArg(object p, Type type)
        {
            return Interpreter.ConvertArgToLisp(p);
        }

        public void MakeStubFile(String filename)
        {
            if (EventArgsInDictionary)
            {
                MakeStubFileDictionary(filename);
                return;
            }
            MakeStubFileNoDictionary(filename);
        }
        public void MakeStubFileNoDictionary(string filename)
        {
            Type eventClass = typeof(AllEvents);
            FileStream FS = new FileStream(filename, FileMode.CreateNew);
            TextWriter TW = new StreamWriter(FS);
            foreach (MethodInfo mi in eventClass.GetMethods())
            {

                int fa = mi.Name.IndexOf("_");
                if (fa > 0)
                {
                    string LispCall = "\n\n;; ";
                    string EventName = mi.Name.Substring(fa + 1);
                    FieldInfo FI = eventClass.GetField("paramTypes" + EventName);
                    Type[] types = (Type[])FI.GetValue(this);
                    for (int i = 0; i < types.Length; i++)
                    {
                        LispCall += " ";
                        LispCall += types[i];
                    }
                    LispCall += "\n(def (" + EventName.Substring(0, 1).ToLower();
                    for (int i = 1; i < EventName.Length; i++)
                    {
                        char ch = EventName[i];
                        if (Char.IsUpper(ch))
                        {
                            LispCall += "-";
                        }
                        LispCall += Char.ToLower(ch);
                    }
                    FI = eventClass.GetField("paramNames" + EventName);
                    string[] names = (string[])FI.GetValue(this);
                    for (int i = 0; i < names.Length; i++)
                    {
                        LispCall += " ";
                        LispCall += names[i];
                    }
                    LispCall += ")\n (progn (thisClient.WriteLine (@\"fromLispExample: ";
                    for (int i = 0; i < names.Length; i++)
                    {
                        LispCall += " {" + i + "}";
                    }
                    LispCall += "\" ";
                    for (int i = 0; i < names.Length; i++)
                    {
                        LispCall += " ";
                        LispCall += names[i];
                    }
                    LispCall += "))))\n";
                    TW.Write(LispCall);
                }
                TW.Flush();
            }
            FS.Close();
            //FS.Write()
            // FS.Write();


        }

        public void MakeStubFileDictionary(string filename)
        {
            Type eventClass = typeof (AllEvents);
            FileStream FS = new FileStream(filename, FileMode.CreateNew);
            TextWriter TW = new StreamWriter(FS);
            foreach (MethodInfo mi in eventClass.GetMethods())
            {

                int fa = mi.Name.IndexOf("_");
                if (fa > 0)
                {
                    string LispCall = "\n\n;; ";
                    string EventName = mi.Name.Substring(fa + 1);
                    FieldInfo FI = eventClass.GetField("paramTypes" + EventName);
                    Type[] types = (Type[])FI.GetValue(this);
                    FI = eventClass.GetField("paramNames" + EventName);
                    string[] names = (string[])FI.GetValue(this);
    
                    for (int i = 0; i < types.Length; i++)
                    {
                        LispCall += "  ";
                        LispCall += types[i];
                        LispCall += ":";
                        LispCall += names[i];
                    }

                    LispCall += "\n(def (" + EventName.Substring(0, 1).ToLower();
                    for (int i = 1; i < EventName.Length; i++)
                    {
                        char ch = EventName[i];
                        if (Char.IsUpper(ch))
                        {
                            LispCall += "-";
                        }
                        LispCall += Char.ToLower(ch);
                    }
                    LispCall += " evArgs)\n (progn (thisClient.WriteLine (@\"fromLispExample: ";
                    for (int i = 0; i < names.Length; i++)
                    {
                        LispCall += " {" + i + "}";
                    }
                    LispCall += "\"";
                    for (int i = 0; i < names.Length; i++)
                    {
                        LispCall += " (get \"";
                        LispCall += names[i];
                        LispCall += "\"" + " evArgs)";
                    }
                    LispCall += "))))\n";
                    TW.Write(LispCall);
                }
                TW.Flush();
            }
            FS.Close();
            //FS.Write()
            // FS.Write();


        }
    }
}

/*
 Generates:
 * 
 * 
 *  
 
 

;;  OpenMetaverse.UUID
(def (on-group-dropped groupID)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  groupID))))


;;  OpenMetaverse.UUID System.Boolean
(def (on-group-member-ejected groupID success)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  groupID success))))


;;  OpenMetaverse.UUID OpenMetaverse.GroupNoticeList
(def (on-group-notices-list groupID notice)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  groupID notice))))


;;  OpenMetaverse.AssetDownload OpenMetaverse.Asset
(def (on-asset-received transfer asset)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  transfer asset))))


;;  OpenMetaverse.XferDownload
(def (on-xfer-received xfer)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  xfer))))


;;  OpenMetaverse.ImageDownload OpenMetaverse.AssetTexture
(def (on-image-received image asset)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  image asset))))


;;  OpenMetaverse.UUID System.Int32 System.Int32 System.Int32
(def (on-image-receive-progress image lastPacket recieved total)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  image lastPacket recieved total))))


;;  OpenMetaverse.AssetUpload
(def (on-asset-uploaded upload)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  upload))))


;;  OpenMetaverse.AssetUpload
(def (on-upload-progress upload)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  upload))))


;; 
(def (on-agent-wearables)
 (progn (thisClient.WriteLine (@"fromLispExample: " ))))


;;  OpenMetaverse.Primitive+TextureEntry
(def (on-appearance-updated te)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  te))))


;;  OpenMetaverse.InventoryItem
(def (on-item-received item)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  item))))


;;  OpenMetaverse.UUID
(def (on-folder-updated folderID)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  folderID))))


;;  OpenMetaverse.InstantMessage OpenMetaverse.AssetType OpenMetaverse.UUID System.Boolean
(def (on-object-offered offerDetails type objectID fromTask)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  offerDetails type objectID fromTask))))


;;  System.String OpenMetaverse.UUID
(def (on-find-object-by-path path inventoryObjectID)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  path inventoryObjectID))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.InventoryType
(def (on-task-item-received itemID folderID creatorID assetID type)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  itemID folderID creatorID assetID type))))


;;  OpenMetaverse.UUID System.Int16 System.String
(def (on-task-inventory-reply itemID serial assetFilename)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  itemID serial assetFilename))))


;;  System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+Classified]
(def (on-classified-reply classifieds)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  classifieds))))


;;  System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+DirectoryParcel]
(def (on-dir-land-reply dirParcels)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  dirParcels))))


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+AgentSearchData]
(def (on-dir-people-reply queryID matchedPeople)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID matchedPeople))))


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+GroupSearchData]
(def (on-dir-groups-reply queryID matchedGroups)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID matchedGroups))))


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+PlacesSearchData]
(def (on-places-reply queryID matchedPlaces)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID matchedPlaces))))


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.DirectoryManager+EventsSearchData]
(def (on-events-reply queryID matchedEvents)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID matchedEvents))))


;;  OpenMetaverse.DirectoryManager+EventInfo
(def (on-event-info matchedEvent)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  matchedEvent))))


;;  OpenMetaverse.Simulator System.Int32 System.Int32 System.Int32 System.Single[]
(def (on-land-patch simulator x y width data)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  simulator x y width data))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID System.Single System.Byte
(def (on-attach-sound soundID ownerID objectID gain flags)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  soundID ownerID objectID gain flags))))


;;  OpenMetaverse.UUID System.Single
(def (on-attach-sound-gain-change objectID gain)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  objectID gain))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID System.Single System.UInt64 OpenMetaverse.Vector3
(def (on-sound-trigger soundID ownerID objectID parentID gain regionHandle position)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6}"  soundID ownerID objectID parentID gain regionHandle position))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.UUID
(def (on-preload-sound soundID ownerID objectID)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  soundID ownerID objectID))))


;;  OpenMetaverse.UUID OpenMetaverse.InternalDictionary`2[OpenMetaverse.UUID,System.Int32]
(def (on-avatar-animation agentID agentAnimations)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  agentID agentAnimations))))


;;  OpenMetaverse.LoginStatus System.String
(def (on-login login message)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  login message))))


;;  System.Object
(def (on-connected sender)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  sender))))


;;  System.Collections.Generic.List`1[OpenMetaverse.UUID]
(def (on-logout-reply inventoryItems)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  inventoryItems))))


;;  OpenMetaverse.Simulator
(def (on-sim-connecting simulator)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  simulator))))


;;  OpenMetaverse.Simulator
(def (on-sim-connected simulator)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  simulator))))


;;  OpenMetaverse.Simulator OpenMetaverse.NetworkManager+DisconnectType
(def (on-sim-disconnected simulator reason)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator reason))))


;;  OpenMetaverse.NetworkManager+DisconnectType System.String
(def (on-disconnected reason message)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  reason message))))


;;  OpenMetaverse.Simulator
(def (on-current-sim-changed PreviousSimulator)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  PreviousSimulator))))


;;  OpenMetaverse.Simulator
(def (on-event-queue-running simulator)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  simulator))))


;;  OpenMetaverse.UUID System.Int32 System.Single
(def (on-parcel-dwell parcelID localID dwell)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  parcelID localID dwell))))


;;  OpenMetaverse.ParcelInfo
(def (on-parcel-info parcel)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  parcel))))


;;  OpenMetaverse.Simulator OpenMetaverse.Parcel OpenMetaverse.ParcelResult System.Int32 System.Int32 System.Boolean
(def (on-parcel-properties simulator parcel result selectedPrims sequenceID snapSelection)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  simulator parcel result selectedPrims sequenceID snapSelection))))


;;  OpenMetaverse.Simulator System.Int32 System.Int32 System.UInt32 System.Collections.Generic.List`1[OpenMetaverse.ParcelManager+ParcelAccessEntry]
(def (on-access-list-reply simulator sequenceID localID flags accessEntries)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  simulator sequenceID localID flags accessEntries))))


;;  OpenMetaverse.Simulator System.Collections.Generic.List`1[OpenMetaverse.ParcelManager+ParcelPrimOwners]
(def (on-prim-owners-list-reply simulator primOwners)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator primOwners))))


;;  OpenMetaverse.Simulator OpenMetaverse.InternalDictionary`2[System.Int32,OpenMetaverse.Parcel] System.Int32[,]
(def (on-sim-parcels-downloaded simulator simParcels parcelMap)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  simulator simParcels parcelMap))))


;;  OpenMetaverse.Simulator System.Collections.Generic.List`1[System.UInt32] System.Boolean
(def (on-parcel-selected-objects simulator objectIDs resetList)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  simulator objectIDs resetList))))


;;  OpenMetaverse.Simulator OpenMetaverse.ParcelMedia
(def (on-parcel-media-update simulator media)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator media))))


;;  System.String OpenMetaverse.ChatAudibleLevel OpenMetaverse.ChatType OpenMetaverse.ChatSourceType System.String OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3
(def (on-chat message audible type sourceType fromName id ownerid position)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6} {7}"  message audible type sourceType fromName id ownerid position))))


;;  System.String System.String OpenMetaverse.UUID OpenMetaverse.UUID System.String System.String System.Int32 System.Collections.Generic.List`1[System.String]
(def (on-script-dialog message objectName imageID objectID firstName lastName chatChannel buttons)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6} {7}"  message objectName imageID objectID firstName lastName chatChannel buttons))))


;;  OpenMetaverse.Simulator OpenMetaverse.UUID OpenMetaverse.UUID System.String System.String OpenMetaverse.ScriptPermission
(def (on-script-question simulator taskID itemID objectName objectOwner questions)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  simulator taskID itemID objectName objectOwner questions))))


;;  System.String OpenMetaverse.UUID OpenMetaverse.UUID System.Boolean System.String System.String
(def (on-load-u-r-l objectName objectID ownerID ownerIsGroup message URL)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  objectName objectID ownerID ownerIsGroup message URL))))


;;  OpenMetaverse.InstantMessage OpenMetaverse.Simulator
(def (on-instant-message im simulator)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  im simulator))))


;;  System.String OpenMetaverse.TeleportStatus OpenMetaverse.TeleportFlags
(def (on-teleport message status flags)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  message status flags))))


;;  System.Int32
(def (on-balance-updated balance)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  balance))))


;;  OpenMetaverse.UUID System.Boolean System.Int32 System.Int32 System.Int32 System.String
(def (on-money-balance-reply-received transactionID transactionSuccess balance metersCredit metersCommitted description)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  transactionID transactionSuccess balance metersCredit metersCommitted description))))


;;  System.String System.String OpenMetaverse.UUID System.String OpenMetaverse.GroupPowers System.String
(def (on-agent-data-updated firstName lastName activeGroupID groupTitle groupPowers groupName)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  firstName lastName activeGroupID groupTitle groupPowers groupName))))


;;  OpenMetaverse.InternalDictionary`2[OpenMetaverse.UUID,System.Int32]
(def (on-animations-changed agentAnimations)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  agentAnimations))))


;;  OpenMetaverse.MeanCollisionType OpenMetaverse.UUID OpenMetaverse.UUID System.Single System.DateTime
(def (on-mean-collision type perp victim magnitude time)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  type perp victim magnitude time))))


;;  OpenMetaverse.Simulator OpenMetaverse.Simulator
(def (on-region-crossed oldSim newSim)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  oldSim newSim))))


;;  OpenMetaverse.UUID System.String OpenMetaverse.UUID System.Boolean
(def (on-group-chat-join groupChatSessionID sessionName tmpSessionID success)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  groupChatSessionID sessionName tmpSessionID success))))


;;  OpenMetaverse.UUID
(def (on-group-chat-left groupchatSessionID)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  groupchatSessionID))))


;;  System.String
(def (on-alert-message message)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  message))))


;;  OpenMetaverse.ScriptControlChange System.Boolean System.Boolean
(def (on-script-control-change controls pass take)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  controls pass take))))


;;  OpenMetaverse.Vector4
(def (on-camera-constraint collidePlane)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  collidePlane))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID System.String OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3 System.Single OpenMetaverse.Quaternion OpenMetaverse.ScriptSensorTypeFlags OpenMetaverse.Vector3
(def (on-script-sensor-reply requestorID groupID name objectID ownerID position range rotation type velocity)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}"  requestorID groupID name objectID ownerID position range rotation type velocity))))


;;  OpenMetaverse.UUID System.Boolean OpenMetaverse.Vector3 OpenMetaverse.Vector3 System.Boolean OpenMetaverse.Vector3 OpenMetaverse.Quaternion
(def (on-avatar-sit-response objectID autoPilot cameraAtOffset cameraEyeOffset forceMouselook sitPosition sitRotation)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5} {6}"  objectID autoPilot cameraAtOffset cameraEyeOffset forceMouselook sitPosition sitRotation))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID
(def (on-chat-session-member-added sessionID agent_key)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  sessionID agent_key))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID
(def (on-chat-session-member-left sessionID agent_key)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  sessionID agent_key))))


;;  OpenMetaverse.UUID System.Boolean OpenMetaverse.Primitive+TextureEntryFace OpenMetaverse.Primitive+TextureEntryFace[] System.Collections.Generic.List`1[System.Byte]
(def (on-avatar-appearance avatarID isTrial defaultTexture faceTextures visualParams)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4}"  avatarID isTrial defaultTexture faceTextures visualParams))))


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-avatar-names names)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  names))))


;;  OpenMetaverse.UUID OpenMetaverse.Avatar+Interests
(def (on-avatar-interests avatarID interests)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  avatarID interests))))


;;  OpenMetaverse.UUID OpenMetaverse.Avatar+AvatarProperties
(def (on-avatar-properties avatarID properties)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  avatarID properties))))


;;  OpenMetaverse.UUID System.Collections.Generic.List`1[OpenMetaverse.AvatarGroup]
(def (on-avatar-groups avatarID avatarGroups)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  avatarID avatarGroups))))


;;  OpenMetaverse.UUID System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-avatar-name-search queryID avatars)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  queryID avatars))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3d OpenMetaverse.PointAtType System.Single OpenMetaverse.UUID
(def (on-point-at sourceID targetID targetPos pointType duration id)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  sourceID targetID targetPos pointType duration id))))


;;  OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3d OpenMetaverse.LookAtType System.Single OpenMetaverse.UUID
(def (on-look-at sourceID targetID targetPos lookType duration id)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  sourceID targetID targetPos lookType duration id))))


;;  OpenMetaverse.EffectType OpenMetaverse.UUID OpenMetaverse.UUID OpenMetaverse.Vector3d System.Single OpenMetaverse.UUID
(def (on-effect type sourceID targetID targetPos duration id)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3} {4} {5}"  type sourceID targetID targetPos duration id))))


;;  OpenMetaverse.UUID System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-avatar-picks avatarid picks)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  avatarid picks))))


;;  OpenMetaverse.UUID OpenMetaverse.ProfilePick
(def (on-pick-info pickid pick)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  pickid pick))))


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-friend-names-received names)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  names))))


;;  OpenMetaverse.FriendInfo
(def (on-friend-online friend)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  friend))))


;;  OpenMetaverse.FriendInfo
(def (on-friend-offline friend)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  friend))))


;;  OpenMetaverse.FriendInfo
(def (on-friend-rights friend)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  friend))))


;;  OpenMetaverse.UUID System.String OpenMetaverse.UUID
(def (on-friendship-offered agentID agentName imSessionID)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  agentID agentName imSessionID))))


;;  OpenMetaverse.UUID System.String System.Boolean
(def (on-friendship-response agentID agentName accepted)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  agentID agentName accepted))))


;;  OpenMetaverse.UUID System.String
(def (on-friendship-terminated agentID agentName)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  agentID agentName))))


;;  OpenMetaverse.UUID System.UInt64 OpenMetaverse.Vector3
(def (on-friend-found agentID regionHandle location)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  agentID regionHandle location))))


;;  OpenMetaverse.Simulator System.Collections.Generic.List`1[OpenMetaverse.UUID] System.Collections.Generic.List`1[OpenMetaverse.UUID]
(def (on-coarse-location-update sim newEntries removedEntries)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  sim newEntries removedEntries))))


;;  OpenMetaverse.GridRegion
(def (on-grid-region region)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  region))))


;;  OpenMetaverse.GridLayer
(def (on-grid-layer layer)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  layer))))


;;  OpenMetaverse.GridItemType System.Collections.Generic.List`1[OpenMetaverse.GridItem]
(def (on-grid-items type items)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  type items))))


;;  OpenMetaverse.UUID System.UInt64
(def (on-region-handle-reply regionID regionHandle)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  regionID regionHandle))))


;;  OpenMetaverse.Simulator OpenMetaverse.Primitive System.UInt64 System.UInt16
(def (on-new-prim simulator prim regionHandle timeDilation)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator prim regionHandle timeDilation))))


;;  OpenMetaverse.Simulator OpenMetaverse.Primitive System.UInt64 System.UInt16
(def (on-new-attachment simulator prim regionHandle timeDilation)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator prim regionHandle timeDilation))))


;;  OpenMetaverse.Simulator OpenMetaverse.Avatar System.UInt64 System.UInt16
(def (on-new-avatar simulator avatar regionHandle timeDilation)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator avatar regionHandle timeDilation))))


;;  OpenMetaverse.Simulator OpenMetaverse.ObjectUpdate System.UInt64 System.UInt16
(def (on-object-updated simulator update regionHandle timeDilation)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator update regionHandle timeDilation))))


;;  OpenMetaverse.Simulator OpenMetaverse.Avatar System.UInt32 System.UInt32
(def (on-avatar-sit-changed simulator avatar sittingOn oldSeat)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2} {3}"  simulator avatar sittingOn oldSeat))))


;;  OpenMetaverse.Simulator System.UInt32
(def (on-object-killed simulator objectID)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator objectID))))


;;  OpenMetaverse.Simulator OpenMetaverse.Primitive+ObjectProperties
(def (on-object-properties simulator props)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  simulator props))))


;;  OpenMetaverse.Simulator OpenMetaverse.Primitive+ObjectProperties OpenMetaverse.ReportType
(def (on-object-properties-family simulator props type)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  simulator props type))))


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,OpenMetaverse.Group]
(def (on-current-groups groups)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  groups))))


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,System.String]
(def (on-group-names groupNames)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  groupNames))))


;;  OpenMetaverse.Group
(def (on-group-profile group)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  group))))


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,OpenMetaverse.GroupMember]
(def (on-group-members members)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  members))))


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,OpenMetaverse.GroupRole]
(def (on-group-roles roles)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  roles))))


;;  System.Collections.Generic.List`1[System.Collections.Generic.KeyValuePair`2[OpenMetaverse.UUID,OpenMetaverse.UUID]]
(def (on-group-roles-members rolesMembers)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  rolesMembers))))


;;  System.Collections.Generic.Dictionary`2[OpenMetaverse.UUID,OpenMetaverse.GroupTitle]
(def (on-group-titles titles)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  titles))))


;;  OpenMetaverse.GroupAccountSummary
(def (on-group-account-summary summary)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0}"  summary))))


;;  OpenMetaverse.UUID System.Boolean System.String
(def (on-group-created groupID success message)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1} {2}"  groupID success message))))


;;  OpenMetaverse.UUID System.Boolean
(def (on-group-joined groupID success)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  groupID success))))


;;  OpenMetaverse.UUID System.Boolean
(def (on-group-left groupID success)
 (progn (thisClient.WriteLine (@"fromLispExample:  {0} {1}"  groupID success))))
 
  
 * 
 */
