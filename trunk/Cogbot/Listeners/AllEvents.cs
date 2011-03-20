using System;
using System.Collections.Generic;
using System.Text;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using System.Reflection;
using OpenMetaverse.Assets;

//using OpenMetaverse;
// older LibOMV
//using TeleportFlags = OpenMetaverse.AgentManager.TeleportFlags;
//using TeleportStatus = OpenMetaverse.AgentManager.TeleportStatus;
//using AgentFlags = OpenMetaverse.AgentManager.AgentFlags;
//using AgentState = OpenMetaverse.AgentManager.AgentState;

namespace cogbot.Listeners
{
#pragma warning disable 0168
    public abstract class AllEvents : AListener
    {
        public AllEvents(BotClient bot)
            : base(bot)
        {
        }

        static public readonly string[] paramNamesOnAvatarAnimation = new string[] { "agentID", "agentAnimations" };
        static public readonly Type[] paramTypesOnAvatarAnimation = new Type[] { typeof(UUID), typeof(InternalDictionary<UUID, int>) };

        public virtual void Avatars_OnAvatarAnimation(UUID agentID, InternalDictionary<UUID, int> agentAnimations) { OnEvent("On-Avatar-Animation", paramNamesOnAvatarAnimation, paramTypesOnAvatarAnimation, agentID, agentAnimations); }



        public abstract bool BooleanOnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters);
        public virtual bool OnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters)
        {
            int missingParams = paramNames.Length - parameters.Length;
            //if (missingParams != 0)
            {
                object[] old = parameters;
                parameters = new object[paramNames.Length];
                foreach (var o in old)
                {
                    fillInParams(o, paramTypes, paramNames, parameters, 2);
                }
                foreach (var o in parameters)
                {
                    if (o==null)
                    {
                        DLRConsole.DebugWriteLine("bd args in " + eventName);
                    }
                }

            }
            return BooleanOnEvent(eventName, paramNames, paramTypes, parameters);
        }

        private void fillInParams(object o, Type[] types, string[] strings, object[] parameters, int d)
        {
            if (d--<0) return;
            if (o==null) return;
            for (int i = 0; i < strings.Length; i++)
            {
                if (parameters[i] != null) continue;
                if (types[i].IsInstanceOfType(o))
                {
                    parameters[i] = o;
                    continue;
                }
                foreach (var propertyInfo in o.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public))
                {
                    if (propertyInfo.PropertyType == types[i])
                    {
                        if (SkipCall(propertyInfo)) continue;
                        if (propertyInfo.Name.ToLower() == strings[i].ToLower())
                        {
                            var val = propertyInfo.GetValue(o, null);
                            if (val != null) parameters[i] = val;
                        }
                    }
                }
            }
            for (int i = 0; i < strings.Length; i++)
            {
                if (parameters[i] != null) continue;
                foreach (var propertyInfo in o.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public))
                {
                    if (propertyInfo.PropertyType == types[i])
                    {
                        if (SkipCall(propertyInfo)) continue;
                        //if (propertyInfo.Name.ToLower() == strings[i].ToLower())
                        {
                            var val = propertyInfo.GetValue(o, null);
                            if (val != null) parameters[i] = val;
                        }
                    }
                }
            }
            for (int i = 0; i < strings.Length; i++)
            {
                if (parameters[i] != null) continue;
                foreach (var propertyInfo in o.GetType().GetFields(BindingFlags.Instance | BindingFlags.Public))
                {
                    if (propertyInfo.FieldType == types[i])
                    {
                        if (propertyInfo.Name.ToLower() == strings[i].ToLower())
                        {
                            var val = propertyInfo.GetValue(o);
                            if (val != null) parameters[i] = val;

                        }
                    }
                }
            }
            for (int i = 0; i < strings.Length; i++)
            {
                if (parameters[i] != null) continue;
                foreach (var propertyInfo in o.GetType().GetFields(BindingFlags.Instance | BindingFlags.Public))
                {
                    if (propertyInfo.FieldType == types[i])
                    {
                        //if (propertyInfo.Name.ToLower() == strings[i].ToLower())
                        {
                            var val = propertyInfo.GetValue(o);
                            if (val != null) parameters[i] = val;

                        }
                    }
                }
            }
            for (int i = 0; i < strings.Length; i++)
            {
                if (parameters[i] != null) continue;
                foreach (var propertyInfo in o.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public))
                {
                    if (SkipCall(propertyInfo)) continue;
                    object val = propertyInfo.GetValue(o, null);
                    if (val == null) continue;
                    fillInParams(val, types, strings, parameters, d);
                }
            }
            for (int i = 0; i < strings.Length; i++)
            {
                if (parameters[i] != null) continue;
                foreach (var propertyInfo in o.GetType().GetFields(BindingFlags.Instance | BindingFlags.Public))
                {
                    object val = propertyInfo.GetValue(o);
                    if (val == null) continue;
                    fillInParams(val, types, strings, parameters, d);
                }
            }
        }

        private bool SkipCall(PropertyInfo info)
        {
            if (!info.CanRead || info.GetIndexParameters().Length != 0) return true;
            string infoNameToLower = info.Name.ToLower();
            if (infoNameToLower.Contains("scope") || infoNameToLower.Contains("worldtime"))
            {
                return true;
            }
            return false;


        }

        static public readonly string[] paramNamesOnLogin = new string[] { "login", "message" };
        static public readonly Type[] paramTypesOnLogin = new Type[] { typeof(LoginStatus), typeof(string) };

       public virtual void Network_OnLogin(object sender, LoginProgressEventArgs e) { OnEvent("On-Login", paramNamesOnLogin, paramTypesOnLogin, e.Status, e.Message); }

        static public readonly string[] paramNamesOnConnected = new string[] { "sender" };
        static public readonly Type[] paramTypesOnConnected = new Type[] { typeof(System.Object) };

        public virtual void Network_OnConnected(System.Object sender) { OnEvent("On-Connected", paramNamesOnConnected, paramTypesOnConnected, sender); }

        static public readonly string[] paramNamesOnLogoutReply = new string[] { "inventoryItems" };
        static public readonly Type[] paramTypesOnLogoutReply = new Type[] { typeof(List<UUID>) };

        public virtual void Network_OnLogoutReply(object sender, LoggedOutEventArgs e) { OnEvent("On-Logout-Reply", paramNamesOnLogoutReply, paramTypesOnLogoutReply, e); }

        static public readonly string[] paramNamesOnSimConnecting = new string[] { "simulator" };
        static public readonly Type[] paramTypesOnSimConnecting = new Type[] { typeof(Simulator) };

        public virtual void Network_OnSimConnecting(object sender, SimConnectingEventArgs e)
        { e.Cancel = !OnEvent("On-Sim-Connecting", paramNamesOnSimConnecting, paramTypesOnSimConnecting, e); }

        static public readonly string[] paramNamesOnSimConnected = new string[] { "simulator" };
        static public readonly Type[] paramTypesOnSimConnected = new Type[] { typeof(Simulator) };

        public virtual void Network_OnSimConnected(object sender, SimConnectedEventArgs e) { OnEvent("On-Sim-Connected", paramNamesOnSimConnected, paramTypesOnSimConnected, e); }

        static public readonly string[] paramNamesOnSimDisconnected = new string[] { "simulator", "reason" };
        static public readonly Type[] paramTypesOnSimDisconnected = new Type[] { typeof(Simulator), typeof(NetworkManager.DisconnectType) };

        public virtual void Network_OnSimDisconnected(object sender, SimDisconnectedEventArgs e) { OnEvent("On-Sim-Disconnected", paramNamesOnSimDisconnected, paramTypesOnSimDisconnected, e); }

        static public readonly string[] paramNamesOnDisconnected = new string[] { "reason", "message" };
        static public readonly Type[] paramTypesOnDisconnected = new Type[] { typeof(NetworkManager.DisconnectType), typeof(string) };

        public virtual void Network_OnDisconnected(object sender, DisconnectedEventArgs e) { OnEvent("On-Disconnected", paramNamesOnDisconnected, paramTypesOnDisconnected,e); }

        static public readonly string[] paramNamesOnCurrentSimChanged = new string[] { "PreviousSimulator" };
        static public readonly Type[] paramTypesOnCurrentSimChanged = new Type[] { typeof(Simulator) };

        public virtual void Network_OnCurrentSimChanged(object sender, SimChangedEventArgs e) { OnEvent("On-Current-Sim-Changed", paramNamesOnCurrentSimChanged, paramTypesOnCurrentSimChanged, e); }

        static public readonly string[] paramNamesOnEventQueueRunning = new string[] { "simulator" };
        static public readonly Type[] paramTypesOnEventQueueRunning = new Type[] { typeof(Simulator) };

        public virtual void Network_OnEventQueueRunning(object sender, EventQueueRunningEventArgs e) { OnEvent("On-Event-Queue-Running", paramNamesOnEventQueueRunning, paramTypesOnEventQueueRunning, e); }

        static public readonly string[] paramNamesOnParcelDwell = new string[] { "parcelID", "localID", "dwell" };
        static public readonly Type[] paramTypesOnParcelDwell = new Type[] { typeof(UUID), typeof(int), typeof(float) };

        public virtual void Parcels_OnParcelDwell(object sender, ParcelDwellReplyEventArgs e) { OnEvent("On-Parcel-Dwell", paramNamesOnParcelDwell, paramTypesOnParcelDwell, e); }

        static public readonly string[] paramNamesOnParcelInfo = new string[] { "parcel" };
        static public readonly Type[] paramTypesOnParcelInfo = new Type[] { typeof(ParcelInfo) };

        public virtual void Parcels_OnParcelInfo(object sender, ParcelInfoReplyEventArgs e) { OnEvent("On-Parcel-Info", paramNamesOnParcelInfo, paramTypesOnParcelInfo, e); }

        static public readonly string[] paramNamesOnParcelProperties = new string[] { "simulator", "parcel", "result", "selectedPrims", "sequenceID", "snapSelection" };
        static public readonly Type[] paramTypesOnParcelProperties = new Type[] { typeof(Simulator), typeof(Parcel), typeof(ParcelResult), typeof(int), typeof(int), typeof(bool) };

        public virtual void Parcels_OnParcelProperties(object sender, ParcelPropertiesEventArgs e) { OnEvent("On-Parcel-Properties", paramNamesOnParcelProperties, paramTypesOnParcelProperties,e); }

        static public readonly string[] paramNamesOnAccessListReply = new string[] { "simulator", "sequenceID", "localID", "flags", "accessEntries" };
        static public readonly Type[] paramTypesOnAccessListReply = new Type[] { typeof(Simulator), typeof(int), typeof(int), typeof(uint), typeof(List<ParcelManager.ParcelAccessEntry>) };

        public virtual void Parcels_OnAccessListReply(object sender, ParcelAccessListReplyEventArgs e) { OnEvent("On-Access-List-Reply", paramNamesOnAccessListReply, paramTypesOnAccessListReply, e); }

        static public readonly string[] paramNamesOnPrimOwnersListReply = new string[] { "simulator", "primOwners" };
        static public readonly Type[] paramTypesOnPrimOwnersListReply = new Type[] { typeof(Simulator), typeof(List<ParcelManager.ParcelPrimOwners>) };

        public virtual void Parcels_OnPrimOwnersListReply(object sender, ParcelObjectOwnersReplyEventArgs e) { OnEvent("On-Prim-Owners-List-Reply", paramNamesOnPrimOwnersListReply, paramTypesOnPrimOwnersListReply,e); }

        static public readonly string[] paramNamesOnSimParcelsDownloaded = new string[] { "simulator", "simParcels", "parcelMap" };
        static public readonly Type[] paramTypesOnSimParcelsDownloaded = new Type[] { typeof(Simulator), typeof(InternalDictionary<int, Parcel>), typeof(int[,]) };

        public virtual void Parcels_OnSimParcelsDownloaded(object sender, SimParcelsDownloadedEventArgs e) { OnEvent("On-Sim-Parcels-Downloaded", paramNamesOnSimParcelsDownloaded, paramTypesOnSimParcelsDownloaded, e); }

        static public readonly string[] paramNamesOnParcelSelectedObjects = new string[] { "simulator", "objectIDs", "resetList" };
        static public readonly Type[] paramTypesOnParcelSelectedObjects = new Type[] { typeof(Simulator), typeof(List<uint>), typeof(bool) };

        public virtual void Parcels_OnParcelSelectedObjects(object sender, ForceSelectObjectsReplyEventArgs e) { OnEvent("On-Parcel-Selected-Objects", paramNamesOnParcelSelectedObjects, paramTypesOnParcelSelectedObjects, e); }

        static public readonly string[] paramNamesOnParcelMediaUpdate = new string[] { "simulator", "media" };
        static public readonly Type[] paramTypesOnParcelMediaUpdate = new Type[] { typeof(Simulator), typeof(ParcelMedia) };

        public virtual void Parcels_OnParcelMediaUpdate(object sender, ParcelMediaUpdateReplyEventArgs e) { OnEvent("On-Parcel-Media-Update", paramNamesOnParcelMediaUpdate, paramTypesOnParcelMediaUpdate, e); }

        static public readonly string[] paramNamesOnChat = new string[] { "message", "audible", "type", "sourceType", "fromName", "id", "ownerid", "position" };
        static public readonly Type[] paramTypesOnChat = new Type[] { typeof(string), typeof(ChatAudibleLevel), typeof(ChatType), typeof(ChatSourceType), typeof(string), typeof(UUID), typeof(UUID), typeof(Vector3) };

        public virtual void Self_OnChat(object sender, ChatEventArgs e) {OnEvent("On-Chat", paramNamesOnChat, paramTypesOnChat, e);}

        static public readonly string[] paramNamesOnScriptDialog = new string[] { "message", "objectName", "imageID", "objectID", "firstName", "lastName", "chatChannel", "buttons" };
        static public readonly Type[] paramTypesOnScriptDialog = new Type[] { typeof(string), typeof(string), typeof(UUID), typeof(UUID), typeof(string), typeof(string), typeof(int), typeof(List<string>) };

        public virtual void Self_OnScriptDialog(object sender, ScriptDialogEventArgs e) { OnEvent("On-Script-Dialog", paramNamesOnScriptDialog, paramTypesOnScriptDialog, e); }

        static public readonly string[] paramNamesOnScriptQuestion = new string[] { "simulator", "taskID", "itemID", "objectName", "objectOwner", "questions" };
        static public readonly Type[] paramTypesOnScriptQuestion = new Type[] { typeof(Simulator), typeof(UUID), typeof(UUID), typeof(string), typeof(string), typeof(ScriptPermission) };

        public virtual void Self_OnScriptQuestion(object sender, ScriptQuestionEventArgs e) { OnEvent("On-Script-Question", paramNamesOnScriptQuestion, paramTypesOnScriptQuestion,e); }

        static public readonly string[] paramNamesOnLoadURL = new string[] { "objectName", "objectID", "ownerID", "ownerIsGroup", "message", "URL" };
        static public readonly Type[] paramTypesOnLoadURL = new Type[] { typeof(string), typeof(UUID), typeof(UUID), typeof(bool), typeof(string), typeof(string) };

        public virtual void Self_OnLoadURL(object sender, LoadUrlEventArgs e) { OnEvent("On-Load-U-R-L", paramNamesOnLoadURL, paramTypesOnLoadURL, e); }

        static public readonly string[] paramNamesOnInstantMessage = new string[] { "im", "simulator" };
        static public readonly Type[] paramTypesOnInstantMessage = new Type[] { typeof(InstantMessage), typeof(Simulator) };

        public virtual void Self_OnInstantMessage(object sender, InstantMessageEventArgs e) { OnEvent("On-Instant-Message", paramNamesOnInstantMessage, paramTypesOnInstantMessage, e); }

        static public readonly string[] paramNamesOnTeleport = new string[] { "message", "status", "flags" };
        static public readonly Type[] paramTypesOnTeleport = new Type[] { typeof(string), typeof(TeleportStatus), typeof(TeleportFlags) };

        public virtual void Self_OnTeleport(object sender, TeleportEventArgs e) { OnEvent("On-Teleport", paramNamesOnTeleport, paramTypesOnTeleport,e); }

        static public readonly string[] paramNamesOnBalanceUpdated = new string[] { "balance" };
        static public readonly Type[] paramTypesOnBalanceUpdated = new Type[] { typeof(int) };

        public virtual void Self_OnBalanceUpdated(object sender, BalanceEventArgs e) 
        {
            OnEvent("On-Balance-Updated", paramNamesOnBalanceUpdated, paramTypesOnBalanceUpdated, e); 
        }

        static public readonly string[] paramNamesOnMoneyBalanceReplyReceived = new string[] { "transactionID", "transactionSuccess", "balance", "metersCredit", "metersCommitted", "description" };
        static public readonly Type[] paramTypesOnMoneyBalanceReplyReceived = new Type[] { typeof(UUID), typeof(bool), typeof(int), typeof(int), typeof(int), typeof(string) };

        public virtual void Self_OnMoneyBalanceReplyReceived(object sender, MoneyBalanceReplyEventArgs e) { OnEvent("On-Money-Balance-Reply-Received", paramNamesOnMoneyBalanceReplyReceived, paramTypesOnMoneyBalanceReplyReceived, e); }

        static public readonly string[] paramNamesOnAgentDataUpdated = new string[] { "firstName", "lastName", "activeGroupID", "groupTitle", "groupPowers", "groupName" };
        static public readonly Type[] paramTypesOnAgentDataUpdated = new Type[] { typeof(string), typeof(string), typeof(UUID), typeof(string), typeof(GroupPowers), typeof(string) };

        public virtual void Self_OnAgentDataUpdated(object sender, AgentDataReplyEventArgs e) { OnEvent("On-Agent-Data-Updated", paramNamesOnAgentDataUpdated, paramTypesOnAgentDataUpdated, e); }

        static public readonly string[] paramNamesOnAnimationsChanged = new string[] { "agentAnimations" };
        static public readonly Type[] paramTypesOnAnimationsChanged = new Type[] { typeof(InternalDictionary<UUID, int>) };

        public virtual void Self_OnAnimationsChanged(object sender, AnimationsChangedEventArgs e) { OnEvent("On-Animations-Changed", paramNamesOnAnimationsChanged, paramTypesOnAnimationsChanged, e); }

        static public readonly string[] paramNamesOnMeanCollision = new string[] { "type", "perp", "victim", "magnitude", "time" };
        static public readonly Type[] paramTypesOnMeanCollision = new Type[] { typeof(MeanCollisionType), typeof(UUID), typeof(UUID), typeof(float), typeof(System.DateTime) };

        public virtual void Self_OnMeanCollision(object sender, MeanCollisionEventArgs e) { OnEvent("On-Mean-Collision", paramNamesOnMeanCollision, paramTypesOnMeanCollision, e); }

        static public readonly string[] paramNamesOnRegionCrossed = new string[] { "oldSim", "newSim" };
        static public readonly Type[] paramTypesOnRegionCrossed = new Type[] { typeof(Simulator), typeof(Simulator) };

        public virtual void Self_OnRegionCrossed(object sender, RegionCrossedEventArgs e) { OnEvent("On-Region-Crossed", paramNamesOnRegionCrossed, paramTypesOnRegionCrossed, e); }

        static public readonly string[] paramNamesOnGroupChatJoin = new string[] { "groupChatSessionID", "sessionName", "tmpSessionID", "success" };
        static public readonly Type[] paramTypesOnGroupChatJoin = new Type[] { typeof(UUID), typeof(string), typeof(UUID), typeof(bool) };

        public virtual void Self_OnGroupChatJoin(object sender, GroupChatJoinedEventArgs e) { OnEvent("On-Group-Chat-Join", paramNamesOnGroupChatJoin, paramTypesOnGroupChatJoin,e); }

        static public readonly string[] paramNamesOnGroupChatLeft = new string[] { "groupchatSessionID" };
        static public readonly Type[] paramTypesOnGroupChatLeft = new Type[] { typeof(UUID) };

        public virtual void Self_OnGroupChatLeft(object sender, GroupChatLeftEventArgs e) { OnEvent("On-Group-Chat-Left", paramNamesOnGroupChatLeft, paramTypesOnGroupChatLeft, e); }

        static public readonly string[] paramNamesOnAlertMessage = new string[] { "message" };
        static public readonly Type[] paramTypesOnAlertMessage = new Type[] { typeof(string) };

        public virtual void Self_OnAlertMessage(object sender, AlertMessageEventArgs e) { OnEvent("On-Alert-Message", paramNamesOnAlertMessage, paramTypesOnAlertMessage, e); }

        static public readonly string[] paramNamesOnScriptControlChange = new string[] { "controls", "pass", "take" };
        static public readonly Type[] paramTypesOnScriptControlChange = new Type[] { typeof(ScriptControlChange), typeof(bool), typeof(bool) };

        public virtual void Self_OnScriptControlChange(object sender, ScriptControlEventArgs e) { OnEvent("On-Script-Control-Change", paramNamesOnScriptControlChange, paramTypesOnScriptControlChange, e); }

        static public readonly string[] paramNamesOnCameraConstraint = new string[] { "collidePlane" };
        static public readonly Type[] paramTypesOnCameraConstraint = new Type[] { typeof(Vector4) };

        public virtual void Self_OnCameraConstraint(object sender, CameraConstraintEventArgs e)
        {
            OnEvent("On-Camera-Constraint", paramNamesOnCameraConstraint, paramTypesOnCameraConstraint, e);
        }

        static public readonly string[] paramNamesOnScriptSensorReply = new string[] { "requestorID", "groupID", "name", "objectID", "ownerID", "position", "range", "rotation", "type", "velocity" };
        static public readonly Type[] paramTypesOnScriptSensorReply = new Type[] { typeof(UUID), typeof(UUID), typeof(string), typeof(UUID), typeof(UUID), typeof(Vector3), typeof(float), typeof(Quaternion), typeof(ScriptSensorTypeFlags), typeof(Vector3) };

        public virtual void Self_OnScriptSensorReply(object sender, ScriptSensorReplyEventArgs e) { OnEvent("On-Script-Sensor-Reply", paramNamesOnScriptSensorReply, paramTypesOnScriptSensorReply, e); }

        static public readonly string[] paramNamesOnAvatarSitResponse = new string[] { "objectID", "autoPilot", "cameraAtOffset", "cameraEyeOffset", "forceMouselook", "sitPosition", "sitRotation" };
        static public readonly Type[] paramTypesOnAvatarSitResponse = new Type[] { typeof(UUID), typeof(bool), typeof(Vector3), typeof(Vector3), typeof(bool), typeof(Vector3), typeof(Quaternion) };

        public virtual void Self_OnAvatarSitResponse(object sender, AvatarSitResponseEventArgs e) { OnEvent("On-Avatar-Sit-Response", paramNamesOnAvatarSitResponse, paramTypesOnAvatarSitResponse, e); }

        static public readonly string[] paramNamesOnChatSessionMemberAdded = new string[] { "sessionID", "agent_key" };
        static public readonly Type[] paramTypesOnChatSessionMemberAdded = new Type[] { typeof(UUID), typeof(UUID) };

        public virtual void Self_OnChatSessionMemberAdded(object sender, ChatSessionMemberAddedEventArgs e) { OnEvent("On-Chat-Session-Member-Added", paramNamesOnChatSessionMemberAdded, paramTypesOnChatSessionMemberAdded, e); }

        static public readonly string[] paramNamesOnChatSessionMemberLeft = new string[] { "sessionID", "agent_key" };
        static public readonly Type[] paramTypesOnChatSessionMemberLeft = new Type[] { typeof(UUID), typeof(UUID) };

        public virtual void Self_OnChatSessionMemberLeft(object sender, ChatSessionMemberLeftEventArgs e) { OnEvent("On-Chat-Session-Member-Left", paramNamesOnChatSessionMemberLeft, paramTypesOnChatSessionMemberLeft, e); }

        static public readonly string[] paramNamesOnAvatarAppearance = new string[] { "avatarID", "isTrial", "defaultTexture", "faceTextures", "visualParams" };
        static public readonly Type[] paramTypesOnAvatarAppearance = new Type[] { typeof(UUID), typeof(bool), typeof(Primitive.TextureEntryFace), typeof(Primitive.TextureEntryFace[]), typeof(List<byte>) };

        public virtual void Avatars_OnAvatarAppearance(object sender, AvatarAppearanceEventArgs e) { OnEvent("On-Avatar-Appearance", paramNamesOnAvatarAppearance, paramTypesOnAvatarAppearance, e); }

        static public readonly string[] paramNamesOnAvatarNames = new string[] { "names" };
        static public readonly Type[] paramTypesOnAvatarNames = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarNames(object sender, UUIDNameReplyEventArgs e) { OnEvent("On-Avatar-Names", paramNamesOnAvatarNames, paramTypesOnAvatarNames, e); }

        static public readonly string[] paramNamesOnAvatarInterests = new string[] { "avatarID", "interests" };
        static public readonly Type[] paramTypesOnAvatarInterests = new Type[] { typeof(UUID), typeof(Avatar.Interests) };

        public virtual void Avatars_OnAvatarInterests(object sender, AvatarInterestsReplyEventArgs e) { OnEvent("On-Avatar-Interests", paramNamesOnAvatarInterests, paramTypesOnAvatarInterests, e); }

        static public readonly string[] paramNamesOnAvatarProperties = new string[] { "avatarID", "properties" };
        static public readonly Type[] paramTypesOnAvatarProperties = new Type[] { typeof(UUID), typeof(Avatar.AvatarProperties) };

        public virtual void Avatars_OnAvatarProperties(object sender, AvatarPropertiesReplyEventArgs e) { OnEvent("On-Avatar-Properties", paramNamesOnAvatarProperties, paramTypesOnAvatarProperties, e); }

        static public readonly string[] paramNamesOnAvatarGroups = new string[] { "avatarID", "avatarGroups" };
        static public readonly Type[] paramTypesOnAvatarGroups = new Type[] { typeof(UUID), typeof(List<AvatarGroup>) };

        public virtual void Avatars_OnAvatarGroups(object sender, AvatarGroupsReplyEventArgs e) { OnEvent("On-Avatar-Groups", paramNamesOnAvatarGroups, paramTypesOnAvatarGroups, e); }

        static public readonly string[] paramNamesOnAvatarNameSearch = new string[] { "queryID", "avatars" };
        static public readonly Type[] paramTypesOnAvatarNameSearch = new Type[] { typeof(UUID), typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarNameSearch(object sender, AvatarPickerReplyEventArgs e) { OnEvent("On-Avatar-Name-Search", paramNamesOnAvatarNameSearch, paramTypesOnAvatarNameSearch, e); }

        static public readonly string[] paramNamesOnPointAt = new string[] { "sourceID", "targetID", "targetPos", "pointType", "duration", "id" };
        static public readonly Type[] paramTypesOnPointAt = new Type[] { typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(PointAtType), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnPointAt(object sender, ViewerEffectPointAtEventArgs e) { OnEvent("On-Point-At", paramNamesOnPointAt, paramTypesOnPointAt,e); }

        static public readonly string[] paramNamesOnLookAt = new string[] { "sourceID", "targetID", "targetPos", "lookType", "duration", "id" };
        static public readonly Type[] paramTypesOnLookAt = new Type[] { typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(LookAtType), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnLookAt(object sender, ViewerEffectLookAtEventArgs e) { OnEvent("On-Look-At", paramNamesOnLookAt, paramTypesOnLookAt, e); }

        static public readonly string[] paramNamesOnEffect = new string[] { "type", "sourceID", "targetID", "targetPos", "duration", "id" };
        static public readonly Type[] paramTypesOnEffect = new Type[] { typeof(EffectType), typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnEffect(object sender, ViewerEffectEventArgs e) { OnEvent("On-Effect", paramNamesOnEffect, paramTypesOnEffect, e); }

        static public readonly string[] paramNamesOnAvatarPicks = new string[] { "avatarid", "picks" };
        static public readonly Type[] paramTypesOnAvatarPicks = new Type[] { typeof(UUID), typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarPicks(object sender, AvatarPicksReplyEventArgs e) { OnEvent("On-Avatar-Picks", paramNamesOnAvatarPicks, paramTypesOnAvatarPicks, e); }

        static public readonly string[] paramNamesOnPickInfo = new string[] { "pickid", "pick" };
        static public readonly Type[] paramTypesOnPickInfo = new Type[] { typeof(UUID), typeof(ProfilePick) };

        public virtual void Avatars_OnPickInfo(object sender, PickInfoReplyEventArgs e) { OnEvent("On-Pick-Info", paramNamesOnPickInfo, paramTypesOnPickInfo, e); }

        static public readonly string[] paramNamesOnFriendNamesReceived = new string[] { "names" };
        static public readonly Type[] paramTypesOnFriendNamesReceived = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Friends_OnFriendNamesReceived(object sender, FriendNamesEventArgs e) { OnEvent("On-Friend-Names-Received", paramNamesOnFriendNamesReceived, paramTypesOnFriendNamesReceived, e); }

        static public readonly string[] paramNamesOnFriendOnline = new string[] { "friend" };
        static public readonly Type[] paramTypesOnFriendOnline = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendOnline(object sender, FriendInfoEventArgs e) { OnEvent("On-Friend-Online", paramNamesOnFriendOnline, paramTypesOnFriendOnline, e); }

        static public readonly string[] paramNamesOnFriendOffline = new string[] { "friend" };
        static public readonly Type[] paramTypesOnFriendOffline = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendOffline(object sender, FriendInfoEventArgs e) { OnEvent("On-Friend-Offline", paramNamesOnFriendOffline, paramTypesOnFriendOffline, e); }

        static public readonly string[] paramNamesOnFriendRights = new string[] { "friend" };
        static public readonly Type[] paramTypesOnFriendRights = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendRights(object sender, FriendInfoEventArgs e) { OnEvent("On-Friend-Rights", paramNamesOnFriendRights, paramTypesOnFriendRights, e); }

        static public readonly string[] paramNamesOnFriendshipOffered = new string[] { "agentID", "agentName", "imSessionID" };
        static public readonly Type[] paramTypesOnFriendshipOffered = new Type[] { typeof(UUID), typeof(string), typeof(UUID) };

        public virtual void Friends_OnFriendshipOffered(object sender, FriendshipOfferedEventArgs e) { OnEvent("On-Friendship-Offered", paramNamesOnFriendshipOffered, paramTypesOnFriendshipOffered, e); }

        static public readonly string[] paramNamesOnFriendshipResponse = new string[] { "agentID", "agentName", "accepted" };
        static public readonly Type[] paramTypesOnFriendshipResponse = new Type[] { typeof(UUID), typeof(string), typeof(bool) };

        public virtual void Friends_OnFriendshipResponse(object sender, FriendshipResponseEventArgs e) { OnEvent("On-Friendship-Response", paramNamesOnFriendshipResponse, paramTypesOnFriendshipResponse, e); }

        static public readonly string[] paramNamesOnFriendshipTerminated = new string[] { "agentID", "agentName" };
        static public readonly Type[] paramTypesOnFriendshipTerminated = new Type[] { typeof(UUID), typeof(string) };

        public virtual void Friends_OnFriendshipTerminated(object sender, FriendshipTerminatedEventArgs e) { OnEvent("On-Friendship-Terminated", paramNamesOnFriendshipTerminated, paramTypesOnFriendshipTerminated, e); }

        static public readonly string[] paramNamesOnFriendFound = new string[] { "agentID", "regionHandle", "location" };
        static public readonly Type[] paramTypesOnFriendFound = new Type[] { typeof(UUID), typeof(ulong), typeof(Vector3) };

        public virtual void Friends_OnFriendFound(object sender, FriendFoundReplyEventArgs e) { OnEvent("On-Friend-Found", paramNamesOnFriendFound, paramTypesOnFriendFound,e); }

        static public readonly string[] paramNamesOnCoarseLocationUpdate = new string[] { "simulator", "newEntries", "removedEntries" };
        static public readonly Type[] paramTypesOnCoarseLocationUpdate = new Type[] { typeof(Simulator), typeof(List<UUID>), typeof(List<UUID>) };

        public virtual void Grid_OnCoarseLocationUpdate(object sender, CoarseLocationUpdateEventArgs e) { OnEvent("On-Coarse-Location-Update", paramNamesOnCoarseLocationUpdate, paramTypesOnCoarseLocationUpdate, e); }

        static public readonly string[] paramNamesOnGridRegion = new string[] { "region" };
        static public readonly Type[] paramTypesOnGridRegion = new Type[] { typeof(GridRegion) };

        public virtual void Grid_OnGridRegion(object sender, GridRegionEventArgs e) { OnEvent("On-Grid-Region", paramNamesOnGridRegion, paramTypesOnGridRegion, e); }

        static public readonly string[] paramNamesOnGridLayer = new string[] { "layer" };
        static public readonly Type[] paramTypesOnGridLayer = new Type[] { typeof(GridLayer) };

        public virtual void Grid_OnGridLayer(object sender, GridLayerEventArgs e) { OnEvent("On-Grid-Layer", paramNamesOnGridLayer, paramTypesOnGridLayer, e); }

        static public readonly string[] paramNamesOnGridItems = new string[] { "type", "items" };
        static public readonly Type[] paramTypesOnGridItems = new Type[] { typeof(GridItemType), typeof(List<MapItem>) };

        public virtual void Grid_OnGridItems(object sender, GridItemsEventArgs e) { OnEvent("On-Grid-Items", paramNamesOnGridItems, paramTypesOnGridItems, e.Type, e.Items); }

        static public readonly string[] paramNamesOnRegionHandleReply = new string[] { "regionID", "regionHandle" };
        static public readonly Type[] paramTypesOnRegionHandleReply = new Type[] { typeof(UUID), typeof(ulong) };

        public virtual void Grid_OnRegionHandleReply(object sender, RegionHandleReplyEventArgs e) { OnEvent("On-Region-Handle-Reply", paramNamesOnRegionHandleReply, paramTypesOnRegionHandleReply, e); }

        static public readonly string[] paramNamesOnNewPrim = new string[] { "simulator", "prim", "regionHandle", "timeDilation" };
        static public readonly Type[] paramTypesOnNewPrim = new Type[] { typeof(Simulator), typeof(Primitive), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewPrim(object sender, PrimEventArgs e) { OnEvent("On-New-Prim", paramNamesOnNewPrim, paramTypesOnNewPrim, e); }

        static public readonly string[] paramNamesOnNewAttachment = new string[] { "simulator", "prim", "regionHandle", "timeDilation" };
        static public readonly Type[] paramTypesOnNewAttachment = new Type[] { typeof(Simulator), typeof(Primitive), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewAttachment(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation) { OnEvent("On-New-Attachment", paramNamesOnNewAttachment, paramTypesOnNewAttachment, simulator, prim, regionHandle, timeDilation); }

        static public readonly string[] paramNamesOnNewAvatar = new string[] { "simulator", "avatar", "regionHandle", "timeDilation" };
        static public readonly Type[] paramTypesOnNewAvatar = new Type[] { typeof(Simulator), typeof(Avatar), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewAvatar(object sender, AvatarUpdateEventArgs e) { OnEvent("On-New-Avatar", paramNamesOnNewAvatar, paramTypesOnNewAvatar, e); }

        static public readonly string[] paramNamesOnObjectUpdated = new string[] { "simulator", "update", "regionHandle", "timeDilation" };
        static public readonly Type[] paramTypesOnObjectUpdated = new Type[] { typeof(Simulator), typeof(ObjectMovementUpdate), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnObjectUpdated(object sender, TerseObjectUpdateEventArgs e) { OnEvent("On-Object-Updated", paramNamesOnObjectUpdated, paramTypesOnObjectUpdated, e); }

        static public readonly string[] paramNamesOnAvatarSitChanged = new string[] { "simulator", "avatar", "sittingOn", "oldSeat" };
        static public readonly Type[] paramTypesOnAvatarSitChanged = new Type[] { typeof(Simulator), typeof(Avatar), typeof(uint), typeof(uint) };

        public virtual void Objects_OnAvatarSitChanged(object sender, AvatarSitChangedEventArgs e) { OnEvent("On-Avatar-Sit-Changed", paramNamesOnAvatarSitChanged, paramTypesOnAvatarSitChanged, e); }

        static public readonly string[] paramNamesOnObjectKilled = new string[] { "simulator", "objectID" };
        static public readonly Type[] paramTypesOnObjectKilled = new Type[] { typeof(Simulator), typeof(uint) };

        public virtual void Objects_OnObjectKilled(object sender, KillObjectEventArgs e) { OnEvent("On-Object-Killed", paramNamesOnObjectKilled, paramTypesOnObjectKilled, e); }

        static public readonly string[] paramNamesOnObjectProperties = new string[] { "simulator", "props" };
        static public readonly Type[] paramTypesOnObjectProperties = new Type[] { typeof(Simulator), typeof(Primitive.ObjectProperties) };

        public virtual void Objects_OnObjectProperties(object sender, ObjectPropertiesEventArgs e) { OnEvent("On-Object-Properties", paramNamesOnObjectProperties, paramTypesOnObjectProperties, e); }

        static public readonly string[] paramNamesOnObjectPropertiesFamily = new string[] { "simulator", "props", "type" };
        static public readonly Type[] paramTypesOnObjectPropertiesFamily = new Type[] { typeof(Simulator), typeof(Primitive.ObjectProperties), typeof(ReportType) };

        public virtual void Objects_OnObjectPropertiesFamily(object sender, ObjectPropertiesFamilyEventArgs e) { OnEvent("On-Object-Properties-Family", paramNamesOnObjectPropertiesFamily, paramTypesOnObjectPropertiesFamily, e); }

        static public readonly string[] paramNamesOnCurrentGroups = new string[] { "groups" };
        static public readonly Type[] paramTypesOnCurrentGroups = new Type[] { typeof(Dictionary<UUID, Group>) };

        public virtual void Groups_OnCurrentGroups(object sender, CurrentGroupsEventArgs e) { OnEvent("On-Current-Groups", paramNamesOnCurrentGroups, paramTypesOnCurrentGroups, e); }

        static public readonly string[] paramNamesOnGroupNames = new string[] { "groupNames" };
        static public readonly Type[] paramTypesOnGroupNames = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Groups_OnGroupNames(object sender, GroupNamesEventArgs e) { OnEvent("On-Group-Names", paramNamesOnGroupNames, paramTypesOnGroupNames, e); }

        static public readonly string[] paramNamesOnGroupProfile = new string[] { "group" };
        static public readonly Type[] paramTypesOnGroupProfile = new Type[] { typeof(Group) };

        public virtual void Groups_OnGroupProfile(object sender, GroupProfileEventArgs e) { OnEvent("On-Group-Profile", paramNamesOnGroupProfile, paramTypesOnGroupProfile, e); }

        public static readonly string[] paramNamesOnGroupMembers = new string[] { "requestID", "groupID", "members" };
        static public readonly Type[] paramTypesOnGroupMembers = new Type[] { typeof(UUID), typeof(UUID), typeof(Dictionary<UUID, GroupMember>) };

        public virtual void Groups_OnGroupMembers(object sender, GroupMembersReplyEventArgs e)
        {
            OnEvent("On-Group-Members", paramNamesOnGroupMembers, paramTypesOnGroupMembers, 
                e);
        }

        static public readonly string[] paramNamesOnGroupRoles = new string[] {"requestID", "groupID", "roles" };
        static public readonly Type[] paramTypesOnGroupRoles = new Type[] { typeof(UUID), typeof(UUID), typeof(Dictionary<UUID, GroupRole>) };

        public virtual void Groups_OnGroupRoles(object sender, GroupRolesDataReplyEventArgs e) { OnEvent("On-Group-Roles", paramNamesOnGroupRoles, paramTypesOnGroupRoles, e); }

        static public readonly string[] paramNamesOnGroupRolesMembers = new string[] { "requestID", "groupID", "rolesMembers" };
        static public readonly Type[] paramTypesOnGroupRolesMembers = new Type[] { typeof(UUID), typeof(UUID),  typeof(List<KeyValuePair<UUID, UUID>>) };

        public virtual void Groups_OnGroupRolesMembers(object sender, GroupRolesMembersReplyEventArgs e) {OnEvent("On-Group-Roles-Members", paramNamesOnGroupRolesMembers, paramTypesOnGroupRolesMembers, e); }

        static public readonly string[] paramNamesOnGroupTitles = new string[] { "requestID", "groupID", "titles" };
        static public readonly Type[] paramTypesOnGroupTitles = new Type[] { typeof(UUID), typeof(UUID), typeof(Dictionary<UUID, GroupTitle>) };

        public virtual void Groups_OnGroupTitles(object sender, GroupTitlesReplyEventArgs e) { OnEvent("On-Group-Titles", paramNamesOnGroupTitles, paramTypesOnGroupTitles, e); }

        static public readonly string[] paramNamesOnGroupAccountSummary = new string[] { "groupID", "summary" };
        static public readonly Type[] paramTypesOnGroupAccountSummary = new Type[] { typeof(UUID), typeof(GroupAccountSummary) };

        public virtual void Groups_OnGroupAccountSummary(object sender, GroupAccountSummaryReplyEventArgs e) { OnEvent("On-Group-Account-Summary", paramNamesOnGroupAccountSummary, paramTypesOnGroupAccountSummary, e); }

        static public readonly string[] paramNamesOnGroupCreated = new string[] { "groupID", "success", "message" };
        static public readonly Type[] paramTypesOnGroupCreated = new Type[] { typeof(UUID), typeof(bool), typeof(string) };

        public virtual void Groups_OnGroupCreated(object sender, GroupCreatedReplyEventArgs e) { OnEvent("On-Group-Created", paramNamesOnGroupCreated, paramTypesOnGroupCreated, e); }

        static public readonly string[] paramNamesOnGroupJoined = new string[] { "groupID", "success" };
        static public readonly Type[] paramTypesOnGroupJoined = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupJoined(object sender, GroupOperationEventArgs e) { OnEvent("On-Group-Joined", paramNamesOnGroupJoined, paramTypesOnGroupJoined, e); }

        static public readonly string[] paramNamesOnGroupLeft = new string[] { "groupID", "success" };
        static public readonly Type[] paramTypesOnGroupLeft = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupLeft(object sender, GroupOperationEventArgs e) { OnEvent("On-Group-Left", paramNamesOnGroupLeft, paramTypesOnGroupLeft, e); }

        static public readonly string[] paramNamesOnGroupDropped = new string[] { "groupID" };
        static public readonly Type[] paramTypesOnGroupDropped = new Type[] { typeof(UUID) };

        public virtual void Groups_OnGroupDropped(object sender, GroupDroppedEventArgs e) { OnEvent("On-Group-Dropped", paramNamesOnGroupDropped, paramTypesOnGroupDropped, e); }

        static public readonly string[] paramNamesOnGroupMemberEjected = new string[] { "groupID", "success" };
        static public readonly Type[] paramTypesOnGroupMemberEjected = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupMemberEjected(object sender, GroupOperationEventArgs e) { OnEvent("On-Group-Member-Ejected", paramNamesOnGroupMemberEjected, paramTypesOnGroupMemberEjected, e); }

        static public readonly string[] paramNamesOnGroupNoticesList = new string[] { "groupID", "notice" };
        static public readonly Type[] paramTypesOnGroupNoticesList = new Type[] { typeof(UUID), typeof(List<GroupNoticesListEntry>) };

        public virtual void Groups_OnGroupNoticesList(object sender, GroupNoticesListReplyEventArgs e) { OnEvent("On-Group-Notices-List", paramNamesOnGroupNoticesList, paramTypesOnGroupNoticesList, e); }

        static public readonly string[] paramNamesOnAssetReceived = new string[] { "transfer", "asset" };
        static public readonly Type[] paramTypesOnAssetReceived = new Type[] { typeof(AssetDownload), typeof(Asset) };

        public virtual void Assets_OnAssetReceived(AssetDownload transfer, Asset asset) { OnEvent("On-Asset-Received", paramNamesOnAssetReceived, paramTypesOnAssetReceived, transfer, asset); }

        static public readonly string[] paramNamesOnXferReceived = new string[] { "xfer" };
        static public readonly Type[] paramTypesOnXferReceived = new Type[] { typeof(XferDownload) };

        public virtual void Assets_OnXferReceived(object sender, XferReceivedEventArgs e) { OnEvent("On-Xfer-Received", paramNamesOnXferReceived, paramTypesOnXferReceived, e.Xfer); }

        static public readonly string[] paramNamesOnImageReceived = new string[] { "image", "asset" };
        static public readonly Type[] paramTypesOnImageReceived = new Type[] { typeof(ImageDownload), typeof(AssetTexture) };

        public virtual void Assets_OnImageReceived(ImageDownload image, AssetTexture asset) { OnEvent("On-Image-Received", paramNamesOnImageReceived, paramTypesOnImageReceived, image, asset); }

        static public readonly string[] paramNamesOnImageReceiveProgress = new string[] { "image", "recieved", "total" };
        static public readonly Type[] paramTypesOnImageReceiveProgress = new Type[] { typeof(UUID),  typeof(int), typeof(int) };

        public virtual void Assets_OnImageReceiveProgress(object sender, ImageReceiveProgressEventArgs e) { OnEvent("On-Image-Receive-Progress", paramNamesOnImageReceiveProgress, paramTypesOnImageReceiveProgress, e.ImageID,  e.Received, e.Total); }

        static public readonly string[] paramNamesOnAssetUploaded = new string[] { "upload" };
        static public readonly Type[] paramTypesOnAssetUploaded = new Type[] { typeof(AssetUpload) };

        public virtual void Assets_OnAssetUploaded(object sender, AssetUploadEventArgs e) { OnEvent("On-Asset-Uploaded", paramNamesOnAssetUploaded, paramTypesOnAssetUploaded, e.Upload); }

        static public readonly string[] paramNamesOnUploadProgress = new string[] { "upload" };
        static public readonly Type[] paramTypesOnUploadProgress = new Type[] { typeof(AssetUpload) };

        public virtual void Assets_OnUploadProgress(object sender, AssetUploadEventArgs e) { OnEvent("On-Upload-Progress", paramNamesOnUploadProgress, paramTypesOnUploadProgress, e.Upload); }

        static public readonly string[] paramNamesOnAgentWearables = new string[] { };
        static public readonly Type[] paramTypesOnAgentWearables = new Type[] { };

        public virtual void Appearance_OnAgentWearables(object sender, AgentWearablesReplyEventArgs e) { OnEvent("On-Agent-Wearables", paramNamesOnAgentWearables, paramTypesOnAgentWearables); }

        static public readonly string[] paramNamesOnAppearanceUpdated = new string[] { "te" };
        static public readonly Type[] paramTypesOnAppearanceUpdated = new Type[] { typeof(Primitive.TextureEntry) };

        public virtual void Appearance_OnAppearanceUpdated(Primitive.TextureEntry te) { OnEvent("On-Appearance-Updated", paramNamesOnAppearanceUpdated, paramTypesOnAppearanceUpdated, te); }

        static public readonly string[] paramNamesOnItemReceived = new string[] { "item" };
        static public readonly Type[] paramTypesOnItemReceived = new Type[] { typeof(InventoryItem) };

        public virtual void Inventory_OnItemReceived(object sender, ItemReceivedEventArgs e) { OnEvent("On-Item-Received", paramNamesOnItemReceived, paramTypesOnItemReceived, e); }

        static public readonly string[] paramNamesOnFolderUpdated = new string[] { "folderID" };
        static public readonly Type[] paramTypesOnFolderUpdated = new Type[] { typeof(UUID) };

        public virtual void Inventory_OnFolderUpdated(object sender, FolderUpdatedEventArgs e) { OnEvent("On-Folder-Updated", paramNamesOnFolderUpdated, paramTypesOnFolderUpdated, e); }

        static public readonly string[] paramNamesOnObjectOffered = new string[] { "offerDetails", "type", "objectID", "fromTask" };
        static public readonly Type[] paramTypesOnObjectOffered = new Type[] { typeof(InstantMessage), typeof(AssetType), typeof(UUID), typeof(bool) };

        public void Inventory_OnObjectOffered(object sender, InventoryObjectOfferedEventArgs e)
        { e.Accept = OnEvent("On-Object-Offered", paramNamesOnObjectOffered, paramTypesOnObjectOffered, e); }

        static public readonly string[] paramNamesOnFindObjectByPath = new string[] { "path", "inventoryObjectID" };
        static public readonly Type[] paramTypesOnFindObjectByPath = new Type[] { typeof(string), typeof(UUID) };

        public virtual void Inventory_OnFindObjectByPath(object sender, FindObjectByPathReplyEventArgs e) { OnEvent("On-Find-Object-By-Path", paramNamesOnFindObjectByPath, paramTypesOnFindObjectByPath, e); }

        static public readonly string[] paramNamesOnTaskItemReceived = new string[] { "itemID", "folderID", "creatorID", "assetID", "type" };
        static public readonly Type[] paramTypesOnTaskItemReceived = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(UUID), typeof(InventoryType) };

        public virtual void Inventory_OnTaskItemReceived(object sender, TaskItemReceivedEventArgs e) { OnEvent("On-Task-Item-Received", paramNamesOnTaskItemReceived, paramTypesOnTaskItemReceived,e); }

        static public readonly string[] paramNamesOnTaskInventoryReply = new string[] { "itemID", "serial", "assetFilename" };
        static public readonly Type[] paramTypesOnTaskInventoryReply = new Type[] { typeof(UUID), typeof(short), typeof(string) };

        public virtual void Inventory_OnTaskInventoryReply(object sender, TaskInventoryReplyEventArgs e) { OnEvent("On-Task-Inventory-Reply", paramNamesOnTaskInventoryReply, paramTypesOnTaskInventoryReply, e); }

        static public readonly string[] paramNamesOnClassifiedReply = new string[] { "classifieds" };
        static public readonly Type[] paramTypesOnClassifiedReply = new Type[] { typeof(List<DirectoryManager.Classified>) };

        public virtual void Directory_OnClassifiedReply(Object sender, DirClassifiedsReplyEventArgs e) { OnEvent("On-Classified-Reply", paramNamesOnClassifiedReply, paramTypesOnClassifiedReply, e.Classifieds); }

        static public readonly string[] paramNamesOnDirLandReply = new string[] { "dirParcels" };
        static public readonly Type[] paramTypesOnDirLandReply = new Type[] { typeof(List<DirectoryManager.DirectoryParcel>) };

        public virtual void Directory_OnDirLandReply(Object sender, DirLandReplyEventArgs e) { OnEvent("On-Dir-Land-Reply", paramNamesOnDirLandReply, paramTypesOnDirLandReply, e.DirParcels); }

        static public readonly string[] paramNamesOnDirPeopleReply = new string[] { "queryID", "matchedPeople" };
        static public readonly Type[] paramTypesOnDirPeopleReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.AgentSearchData>) };

        public virtual void Directory_OnDirPeopleReply(Object sender, DirPeopleReplyEventArgs e) { OnEvent("On-Dir-People-Reply", paramNamesOnDirPeopleReply, paramTypesOnDirPeopleReply, e.QueryID, e.MatchedPeople); }

        static public readonly string[] paramNamesOnDirGroupsReply = new string[] { "queryID", "matchedGroups" };
        static public readonly Type[] paramTypesOnDirGroupsReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.GroupSearchData>) };

        public virtual void Directory_OnDirGroupsReply(Object sender, DirGroupsReplyEventArgs e) { OnEvent("On-Dir-Groups-Reply", paramNamesOnDirGroupsReply, paramTypesOnDirGroupsReply, e.QueryID, e.MatchedGroups); }

        static public readonly string[] paramNamesOnPlacesReply = new string[] { "queryID", "matchedPlaces" };
        static public readonly Type[] paramTypesOnPlacesReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.PlacesSearchData>) };

        public virtual void Directory_OnPlacesReply(Object sender, DirPlacesReplyEventArgs e) { OnEvent("On-Places-Reply", paramNamesOnPlacesReply, paramTypesOnPlacesReply, e.QueryID, e.MatchedParcels); }

        static public readonly string[] paramNamesOnEventsReply = new string[] { "queryID", "matchedEvents" };
        static public readonly Type[] paramTypesOnEventsReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.EventsSearchData>) };

        public virtual void Directory_OnEventsReply(Object sender, DirEventsReplyEventArgs e) { OnEvent("On-Events-Reply", paramNamesOnEventsReply, paramTypesOnEventsReply, e.QueryID, e.MatchedEvents); }

        static public readonly string[] paramNamesOnEventInfo = new string[] { "matchedEvent" };
        static public readonly Type[] paramTypesOnEventInfo = new Type[] { typeof(DirectoryManager.EventInfo) };

        public virtual void Directory_OnEventInfo(Object sender, EventInfoReplyEventArgs e) { OnEvent("On-Event-Info", paramNamesOnEventInfo, paramTypesOnEventInfo, e.MatchedEvent); }

        static public readonly string[] paramNamesOnLandPatch = new string[] { "simulator", "x", "y", "width", "data" };
        static public readonly Type[] paramTypesOnLandPatch = new Type[] { typeof(Simulator), typeof(int), typeof(int), typeof(int), typeof(float[]) };

        public virtual void Terrain_OnLandPatch(object sender, LandPatchReceivedEventArgs e) { OnEvent("On-Land-Patch", paramNamesOnLandPatch, paramTypesOnLandPatch, e.Simulator, e.X, e.Y, e.PatchSize, e.HeightMap); }

        static public readonly string[] paramNamesOnAttachSound = new string[] { "soundID", "ownerID", "objectID", "gain", "flags" };
        static public readonly Type[] paramTypesOnAttachSound = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(float), typeof(byte) };

        public virtual void Sound_OnAttachSound(object sender, AttachedSoundEventArgs e) { OnEvent("On-Attach-Sound", paramNamesOnAttachSound, paramTypesOnAttachSound, e); }

        static public readonly string[] paramNamesOnAttachSoundGainChange = new string[] { "objectID", "gain" };
        static public readonly Type[] paramTypesOnAttachSoundGainChange = new Type[] { typeof(UUID), typeof(float) };

        public virtual void Sound_OnAttachSoundGainChange(object sender, AttachedSoundGainChangeEventArgs e) { OnEvent("On-Attach-Sound-Gain-Change", paramNamesOnAttachSoundGainChange, paramTypesOnAttachSoundGainChange, e); }

        static public readonly string[] paramNamesOnSoundTrigger = new string[] { "soundID", "ownerID", "objectID", "parentID", "gain", "regionHandle", "position" };
        static public readonly Type[] paramTypesOnSoundTrigger = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(UUID), typeof(float), typeof(ulong), typeof(Vector3) };

        public virtual void Sound_OnSoundTrigger(object sender, SoundTriggerEventArgs e) { OnEvent("On-Sound-Trigger", paramNamesOnSoundTrigger, paramTypesOnSoundTrigger, e); }

        static public readonly string[] paramNamesOnPreloadSound = new string[] { "soundID", "ownerID", "objectID" };
        static public readonly Type[] paramTypesOnPreloadSound = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID) };

        public virtual void Sound_OnPreloadSound(object sender, PreloadSoundEventArgs e) { OnEvent("On-Preload-Sound", paramNamesOnPreloadSound, paramTypesOnPreloadSound, e); }

        public virtual void RegisterAll()
        {
            client.Network.LoginProgress += Network_OnLogin;
            //client.Network.OnConnected += Network_OnConnected;
            client.Network.LoggedOut += Network_OnLogoutReply;
            client.Network.SimConnecting += Network_OnSimConnecting;
            client.Network.SimConnected += Network_OnSimConnected;
            client.Network.SimDisconnected += Network_OnSimDisconnected;
            client.Network.Disconnected += Network_OnDisconnected;
            client.Network.SimChanged += Network_OnCurrentSimChanged;
            client.Network.EventQueueRunning += Network_OnEventQueueRunning;
            client.Parcels.ParcelDwellReply += Parcels_OnParcelDwell;
            client.Parcels.ParcelInfoReply += Parcels_OnParcelInfo;
            client.Parcels.ParcelProperties += Parcels_OnParcelProperties;
            client.Parcels.ParcelAccessListReply += Parcels_OnAccessListReply;
            client.Parcels.ParcelObjectOwnersReply += Parcels_OnPrimOwnersListReply;
            client.Parcels.SimParcelsDownloaded += Parcels_OnSimParcelsDownloaded;
            client.Parcels.ForceSelectObjectsReply += Parcels_OnParcelSelectedObjects;
            client.Parcels.ParcelMediaUpdateReply += Parcels_OnParcelMediaUpdate;
            client.Self.ChatFromSimulator += Self_OnChat;
            client.Self.ScriptDialog += Self_OnScriptDialog;
            client.Self.ScriptQuestion += Self_OnScriptQuestion;
            client.Self.LoadURL += Self_OnLoadURL;
            client.Self.IM += Self_OnInstantMessage;
            client.Self.TeleportProgress += Self_OnTeleport;
            client.Self.MoneyBalance += Self_OnBalanceUpdated;
            client.Self.MoneyBalanceReply += Self_OnMoneyBalanceReplyReceived;
            client.Self.AgentDataReply += Self_OnAgentDataUpdated;
            client.Self.AnimationsChanged += Self_OnAnimationsChanged;
            client.Self.MeanCollision += Self_OnMeanCollision;
            client.Self.RegionCrossed += Self_OnRegionCrossed;
            client.Self.GroupChatJoined += Self_OnGroupChatJoin;
            client.Self.GroupChatLeft += Self_OnGroupChatLeft;
            client.Self.AlertMessage += Self_OnAlertMessage;
            client.Self.ScriptControlChange += Self_OnScriptControlChange;
            client.Self.CameraConstraint += Self_OnCameraConstraint;
            client.Self.ScriptSensorReply += Self_OnScriptSensorReply;
            client.Self.AvatarSitResponse += Self_OnAvatarSitResponse;
            client.Self.ChatSessionMemberAdded += Self_OnChatSessionMemberAdded;
            client.Self.ChatSessionMemberLeft += Self_OnChatSessionMemberLeft;
            client.Avatars.AvatarAppearance += Avatars_OnAvatarAppearance;
            //client.Avatars.OnAvatarAnimation += Avatars_OnAvatarAnimation;

            client.Avatars.AvatarPickerReply += Avatars_OnAvatarNameSearch;
            client.Avatars.AvatarInterestsReply += Avatars_OnAvatarInterests;
            client.Avatars.AvatarPropertiesReply += Avatars_OnAvatarProperties;
            client.Avatars.AvatarGroupsReply += Avatars_OnAvatarGroups;
            client.Avatars.UUIDNameReply += Avatars_OnAvatarNames;
            client.Avatars.ViewerEffectPointAt += Avatars_OnPointAt;
            client.Avatars.ViewerEffectLookAt += Avatars_OnLookAt;
            client.Avatars.ViewerEffect += Avatars_OnEffect;
            client.Avatars.AvatarPicksReply += Avatars_OnAvatarPicks;
            client.Avatars.PickInfoReply += Avatars_OnPickInfo;
            client.Friends.FriendNames += Friends_OnFriendNamesReceived;
            client.Friends.FriendOnline += Friends_OnFriendOnline;
            client.Friends.FriendOffline += Friends_OnFriendOffline;
            client.Friends.FriendRightsUpdate += Friends_OnFriendRights;
            client.Friends.FriendshipOffered += Friends_OnFriendshipOffered;
            client.Friends.FriendshipResponse += Friends_OnFriendshipResponse;
            client.Friends.FriendshipTerminated += Friends_OnFriendshipTerminated;
            client.Friends.FriendFoundReply += Friends_OnFriendFound;
            client.Grid.CoarseLocationUpdate += Grid_OnCoarseLocationUpdate;
            client.Grid.GridRegion += Grid_OnGridRegion;
            client.Grid.GridLayer += Grid_OnGridLayer;
            client.Grid.GridItems += Grid_OnGridItems;
            client.Grid.RegionHandleReply += Grid_OnRegionHandleReply;
            client.Objects.ObjectUpdate += Objects_OnNewPrim;
            //TODO client.Objects.OnNewAttachment += Objects_OnNewAttachment;
            client.Objects.AvatarUpdate += Objects_OnNewAvatar;
            //client.Objects.ObjectPropertiesUpdated += Objects_OnObjectUpdated;
            client.Objects.AvatarSitChanged += Objects_OnAvatarSitChanged;
            client.Objects.KillObject += Objects_OnObjectKilled;
            client.Objects.ObjectProperties += Objects_OnObjectProperties;
            client.Objects.ObjectPropertiesFamily += Objects_OnObjectPropertiesFamily;
            client.Groups.CurrentGroups += Groups_OnCurrentGroups;
            client.Groups.GroupNamesReply += Groups_OnGroupNames;
            client.Groups.GroupProfile += Groups_OnGroupProfile;
            client.Groups.GroupMembersReply += Groups_OnGroupMembers;
            client.Groups.GroupRoleDataReply += Groups_OnGroupRoles;
            client.Groups.GroupRoleMembersReply += Groups_OnGroupRolesMembers;
            client.Groups.GroupTitlesReply += Groups_OnGroupTitles;
            client.Groups.GroupAccountSummaryReply += Groups_OnGroupAccountSummary;
            client.Groups.GroupCreatedReply += Groups_OnGroupCreated;
            client.Groups.GroupJoinedReply += Groups_OnGroupJoined;
            client.Groups.GroupLeaveReply += Groups_OnGroupLeft;
            client.Groups.GroupDropped += Groups_OnGroupDropped;
            client.Groups.GroupMemberEjected += Groups_OnGroupMemberEjected;
            client.Groups.GroupNoticesListReply += Groups_OnGroupNoticesList;
            // client.Assets.OnAssetReceived += Assets_OnAssetReceived;
            client.Assets.XferReceived += Assets_OnXferReceived;
            //todo client.Assets.OnImageReceived += Assets_OnImageReceived;
            client.Assets.ImageReceiveProgress += Assets_OnImageReceiveProgress;
            client.Assets.AssetUploaded += Assets_OnAssetUploaded;
            client.Assets.UploadProgress += Assets_OnUploadProgress;
            client.Appearance.AgentWearablesReply += Appearance_OnAgentWearables;
            //client.Appearance.OnAppearanceUpdated += Appearance_OnAppearanceUpdated;
            client.Inventory.ItemReceived += Inventory_OnItemReceived;
            client.Inventory.FolderUpdated += Inventory_OnFolderUpdated;
            client.Inventory.InventoryObjectOffered += Inventory_OnObjectOffered;
            client.Inventory.FindObjectByPathReply += Inventory_OnFindObjectByPath;
            client.Inventory.TaskItemReceived += Inventory_OnTaskItemReceived;
            client.Inventory.TaskInventoryReply += Inventory_OnTaskInventoryReply;
            client.Directory.DirClassifiedsReply += Directory_OnClassifiedReply;
            client.Directory.DirLandReply += Directory_OnDirLandReply;
            client.Directory.DirPeopleReply += Directory_OnDirPeopleReply;
            client.Directory.DirGroupsReply += Directory_OnDirGroupsReply;
            client.Directory.DirPlacesReply += Directory_OnPlacesReply;
            client.Directory.DirEventsReply += Directory_OnEventsReply;
            client.Directory.EventInfoReply += Directory_OnEventInfo;
            client.Terrain.LandPatchReceived += Terrain_OnLandPatch;
            client.Sound.AttachedSound += Sound_OnAttachSound;
            client.Sound.AttachedSoundGainChange += Sound_OnAttachSoundGainChange;
            client.Sound.SoundTrigger += Sound_OnSoundTrigger;
            client.Sound.PreloadSound += Sound_OnPreloadSound;
        }

        private void RegisterManagerLegacy(object manager)
        {
            

        }

        private void RegisterManager(object networkManager)
        {
          
        }
        

        public virtual void UnregisterAll()
        {
            client.Network.LoginProgress -= Network_OnLogin;
            //client.Network.OnConnected -= Network_OnConnected;
            client.Network.LoggedOut -= Network_OnLogoutReply;
            client.Network.SimConnecting -= Network_OnSimConnecting;
            client.Network.SimConnected -= Network_OnSimConnected;
            client.Network.SimDisconnected -= Network_OnSimDisconnected;
            client.Network.Disconnected -= Network_OnDisconnected;
            client.Network.SimChanged -= Network_OnCurrentSimChanged;
            client.Network.EventQueueRunning -= Network_OnEventQueueRunning;
            client.Parcels.ParcelDwellReply -= Parcels_OnParcelDwell;
            client.Parcels.ParcelInfoReply -= Parcels_OnParcelInfo;
            client.Parcels.ParcelProperties -= Parcels_OnParcelProperties;
            client.Parcels.ParcelAccessListReply -= Parcels_OnAccessListReply;
            client.Parcels.ParcelObjectOwnersReply -= Parcels_OnPrimOwnersListReply;
            client.Parcels.SimParcelsDownloaded -= Parcels_OnSimParcelsDownloaded;
            client.Parcels.ForceSelectObjectsReply -= Parcels_OnParcelSelectedObjects;
            client.Parcels.ParcelMediaUpdateReply -= Parcels_OnParcelMediaUpdate;
            client.Self.ChatFromSimulator -= Self_OnChat;
            client.Self.ScriptDialog -= Self_OnScriptDialog;
            client.Self.ScriptQuestion -= Self_OnScriptQuestion;
            client.Self.LoadURL -= Self_OnLoadURL;
            client.Self.IM -= Self_OnInstantMessage;
            client.Self.TeleportProgress -= Self_OnTeleport;
            client.Self.MoneyBalance -= Self_OnBalanceUpdated;
            client.Self.MoneyBalanceReply -= Self_OnMoneyBalanceReplyReceived;
            client.Self.AgentDataReply -= Self_OnAgentDataUpdated;
            client.Self.AnimationsChanged -= Self_OnAnimationsChanged;
            client.Self.MeanCollision -= Self_OnMeanCollision;
            client.Self.RegionCrossed -= Self_OnRegionCrossed;
            client.Self.GroupChatJoined -= Self_OnGroupChatJoin;
            client.Self.GroupChatLeft -= Self_OnGroupChatLeft;
            client.Self.AlertMessage -= Self_OnAlertMessage;
            client.Self.ScriptControlChange -= Self_OnScriptControlChange;
            client.Self.CameraConstraint -= Self_OnCameraConstraint;
            client.Self.ScriptSensorReply -= Self_OnScriptSensorReply;
            client.Self.AvatarSitResponse -= Self_OnAvatarSitResponse;
            client.Self.ChatSessionMemberAdded -= Self_OnChatSessionMemberAdded;
            client.Self.ChatSessionMemberLeft -= Self_OnChatSessionMemberLeft;
            client.Avatars.AvatarAppearance -= Avatars_OnAvatarAppearance;
            //client.Avatars.OnAvatarAnimation -= Avatars_OnAvatarAnimation;

            client.Avatars.AvatarPickerReply -= Avatars_OnAvatarNameSearch;
            client.Avatars.AvatarInterestsReply -= Avatars_OnAvatarInterests;
            client.Avatars.AvatarPropertiesReply -= Avatars_OnAvatarProperties;
            client.Avatars.AvatarGroupsReply -= Avatars_OnAvatarGroups;
            client.Avatars.UUIDNameReply -= Avatars_OnAvatarNames;
            client.Avatars.ViewerEffectPointAt -= Avatars_OnPointAt;
            client.Avatars.ViewerEffectLookAt -= Avatars_OnLookAt;
            client.Avatars.ViewerEffect -= Avatars_OnEffect;
            client.Avatars.AvatarPicksReply -= Avatars_OnAvatarPicks;
            client.Avatars.PickInfoReply -= Avatars_OnPickInfo;
            client.Friends.FriendNames -= Friends_OnFriendNamesReceived;
            client.Friends.FriendOnline -= Friends_OnFriendOnline;
            client.Friends.FriendOffline -= Friends_OnFriendOffline;
            client.Friends.FriendRightsUpdate -= Friends_OnFriendRights;
            client.Friends.FriendshipOffered -= Friends_OnFriendshipOffered;
            client.Friends.FriendshipResponse -= Friends_OnFriendshipResponse;
            client.Friends.FriendshipTerminated -= Friends_OnFriendshipTerminated;
            client.Friends.FriendFoundReply -= Friends_OnFriendFound;
            client.Grid.CoarseLocationUpdate -= Grid_OnCoarseLocationUpdate;
            client.Grid.GridRegion -= Grid_OnGridRegion;
            client.Grid.GridLayer -= Grid_OnGridLayer;
            client.Grid.GridItems -= Grid_OnGridItems;
            client.Grid.RegionHandleReply -= Grid_OnRegionHandleReply;
            client.Objects.ObjectUpdate -= Objects_OnNewPrim;
            //TODO client.Objects.OnNewAttachment -= Objects_OnNewAttachment;
            client.Objects.AvatarUpdate -= Objects_OnNewAvatar;
            //client.Objects.ObjectPropertiesUpdated -= Objects_OnObjectUpdated;
            client.Objects.AvatarSitChanged -= Objects_OnAvatarSitChanged;
            client.Objects.KillObject -= Objects_OnObjectKilled;
            client.Objects.ObjectProperties -= Objects_OnObjectProperties;
            client.Objects.ObjectPropertiesFamily -= Objects_OnObjectPropertiesFamily;
            client.Groups.CurrentGroups -= Groups_OnCurrentGroups;
            client.Groups.GroupNamesReply -= Groups_OnGroupNames;
            client.Groups.GroupProfile -= Groups_OnGroupProfile;
            client.Groups.GroupMembersReply -= Groups_OnGroupMembers;
            client.Groups.GroupRoleDataReply -= Groups_OnGroupRoles;
            client.Groups.GroupRoleMembersReply -= Groups_OnGroupRolesMembers;
            client.Groups.GroupTitlesReply -= Groups_OnGroupTitles;
            client.Groups.GroupAccountSummaryReply -= Groups_OnGroupAccountSummary;
            client.Groups.GroupCreatedReply -= Groups_OnGroupCreated;
            client.Groups.GroupJoinedReply -= Groups_OnGroupJoined;
            client.Groups.GroupLeaveReply -= Groups_OnGroupLeft;
            client.Groups.GroupDropped -= Groups_OnGroupDropped;
            client.Groups.GroupMemberEjected -= Groups_OnGroupMemberEjected;
            client.Groups.GroupNoticesListReply -= Groups_OnGroupNoticesList;
            // client.Assets.OnAssetReceived -= Assets_OnAssetReceived;
            client.Assets.XferReceived -= Assets_OnXferReceived;
            //todo client.Assets.OnImageReceived -= Assets_OnImageReceived;
            client.Assets.ImageReceiveProgress -= Assets_OnImageReceiveProgress;
            client.Assets.AssetUploaded -= Assets_OnAssetUploaded;
            client.Assets.UploadProgress -= Assets_OnUploadProgress;
            client.Appearance.AgentWearablesReply -= Appearance_OnAgentWearables;
            //client.Appearance.OnAppearanceUpdated -= Appearance_OnAppearanceUpdated;
            client.Inventory.ItemReceived -= Inventory_OnItemReceived;
            client.Inventory.FolderUpdated -= Inventory_OnFolderUpdated;
            client.Inventory.InventoryObjectOffered -= Inventory_OnObjectOffered;
            client.Inventory.FindObjectByPathReply -= Inventory_OnFindObjectByPath;
            client.Inventory.TaskItemReceived -= Inventory_OnTaskItemReceived;
            client.Inventory.TaskInventoryReply -= Inventory_OnTaskInventoryReply;
            client.Directory.DirClassifiedsReply -= Directory_OnClassifiedReply;
            client.Directory.DirLandReply -= Directory_OnDirLandReply;
            client.Directory.DirPeopleReply -= Directory_OnDirPeopleReply;
            client.Directory.DirGroupsReply -= Directory_OnDirGroupsReply;
            client.Directory.DirPlacesReply -= Directory_OnPlacesReply;
            client.Directory.DirEventsReply -= Directory_OnEventsReply;
            client.Directory.EventInfoReply -= Directory_OnEventInfo;
            client.Terrain.LandPatchReceived -= Terrain_OnLandPatch;
            client.Sound.AttachedSound -= Sound_OnAttachSound;
            client.Sound.AttachedSoundGainChange -= Sound_OnAttachSoundGainChange;
            client.Sound.SoundTrigger -= Sound_OnSoundTrigger;
            client.Sound.PreloadSound -= Sound_OnPreloadSound;

        }

    }
#pragma warning restore 0168

}

