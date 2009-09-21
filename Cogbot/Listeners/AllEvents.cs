using System;
using System.Collections.Generic;
using System.Text;
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
    public abstract class AllEvents : Listener
    {
        public AllEvents(BotClient bot)
            : base(bot)
        {
        }

        static public readonly string[] paramNamesOnAvatarAnimation = new string[] { "agentID", "agentAnimations" };
        static public readonly Type[] paramTypesOnAvatarAnimation = new Type[] { typeof(UUID), typeof(InternalDictionary<UUID, int>) };

        public virtual void Avatars_OnAvatarAnimation(UUID agentID, InternalDictionary<UUID, int> agentAnimations) { OnEvent("On-Avatar-Animation", paramNamesOnAvatarAnimation, paramTypesOnAvatarAnimation, agentID, agentAnimations); }



        public abstract bool BooleanOnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters);
        public virtual void OnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters)
        {
            BooleanOnEvent(eventName, paramNames, paramTypes, parameters);
        }

        static public readonly string[] paramNamesOnLogin = new string[] { "login", "message" };
        static public readonly Type[] paramTypesOnLogin = new Type[] { typeof(LoginStatus), typeof(string) };

        public virtual void Network_OnLogin(LoginStatus login, string message) { OnEvent("On-Login", paramNamesOnLogin, paramTypesOnLogin, login, message); }

        static public readonly string[] paramNamesOnConnected = new string[] { "sender" };
        static public readonly Type[] paramTypesOnConnected = new Type[] { typeof(System.Object) };

        public virtual void Network_OnConnected(System.Object sender) { OnEvent("On-Connected", paramNamesOnConnected, paramTypesOnConnected, sender); }

        static public readonly string[] paramNamesOnLogoutReply = new string[] { "inventoryItems" };
        static public readonly Type[] paramTypesOnLogoutReply = new Type[] { typeof(List<UUID>) };

        public virtual void Network_OnLogoutReply(List<UUID> inventoryItems) { OnEvent("On-Logout-Reply", paramNamesOnLogoutReply, paramTypesOnLogoutReply, inventoryItems); }

        static public readonly string[] paramNamesOnSimConnecting = new string[] { "simulator" };
        static public readonly Type[] paramTypesOnSimConnecting = new Type[] { typeof(Simulator) };

        public virtual bool Network_OnSimConnecting(Simulator simulator)
        { return BooleanOnEvent("On-Sim-Connecting", paramNamesOnSimConnecting, paramTypesOnSimConnecting, simulator); }

        static public readonly string[] paramNamesOnSimConnected = new string[] { "simulator" };
        static public readonly Type[] paramTypesOnSimConnected = new Type[] { typeof(Simulator) };

        public virtual void Network_OnSimConnected(Simulator simulator) { OnEvent("On-Sim-Connected", paramNamesOnSimConnected, paramTypesOnSimConnected, simulator); }

        static public readonly string[] paramNamesOnSimDisconnected = new string[] { "simulator", "reason" };
        static public readonly Type[] paramTypesOnSimDisconnected = new Type[] { typeof(Simulator), typeof(NetworkManager.DisconnectType) };

        public virtual void Network_OnSimDisconnected(Simulator simulator, NetworkManager.DisconnectType reason) { OnEvent("On-Sim-Disconnected", paramNamesOnSimDisconnected, paramTypesOnSimDisconnected, simulator, reason); }

        static public readonly string[] paramNamesOnDisconnected = new string[] { "reason", "message" };
        static public readonly Type[] paramTypesOnDisconnected = new Type[] { typeof(NetworkManager.DisconnectType), typeof(string) };

        public virtual void Network_OnDisconnected(NetworkManager.DisconnectType reason, string message) { OnEvent("On-Disconnected", paramNamesOnDisconnected, paramTypesOnDisconnected, reason, message); }

        static public readonly string[] paramNamesOnCurrentSimChanged = new string[] { "PreviousSimulator" };
        static public readonly Type[] paramTypesOnCurrentSimChanged = new Type[] { typeof(Simulator) };

        public virtual void Network_OnCurrentSimChanged(Simulator PreviousSimulator) { OnEvent("On-Current-Sim-Changed", paramNamesOnCurrentSimChanged, paramTypesOnCurrentSimChanged, PreviousSimulator); }

        static public readonly string[] paramNamesOnEventQueueRunning = new string[] { "simulator" };
        static public readonly Type[] paramTypesOnEventQueueRunning = new Type[] { typeof(Simulator) };

        public virtual void Network_OnEventQueueRunning(Simulator simulator) { OnEvent("On-Event-Queue-Running", paramNamesOnEventQueueRunning, paramTypesOnEventQueueRunning, simulator); }

        static public readonly string[] paramNamesOnParcelDwell = new string[] { "parcelID", "localID", "dwell" };
        static public readonly Type[] paramTypesOnParcelDwell = new Type[] { typeof(UUID), typeof(int), typeof(float) };

        public virtual void Parcels_OnParcelDwell(UUID parcelID, int localID, float dwell) { OnEvent("On-Parcel-Dwell", paramNamesOnParcelDwell, paramTypesOnParcelDwell, parcelID, localID, dwell); }

        static public readonly string[] paramNamesOnParcelInfo = new string[] { "parcel" };
        static public readonly Type[] paramTypesOnParcelInfo = new Type[] { typeof(ParcelInfo) };

        public virtual void Parcels_OnParcelInfo(ParcelInfo parcel) { OnEvent("On-Parcel-Info", paramNamesOnParcelInfo, paramTypesOnParcelInfo, parcel); }

        static public readonly string[] paramNamesOnParcelProperties = new string[] { "simulator", "parcel", "result", "selectedPrims", "sequenceID", "snapSelection" };
        static public readonly Type[] paramTypesOnParcelProperties = new Type[] { typeof(Simulator), typeof(Parcel), typeof(ParcelResult), typeof(int), typeof(int), typeof(bool) };

        public virtual void Parcels_OnParcelProperties(Simulator simulator, Parcel parcel, ParcelResult result, int selectedPrims, int sequenceID, bool snapSelection) { OnEvent("On-Parcel-Properties", paramNamesOnParcelProperties, paramTypesOnParcelProperties, simulator, parcel, result, selectedPrims, sequenceID, snapSelection); }

        static public readonly string[] paramNamesOnAccessListReply = new string[] { "simulator", "sequenceID", "localID", "flags", "accessEntries" };
        static public readonly Type[] paramTypesOnAccessListReply = new Type[] { typeof(Simulator), typeof(int), typeof(int), typeof(uint), typeof(List<ParcelManager.ParcelAccessEntry>) };

        public virtual void Parcels_OnAccessListReply(Simulator simulator, int sequenceID, int localID, uint flags, List<ParcelManager.ParcelAccessEntry> accessEntries) { OnEvent("On-Access-List-Reply", paramNamesOnAccessListReply, paramTypesOnAccessListReply, simulator, sequenceID, localID, flags, accessEntries); }

        static public readonly string[] paramNamesOnPrimOwnersListReply = new string[] { "simulator", "primOwners" };
        static public readonly Type[] paramTypesOnPrimOwnersListReply = new Type[] { typeof(Simulator), typeof(List<ParcelManager.ParcelPrimOwners>) };

        public virtual void Parcels_OnPrimOwnersListReply(Simulator simulator, List<ParcelManager.ParcelPrimOwners> primOwners) { OnEvent("On-Prim-Owners-List-Reply", paramNamesOnPrimOwnersListReply, paramTypesOnPrimOwnersListReply, simulator, primOwners); }

        static public readonly string[] paramNamesOnSimParcelsDownloaded = new string[] { "simulator", "simParcels", "parcelMap" };
        static public readonly Type[] paramTypesOnSimParcelsDownloaded = new Type[] { typeof(Simulator), typeof(InternalDictionary<int, Parcel>), typeof(int[,]) };

        public virtual void Parcels_OnSimParcelsDownloaded(Simulator simulator, InternalDictionary<int, Parcel> simParcels, int[,] parcelMap) { OnEvent("On-Sim-Parcels-Downloaded", paramNamesOnSimParcelsDownloaded, paramTypesOnSimParcelsDownloaded, simulator, simParcels, parcelMap); }

        static public readonly string[] paramNamesOnParcelSelectedObjects = new string[] { "simulator", "objectIDs", "resetList" };
        static public readonly Type[] paramTypesOnParcelSelectedObjects = new Type[] { typeof(Simulator), typeof(List<uint>), typeof(bool) };

        public virtual void Parcels_OnParcelSelectedObjects(Simulator simulator, List<uint> objectIDs, bool resetList) { OnEvent("On-Parcel-Selected-Objects", paramNamesOnParcelSelectedObjects, paramTypesOnParcelSelectedObjects, simulator, objectIDs, resetList); }

        static public readonly string[] paramNamesOnParcelMediaUpdate = new string[] { "simulator", "media" };
        static public readonly Type[] paramTypesOnParcelMediaUpdate = new Type[] { typeof(Simulator), typeof(ParcelMedia) };

        public virtual void Parcels_OnParcelMediaUpdate(Simulator simulator, ParcelMedia media) { OnEvent("On-Parcel-Media-Update", paramNamesOnParcelMediaUpdate, paramTypesOnParcelMediaUpdate, simulator, media); }

        static public readonly string[] paramNamesOnChat = new string[] { "message", "audible", "type", "sourceType", "fromName", "id", "ownerid", "position" };
        static public readonly Type[] paramTypesOnChat = new Type[] { typeof(string), typeof(ChatAudibleLevel), typeof(ChatType), typeof(ChatSourceType), typeof(string), typeof(UUID), typeof(UUID), typeof(Vector3) };

        public virtual void Self_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourceType, string fromName, UUID id, UUID ownerid, Vector3 position) { OnEvent("On-Chat", paramNamesOnChat, paramTypesOnChat, message, audible, type, sourceType, fromName, id, ownerid, position); }

        static public readonly string[] paramNamesOnScriptDialog = new string[] { "message", "objectName", "imageID", "objectID", "firstName", "lastName", "chatChannel", "buttons" };
        static public readonly Type[] paramTypesOnScriptDialog = new Type[] { typeof(string), typeof(string), typeof(UUID), typeof(UUID), typeof(string), typeof(string), typeof(int), typeof(List<string>) };

        public virtual void Self_OnScriptDialog(string message, string objectName, UUID imageID, UUID objectID, string firstName, string lastName, int chatChannel, List<string> buttons) { OnEvent("On-Script-Dialog", paramNamesOnScriptDialog, paramTypesOnScriptDialog, message, objectName, imageID, objectID, firstName, lastName, chatChannel, buttons); }

        static public readonly string[] paramNamesOnScriptQuestion = new string[] { "simulator", "taskID", "itemID", "objectName", "objectOwner", "questions" };
        static public readonly Type[] paramTypesOnScriptQuestion = new Type[] { typeof(Simulator), typeof(UUID), typeof(UUID), typeof(string), typeof(string), typeof(ScriptPermission) };

        public virtual void Self_OnScriptQuestion(Simulator simulator, UUID taskID, UUID itemID, string objectName, string objectOwner, ScriptPermission questions) { OnEvent("On-Script-Question", paramNamesOnScriptQuestion, paramTypesOnScriptQuestion, simulator, taskID, itemID, objectName, objectOwner, questions); }

        static public readonly string[] paramNamesOnLoadURL = new string[] { "objectName", "objectID", "ownerID", "ownerIsGroup", "message", "URL" };
        static public readonly Type[] paramTypesOnLoadURL = new Type[] { typeof(string), typeof(UUID), typeof(UUID), typeof(bool), typeof(string), typeof(string) };

        public virtual void Self_OnLoadURL(string objectName, UUID objectID, UUID ownerID, bool ownerIsGroup, string message, string URL) { OnEvent("On-Load-U-R-L", paramNamesOnLoadURL, paramTypesOnLoadURL, objectName, objectID, ownerID, ownerIsGroup, message, URL); }

        static public readonly string[] paramNamesOnInstantMessage = new string[] { "im", "simulator" };
        static public readonly Type[] paramTypesOnInstantMessage = new Type[] { typeof(InstantMessage), typeof(Simulator) };

        public virtual void Self_OnInstantMessage(InstantMessage im, Simulator simulator) { OnEvent("On-Instant-Message", paramNamesOnInstantMessage, paramTypesOnInstantMessage, im, simulator); }

        static public readonly string[] paramNamesOnTeleport = new string[] { "message", "status", "flags" };
        static public readonly Type[] paramTypesOnTeleport = new Type[] { typeof(string), typeof(TeleportStatus), typeof(TeleportFlags) };

        public virtual void Self_OnTeleport(string message, TeleportStatus status, TeleportFlags flags) { OnEvent("On-Teleport", paramNamesOnTeleport, paramTypesOnTeleport, message, status, flags); }

        static public readonly string[] paramNamesOnBalanceUpdated = new string[] { "balance" };
        static public readonly Type[] paramTypesOnBalanceUpdated = new Type[] { typeof(int) };

        public virtual void Self_OnBalanceUpdated(int balance) 
        {
            OnEvent("On-Balance-Updated", paramNamesOnBalanceUpdated, paramTypesOnBalanceUpdated, balance); 
        }

        static public readonly string[] paramNamesOnMoneyBalanceReplyReceived = new string[] { "transactionID", "transactionSuccess", "balance", "metersCredit", "metersCommitted", "description" };
        static public readonly Type[] paramTypesOnMoneyBalanceReplyReceived = new Type[] { typeof(UUID), typeof(bool), typeof(int), typeof(int), typeof(int), typeof(string) };

        public virtual void Self_OnMoneyBalanceReplyReceived(UUID transactionID, bool transactionSuccess, int balance, int metersCredit, int metersCommitted, string description) { OnEvent("On-Money-Balance-Reply-Received", paramNamesOnMoneyBalanceReplyReceived, paramTypesOnMoneyBalanceReplyReceived, transactionID, transactionSuccess, balance, metersCredit, metersCommitted, description); }

        static public readonly string[] paramNamesOnAgentDataUpdated = new string[] { "firstName", "lastName", "activeGroupID", "groupTitle", "groupPowers", "groupName" };
        static public readonly Type[] paramTypesOnAgentDataUpdated = new Type[] { typeof(string), typeof(string), typeof(UUID), typeof(string), typeof(GroupPowers), typeof(string) };

        public virtual void Self_OnAgentDataUpdated(string firstName, string lastName, UUID activeGroupID, string groupTitle, GroupPowers groupPowers, string groupName) { OnEvent("On-Agent-Data-Updated", paramNamesOnAgentDataUpdated, paramTypesOnAgentDataUpdated, firstName, lastName, activeGroupID, groupTitle, groupPowers, groupName); }

        static public readonly string[] paramNamesOnAnimationsChanged = new string[] { "agentAnimations" };
        static public readonly Type[] paramTypesOnAnimationsChanged = new Type[] { typeof(InternalDictionary<UUID, int>) };

        public virtual void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations) { OnEvent("On-Animations-Changed", paramNamesOnAnimationsChanged, paramTypesOnAnimationsChanged, agentAnimations); }

        static public readonly string[] paramNamesOnMeanCollision = new string[] { "type", "perp", "victim", "magnitude", "time" };
        static public readonly Type[] paramTypesOnMeanCollision = new Type[] { typeof(MeanCollisionType), typeof(UUID), typeof(UUID), typeof(float), typeof(System.DateTime) };

        public virtual void Self_OnMeanCollision(MeanCollisionType type, UUID perp, UUID victim, float magnitude, System.DateTime time) { OnEvent("On-Mean-Collision", paramNamesOnMeanCollision, paramTypesOnMeanCollision, type, perp, victim, magnitude, time); }

        static public readonly string[] paramNamesOnRegionCrossed = new string[] { "oldSim", "newSim" };
        static public readonly Type[] paramTypesOnRegionCrossed = new Type[] { typeof(Simulator), typeof(Simulator) };

        public virtual void Self_OnRegionCrossed(Simulator oldSim, Simulator newSim) { OnEvent("On-Region-Crossed", paramNamesOnRegionCrossed, paramTypesOnRegionCrossed, oldSim, newSim); }

        static public readonly string[] paramNamesOnGroupChatJoin = new string[] { "groupChatSessionID", "sessionName", "tmpSessionID", "success" };
        static public readonly Type[] paramTypesOnGroupChatJoin = new Type[] { typeof(UUID), typeof(string), typeof(UUID), typeof(bool) };

        public virtual void Self_OnGroupChatJoin(UUID groupChatSessionID, string sessionName, UUID tmpSessionID, bool success) { OnEvent("On-Group-Chat-Join", paramNamesOnGroupChatJoin, paramTypesOnGroupChatJoin, groupChatSessionID, sessionName, tmpSessionID, success); }

        static public readonly string[] paramNamesOnGroupChatLeft = new string[] { "groupchatSessionID" };
        static public readonly Type[] paramTypesOnGroupChatLeft = new Type[] { typeof(UUID) };

        public virtual void Self_OnGroupChatLeft(UUID groupchatSessionID) { OnEvent("On-Group-Chat-Left", paramNamesOnGroupChatLeft, paramTypesOnGroupChatLeft, groupchatSessionID); }

        static public readonly string[] paramNamesOnAlertMessage = new string[] { "message" };
        static public readonly Type[] paramTypesOnAlertMessage = new Type[] { typeof(string) };

        public virtual void Self_OnAlertMessage(string message) { OnEvent("On-Alert-Message", paramNamesOnAlertMessage, paramTypesOnAlertMessage, message); }

        static public readonly string[] paramNamesOnScriptControlChange = new string[] { "controls", "pass", "take" };
        static public readonly Type[] paramTypesOnScriptControlChange = new Type[] { typeof(ScriptControlChange), typeof(bool), typeof(bool) };

        public virtual void Self_OnScriptControlChange(ScriptControlChange controls, bool pass, bool take) { OnEvent("On-Script-Control-Change", paramNamesOnScriptControlChange, paramTypesOnScriptControlChange, controls, pass, take); }

        static public readonly string[] paramNamesOnCameraConstraint = new string[] { "collidePlane" };
        static public readonly Type[] paramTypesOnCameraConstraint = new Type[] { typeof(Vector4) };

        public virtual void Self_OnCameraConstraint(Vector4 collidePlane) { OnEvent("On-Camera-Constraint", paramNamesOnCameraConstraint, paramTypesOnCameraConstraint, collidePlane); }

        static public readonly string[] paramNamesOnScriptSensorReply = new string[] { "requestorID", "groupID", "name", "objectID", "ownerID", "position", "range", "rotation", "type", "velocity" };
        static public readonly Type[] paramTypesOnScriptSensorReply = new Type[] { typeof(UUID), typeof(UUID), typeof(string), typeof(UUID), typeof(UUID), typeof(Vector3), typeof(float), typeof(Quaternion), typeof(ScriptSensorTypeFlags), typeof(Vector3) };

        public virtual void Self_OnScriptSensorReply(UUID requestorID, UUID groupID, string name, UUID objectID, UUID ownerID, Vector3 position, float range, Quaternion rotation, ScriptSensorTypeFlags type, Vector3 velocity) { OnEvent("On-Script-Sensor-Reply", paramNamesOnScriptSensorReply, paramTypesOnScriptSensorReply, requestorID, groupID, name, objectID, ownerID, position, range, rotation, type, velocity); }

        static public readonly string[] paramNamesOnAvatarSitResponse = new string[] { "objectID", "autoPilot", "cameraAtOffset", "cameraEyeOffset", "forceMouselook", "sitPosition", "sitRotation" };
        static public readonly Type[] paramTypesOnAvatarSitResponse = new Type[] { typeof(UUID), typeof(bool), typeof(Vector3), typeof(Vector3), typeof(bool), typeof(Vector3), typeof(Quaternion) };

        public virtual void Self_OnAvatarSitResponse(UUID objectID, bool autoPilot, Vector3 cameraAtOffset, Vector3 cameraEyeOffset, bool forceMouselook, Vector3 sitPosition, Quaternion sitRotation) { OnEvent("On-Avatar-Sit-Response", paramNamesOnAvatarSitResponse, paramTypesOnAvatarSitResponse, objectID, autoPilot, cameraAtOffset, cameraEyeOffset, forceMouselook, sitPosition, sitRotation); }

        static public readonly string[] paramNamesOnChatSessionMemberAdded = new string[] { "sessionID", "agent_key" };
        static public readonly Type[] paramTypesOnChatSessionMemberAdded = new Type[] { typeof(UUID), typeof(UUID) };

        public virtual void Self_OnChatSessionMemberAdded(UUID sessionID, UUID agent_key) { OnEvent("On-Chat-Session-Member-Added", paramNamesOnChatSessionMemberAdded, paramTypesOnChatSessionMemberAdded, sessionID, agent_key); }

        static public readonly string[] paramNamesOnChatSessionMemberLeft = new string[] { "sessionID", "agent_key" };
        static public readonly Type[] paramTypesOnChatSessionMemberLeft = new Type[] { typeof(UUID), typeof(UUID) };

        public virtual void Self_OnChatSessionMemberLeft(UUID sessionID, UUID agent_key) { OnEvent("On-Chat-Session-Member-Left", paramNamesOnChatSessionMemberLeft, paramTypesOnChatSessionMemberLeft, sessionID, agent_key); }

        static public readonly string[] paramNamesOnAvatarAppearance = new string[] { "avatarID", "isTrial", "defaultTexture", "faceTextures", "visualParams" };
        static public readonly Type[] paramTypesOnAvatarAppearance = new Type[] { typeof(UUID), typeof(bool), typeof(Primitive.TextureEntryFace), typeof(Primitive.TextureEntryFace[]), typeof(List<byte>) };

        public virtual void Avatars_OnAvatarAppearance(UUID avatarID, bool isTrial, Primitive.TextureEntryFace defaultTexture, Primitive.TextureEntryFace[] faceTextures, List<byte> visualParams) { OnEvent("On-Avatar-Appearance", paramNamesOnAvatarAppearance, paramTypesOnAvatarAppearance, avatarID, isTrial, defaultTexture, faceTextures, visualParams); }

        static public readonly string[] paramNamesOnAvatarNames = new string[] { "names" };
        static public readonly Type[] paramTypesOnAvatarNames = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarNames(Dictionary<UUID, string> names) { OnEvent("On-Avatar-Names", paramNamesOnAvatarNames, paramTypesOnAvatarNames, names); }

        static public readonly string[] paramNamesOnAvatarInterests = new string[] { "avatarID", "interests" };
        static public readonly Type[] paramTypesOnAvatarInterests = new Type[] { typeof(UUID), typeof(Avatar.Interests) };

        public virtual void Avatars_OnAvatarInterests(UUID avatarID, Avatar.Interests interests) { OnEvent("On-Avatar-Interests", paramNamesOnAvatarInterests, paramTypesOnAvatarInterests, avatarID, interests); }

        static public readonly string[] paramNamesOnAvatarProperties = new string[] { "avatarID", "properties" };
        static public readonly Type[] paramTypesOnAvatarProperties = new Type[] { typeof(UUID), typeof(Avatar.AvatarProperties) };

        public virtual void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties) { OnEvent("On-Avatar-Properties", paramNamesOnAvatarProperties, paramTypesOnAvatarProperties, avatarID, properties); }

        static public readonly string[] paramNamesOnAvatarGroups = new string[] { "avatarID", "avatarGroups" };
        static public readonly Type[] paramTypesOnAvatarGroups = new Type[] { typeof(UUID), typeof(List<AvatarGroup>) };

        public virtual void Avatars_OnAvatarGroups(UUID avatarID, List<AvatarGroup> avatarGroups) { OnEvent("On-Avatar-Groups", paramNamesOnAvatarGroups, paramTypesOnAvatarGroups, avatarID, avatarGroups); }

        static public readonly string[] paramNamesOnAvatarNameSearch = new string[] { "queryID", "avatars" };
        static public readonly Type[] paramTypesOnAvatarNameSearch = new Type[] { typeof(UUID), typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarNameSearch(UUID queryID, Dictionary<UUID, string> avatars) { OnEvent("On-Avatar-Name-Search", paramNamesOnAvatarNameSearch, paramTypesOnAvatarNameSearch, queryID, avatars); }

        static public readonly string[] paramNamesOnPointAt = new string[] { "sourceID", "targetID", "targetPos", "pointType", "duration", "id" };
        static public readonly Type[] paramTypesOnPointAt = new Type[] { typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(PointAtType), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnPointAt(UUID sourceID, UUID targetID, Vector3d targetPos, PointAtType pointType, float duration, UUID id) { OnEvent("On-Point-At", paramNamesOnPointAt, paramTypesOnPointAt, sourceID, targetID, targetPos, pointType, duration, id); }

        static public readonly string[] paramNamesOnLookAt = new string[] { "sourceID", "targetID", "targetPos", "lookType", "duration", "id" };
        static public readonly Type[] paramTypesOnLookAt = new Type[] { typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(LookAtType), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnLookAt(UUID sourceID, UUID targetID, Vector3d targetPos, LookAtType lookType, float duration, UUID id) { OnEvent("On-Look-At", paramNamesOnLookAt, paramTypesOnLookAt, sourceID, targetID, targetPos, lookType, duration, id); }

        static public readonly string[] paramNamesOnEffect = new string[] { "type", "sourceID", "targetID", "targetPos", "duration", "id" };
        static public readonly Type[] paramTypesOnEffect = new Type[] { typeof(EffectType), typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnEffect(EffectType type, UUID sourceID, UUID targetID, Vector3d targetPos, float duration, UUID id) { OnEvent("On-Effect", paramNamesOnEffect, paramTypesOnEffect, type, sourceID, targetID, targetPos, duration, id); }

        static public readonly string[] paramNamesOnAvatarPicks = new string[] { "avatarid", "picks" };
        static public readonly Type[] paramTypesOnAvatarPicks = new Type[] { typeof(UUID), typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarPicks(UUID avatarid, Dictionary<UUID, string> picks) { OnEvent("On-Avatar-Picks", paramNamesOnAvatarPicks, paramTypesOnAvatarPicks, avatarid, picks); }

        static public readonly string[] paramNamesOnPickInfo = new string[] { "pickid", "pick" };
        static public readonly Type[] paramTypesOnPickInfo = new Type[] { typeof(UUID), typeof(ProfilePick) };

        public virtual void Avatars_OnPickInfo(UUID pickid, ProfilePick pick) { OnEvent("On-Pick-Info", paramNamesOnPickInfo, paramTypesOnPickInfo, pickid, pick); }

        static public readonly string[] paramNamesOnFriendNamesReceived = new string[] { "names" };
        static public readonly Type[] paramTypesOnFriendNamesReceived = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Friends_OnFriendNamesReceived(Dictionary<UUID, string> names) { OnEvent("On-Friend-Names-Received", paramNamesOnFriendNamesReceived, paramTypesOnFriendNamesReceived, names); }

        static public readonly string[] paramNamesOnFriendOnline = new string[] { "friend" };
        static public readonly Type[] paramTypesOnFriendOnline = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendOnline(FriendInfo friend) { OnEvent("On-Friend-Online", paramNamesOnFriendOnline, paramTypesOnFriendOnline, friend); }

        static public readonly string[] paramNamesOnFriendOffline = new string[] { "friend" };
        static public readonly Type[] paramTypesOnFriendOffline = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendOffline(FriendInfo friend) { OnEvent("On-Friend-Offline", paramNamesOnFriendOffline, paramTypesOnFriendOffline, friend); }

        static public readonly string[] paramNamesOnFriendRights = new string[] { "friend" };
        static public readonly Type[] paramTypesOnFriendRights = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendRights(FriendInfo friend) { OnEvent("On-Friend-Rights", paramNamesOnFriendRights, paramTypesOnFriendRights, friend); }

        static public readonly string[] paramNamesOnFriendshipOffered = new string[] { "agentID", "agentName", "imSessionID" };
        static public readonly Type[] paramTypesOnFriendshipOffered = new Type[] { typeof(UUID), typeof(string), typeof(UUID) };

        public virtual void Friends_OnFriendshipOffered(UUID agentID, string agentName, UUID imSessionID) { OnEvent("On-Friendship-Offered", paramNamesOnFriendshipOffered, paramTypesOnFriendshipOffered, agentID, agentName, imSessionID); }

        static public readonly string[] paramNamesOnFriendshipResponse = new string[] { "agentID", "agentName", "accepted" };
        static public readonly Type[] paramTypesOnFriendshipResponse = new Type[] { typeof(UUID), typeof(string), typeof(bool) };

        public virtual void Friends_OnFriendshipResponse(UUID agentID, string agentName, bool accepted) { OnEvent("On-Friendship-Response", paramNamesOnFriendshipResponse, paramTypesOnFriendshipResponse, agentID, agentName, accepted); }

        static public readonly string[] paramNamesOnFriendshipTerminated = new string[] { "agentID", "agentName" };
        static public readonly Type[] paramTypesOnFriendshipTerminated = new Type[] { typeof(UUID), typeof(string) };

        public virtual void Friends_OnFriendshipTerminated(UUID agentID, string agentName) { OnEvent("On-Friendship-Terminated", paramNamesOnFriendshipTerminated, paramTypesOnFriendshipTerminated, agentID, agentName); }

        static public readonly string[] paramNamesOnFriendFound = new string[] { "agentID", "regionHandle", "location" };
        static public readonly Type[] paramTypesOnFriendFound = new Type[] { typeof(UUID), typeof(ulong), typeof(Vector3) };

        public virtual void Friends_OnFriendFound(UUID agentID, ulong regionHandle, Vector3 location) { OnEvent("On-Friend-Found", paramNamesOnFriendFound, paramTypesOnFriendFound, agentID, regionHandle, location); }

        static public readonly string[] paramNamesOnCoarseLocationUpdate = new string[] { "sim", "newEntries", "removedEntries" };
        static public readonly Type[] paramTypesOnCoarseLocationUpdate = new Type[] { typeof(Simulator), typeof(List<UUID>), typeof(List<UUID>) };

        public virtual void Grid_OnCoarseLocationUpdate(Simulator sim, List<UUID> newEntries, List<UUID> removedEntries) { OnEvent("On-Coarse-Location-Update", paramNamesOnCoarseLocationUpdate, paramTypesOnCoarseLocationUpdate, sim, newEntries,  removedEntries); }

        static public readonly string[] paramNamesOnGridRegion = new string[] { "region" };
        static public readonly Type[] paramTypesOnGridRegion = new Type[] { typeof(GridRegion) };

        public virtual void Grid_OnGridRegion(GridRegion region) { OnEvent("On-Grid-Region", paramNamesOnGridRegion, paramTypesOnGridRegion, region); }

        static public readonly string[] paramNamesOnGridLayer = new string[] { "layer" };
        static public readonly Type[] paramTypesOnGridLayer = new Type[] { typeof(GridLayer) };

        public virtual void Grid_OnGridLayer(GridLayer layer) { OnEvent("On-Grid-Layer", paramNamesOnGridLayer, paramTypesOnGridLayer, layer); }

        static public readonly string[] paramNamesOnGridItems = new string[] { "type", "items" };
        static public readonly Type[] paramTypesOnGridItems = new Type[] { typeof(GridItemType), typeof(List<GridItem>) };

        public virtual void Grid_OnGridItems(GridItemType type, List<GridItem> items) { OnEvent("On-Grid-Items", paramNamesOnGridItems, paramTypesOnGridItems, type, items); }

        static public readonly string[] paramNamesOnRegionHandleReply = new string[] { "regionID", "regionHandle" };
        static public readonly Type[] paramTypesOnRegionHandleReply = new Type[] { typeof(UUID), typeof(ulong) };

        public virtual void Grid_OnRegionHandleReply(UUID regionID, ulong regionHandle) { OnEvent("On-Region-Handle-Reply", paramNamesOnRegionHandleReply, paramTypesOnRegionHandleReply, regionID, regionHandle); }

        static public readonly string[] paramNamesOnNewPrim = new string[] { "simulator", "prim", "regionHandle", "timeDilation" };
        static public readonly Type[] paramTypesOnNewPrim = new Type[] { typeof(Simulator), typeof(Primitive), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation) { OnEvent("On-New-Prim", paramNamesOnNewPrim, paramTypesOnNewPrim, simulator, prim, regionHandle, timeDilation); }

        static public readonly string[] paramNamesOnNewAttachment = new string[] { "simulator", "prim", "regionHandle", "timeDilation" };
        static public readonly Type[] paramTypesOnNewAttachment = new Type[] { typeof(Simulator), typeof(Primitive), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewAttachment(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation) { OnEvent("On-New-Attachment", paramNamesOnNewAttachment, paramTypesOnNewAttachment, simulator, prim, regionHandle, timeDilation); }

        static public readonly string[] paramNamesOnNewAvatar = new string[] { "simulator", "avatar", "regionHandle", "timeDilation" };
        static public readonly Type[] paramTypesOnNewAvatar = new Type[] { typeof(Simulator), typeof(Avatar), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation) { OnEvent("On-New-Avatar", paramNamesOnNewAvatar, paramTypesOnNewAvatar, simulator, avatar, regionHandle, timeDilation); }

        static public readonly string[] paramNamesOnObjectUpdated = new string[] { "simulator", "update", "regionHandle", "timeDilation" };
        static public readonly Type[] paramTypesOnObjectUpdated = new Type[] { typeof(Simulator), typeof(ObjectUpdate), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation) { OnEvent("On-Object-Updated", paramNamesOnObjectUpdated, paramTypesOnObjectUpdated, simulator, update, regionHandle, timeDilation); }

        static public readonly string[] paramNamesOnAvatarSitChanged = new string[] { "simulator", "avatar", "sittingOn", "oldSeat" };
        static public readonly Type[] paramTypesOnAvatarSitChanged = new Type[] { typeof(Simulator), typeof(Avatar), typeof(uint), typeof(uint) };

        public virtual void Objects_OnAvatarSitChanged(Simulator simulator, Avatar avatar, uint sittingOn, uint oldSeat) { OnEvent("On-Avatar-Sit-Changed", paramNamesOnAvatarSitChanged, paramTypesOnAvatarSitChanged, simulator, avatar, sittingOn, oldSeat); }

        static public readonly string[] paramNamesOnObjectKilled = new string[] { "simulator", "objectID" };
        static public readonly Type[] paramTypesOnObjectKilled = new Type[] { typeof(Simulator), typeof(uint) };

        public virtual void Objects_OnObjectKilled(Simulator simulator, uint objectID) { OnEvent("On-Object-Killed", paramNamesOnObjectKilled, paramTypesOnObjectKilled, simulator, objectID); }

        static public readonly string[] paramNamesOnObjectProperties = new string[] { "simulator", "props" };
        static public readonly Type[] paramTypesOnObjectProperties = new Type[] { typeof(Simulator), typeof(Primitive.ObjectProperties) };

        public virtual void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props) { OnEvent("On-Object-Properties", paramNamesOnObjectProperties, paramTypesOnObjectProperties, simulator, props); }

        static public readonly string[] paramNamesOnObjectPropertiesFamily = new string[] { "simulator", "props", "type" };
        static public readonly Type[] paramTypesOnObjectPropertiesFamily = new Type[] { typeof(Simulator), typeof(Primitive.ObjectProperties), typeof(ReportType) };

        public virtual void Objects_OnObjectPropertiesFamily(Simulator simulator, Primitive.ObjectProperties props, ReportType type) { OnEvent("On-Object-Properties-Family", paramNamesOnObjectPropertiesFamily, paramTypesOnObjectPropertiesFamily, simulator, props, type); }

        static public readonly string[] paramNamesOnCurrentGroups = new string[] { "groups" };
        static public readonly Type[] paramTypesOnCurrentGroups = new Type[] { typeof(Dictionary<UUID, Group>) };

        public virtual void Groups_OnCurrentGroups(Dictionary<UUID, Group> groups) { OnEvent("On-Current-Groups", paramNamesOnCurrentGroups, paramTypesOnCurrentGroups, groups); }

        static public readonly string[] paramNamesOnGroupNames = new string[] { "groupNames" };
        static public readonly Type[] paramTypesOnGroupNames = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Groups_OnGroupNames(Dictionary<UUID, string> groupNames) { OnEvent("On-Group-Names", paramNamesOnGroupNames, paramTypesOnGroupNames, groupNames); }

        static public readonly string[] paramNamesOnGroupProfile = new string[] { "group" };
        static public readonly Type[] paramTypesOnGroupProfile = new Type[] { typeof(Group) };

        public virtual void Groups_OnGroupProfile(Group group) { OnEvent("On-Group-Profile", paramNamesOnGroupProfile, paramTypesOnGroupProfile, group); }

        public static readonly string[] paramNamesOnGroupMembers = new string[] { "requestID", "groupID", "members" };
        static public readonly Type[] paramTypesOnGroupMembers = new Type[] { typeof(UUID), typeof(UUID), typeof(Dictionary<UUID, GroupMember>) };

        public virtual void Groups_OnGroupMembers(UUID requestID, UUID groupID, Dictionary<UUID, GroupMember> members)
        {
            OnEvent("On-Group-Members", paramNamesOnGroupMembers, paramTypesOnGroupMembers, 
                requestID, groupID,  members);
        }

        static public readonly string[] paramNamesOnGroupRoles = new string[] {"requestID", "groupID", "roles" };
        static public readonly Type[] paramTypesOnGroupRoles = new Type[] { typeof(UUID), typeof(UUID), typeof(Dictionary<UUID, GroupRole>) };

        public virtual void Groups_OnGroupRoles(UUID requestID, UUID groupID, Dictionary<UUID, GroupRole> roles) { OnEvent("On-Group-Roles", paramNamesOnGroupRoles, paramTypesOnGroupRoles, requestID, groupID, roles); }

        static public readonly string[] paramNamesOnGroupRolesMembers = new string[] { "requestID", "groupID", "rolesMembers" };
        static public readonly Type[] paramTypesOnGroupRolesMembers = new Type[] { typeof(UUID), typeof(UUID),  typeof(List<KeyValuePair<UUID, UUID>>) };

        public virtual void Groups_OnGroupRolesMembers(UUID requestID, UUID groupID, List<KeyValuePair<UUID, UUID>> rolesMembers) 
        { OnEvent("On-Group-Roles-Members", paramNamesOnGroupRolesMembers, paramTypesOnGroupRolesMembers,requestID, groupID, rolesMembers); }

        static public readonly string[] paramNamesOnGroupTitles = new string[] { "requestID", "groupID", "titles" };
        static public readonly Type[] paramTypesOnGroupTitles = new Type[] { typeof(UUID), typeof(UUID), typeof(Dictionary<UUID, GroupTitle>) };

        public virtual void Groups_OnGroupTitles(UUID requestID, UUID groupID, Dictionary<UUID, GroupTitle> titles) { OnEvent("On-Group-Titles", paramNamesOnGroupTitles, paramTypesOnGroupTitles, requestID, groupID, titles); }

        static public readonly string[] paramNamesOnGroupAccountSummary = new string[] { "groupID", "summary" };
        static public readonly Type[] paramTypesOnGroupAccountSummary = new Type[] { typeof(UUID), typeof(GroupAccountSummary) };

        public virtual void Groups_OnGroupAccountSummary(UUID groupID, GroupAccountSummary summary) { OnEvent("On-Group-Account-Summary", paramNamesOnGroupAccountSummary, paramTypesOnGroupAccountSummary, groupID, summary); }

        static public readonly string[] paramNamesOnGroupCreated = new string[] { "groupID", "success", "message" };
        static public readonly Type[] paramTypesOnGroupCreated = new Type[] { typeof(UUID), typeof(bool), typeof(string) };

        public virtual void Groups_OnGroupCreated(UUID groupID, bool success, string message) { OnEvent("On-Group-Created", paramNamesOnGroupCreated, paramTypesOnGroupCreated, groupID, success, message); }

        static public readonly string[] paramNamesOnGroupJoined = new string[] { "groupID", "success" };
        static public readonly Type[] paramTypesOnGroupJoined = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupJoined(UUID groupID, bool success) { OnEvent("On-Group-Joined", paramNamesOnGroupJoined, paramTypesOnGroupJoined, groupID, success); }

        static public readonly string[] paramNamesOnGroupLeft = new string[] { "groupID", "success" };
        static public readonly Type[] paramTypesOnGroupLeft = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupLeft(UUID groupID, bool success) { OnEvent("On-Group-Left", paramNamesOnGroupLeft, paramTypesOnGroupLeft, groupID, success); }

        static public readonly string[] paramNamesOnGroupDropped = new string[] { "groupID" };
        static public readonly Type[] paramTypesOnGroupDropped = new Type[] { typeof(UUID) };

        public virtual void Groups_OnGroupDropped(UUID groupID) { OnEvent("On-Group-Dropped", paramNamesOnGroupDropped, paramTypesOnGroupDropped, groupID); }

        static public readonly string[] paramNamesOnGroupMemberEjected = new string[] { "groupID", "success" };
        static public readonly Type[] paramTypesOnGroupMemberEjected = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupMemberEjected(UUID groupID, bool success) { OnEvent("On-Group-Member-Ejected", paramNamesOnGroupMemberEjected, paramTypesOnGroupMemberEjected, groupID, success); }

        static public readonly string[] paramNamesOnGroupNoticesList = new string[] { "groupID", "notice" };
        static public readonly Type[] paramTypesOnGroupNoticesList = new Type[] { typeof(UUID), typeof(GroupNoticeList) };

        public virtual void Groups_OnGroupNoticesList(UUID groupID, GroupNoticeList notice) { OnEvent("On-Group-Notices-List", paramNamesOnGroupNoticesList, paramTypesOnGroupNoticesList, groupID, notice); }

        static public readonly string[] paramNamesOnAssetReceived = new string[] { "transfer", "asset" };
        static public readonly Type[] paramTypesOnAssetReceived = new Type[] { typeof(AssetDownload), typeof(Asset) };

        public virtual void Assets_OnAssetReceived(AssetDownload transfer, Asset asset) { OnEvent("On-Asset-Received", paramNamesOnAssetReceived, paramTypesOnAssetReceived, transfer, asset); }

        static public readonly string[] paramNamesOnXferReceived = new string[] { "xfer" };
        static public readonly Type[] paramTypesOnXferReceived = new Type[] { typeof(XferDownload) };

        public virtual void Assets_OnXferReceived(XferDownload xfer) { OnEvent("On-Xfer-Received", paramNamesOnXferReceived, paramTypesOnXferReceived, xfer); }

        static public readonly string[] paramNamesOnImageReceived = new string[] { "image", "asset" };
        static public readonly Type[] paramTypesOnImageReceived = new Type[] { typeof(ImageDownload), typeof(AssetTexture) };

        public virtual void Assets_OnImageReceived(ImageDownload image, AssetTexture asset) { OnEvent("On-Image-Received", paramNamesOnImageReceived, paramTypesOnImageReceived, image, asset); }

        static public readonly string[] paramNamesOnImageReceiveProgress = new string[] { "image", "recieved", "total" };
        static public readonly Type[] paramTypesOnImageReceiveProgress = new Type[] { typeof(UUID),  typeof(int), typeof(int) };

        public virtual void Assets_OnImageReceiveProgress(UUID image, int recieved, int total) { OnEvent("On-Image-Receive-Progress", paramNamesOnImageReceiveProgress, paramTypesOnImageReceiveProgress, image,  recieved, total); }

        static public readonly string[] paramNamesOnAssetUploaded = new string[] { "upload" };
        static public readonly Type[] paramTypesOnAssetUploaded = new Type[] { typeof(AssetUpload) };

        public virtual void Assets_OnAssetUploaded(AssetUpload upload) { OnEvent("On-Asset-Uploaded", paramNamesOnAssetUploaded, paramTypesOnAssetUploaded, upload); }

        static public readonly string[] paramNamesOnUploadProgress = new string[] { "upload" };
        static public readonly Type[] paramTypesOnUploadProgress = new Type[] { typeof(AssetUpload) };

        public virtual void Assets_OnUploadProgress(AssetUpload upload) { OnEvent("On-Upload-Progress", paramNamesOnUploadProgress, paramTypesOnUploadProgress, upload); }

        static public readonly string[] paramNamesOnAgentWearables = new string[] { };
        static public readonly Type[] paramTypesOnAgentWearables = new Type[] { };

        public virtual void Appearance_OnAgentWearables() { OnEvent("On-Agent-Wearables", paramNamesOnAgentWearables, paramTypesOnAgentWearables); }

        static public readonly string[] paramNamesOnAppearanceUpdated = new string[] { "te" };
        static public readonly Type[] paramTypesOnAppearanceUpdated = new Type[] { typeof(Primitive.TextureEntry) };

        public virtual void Appearance_OnAppearanceUpdated(Primitive.TextureEntry te) { OnEvent("On-Appearance-Updated", paramNamesOnAppearanceUpdated, paramTypesOnAppearanceUpdated, te); }

        static public readonly string[] paramNamesOnItemReceived = new string[] { "item" };
        static public readonly Type[] paramTypesOnItemReceived = new Type[] { typeof(InventoryItem) };

        public virtual void Inventory_OnItemReceived(InventoryItem item) { OnEvent("On-Item-Received", paramNamesOnItemReceived, paramTypesOnItemReceived, item); }

        static public readonly string[] paramNamesOnFolderUpdated = new string[] { "folderID" };
        static public readonly Type[] paramTypesOnFolderUpdated = new Type[] { typeof(UUID) };

        public virtual void Inventory_OnFolderUpdated(UUID folderID) { OnEvent("On-Folder-Updated", paramNamesOnFolderUpdated, paramTypesOnFolderUpdated, folderID); }

        static public readonly string[] paramNamesOnObjectOffered = new string[] { "offerDetails", "type", "objectID", "fromTask" };
        static public readonly Type[] paramTypesOnObjectOffered = new Type[] { typeof(InstantMessage), typeof(AssetType), typeof(UUID), typeof(bool) };

        public bool Inventory_OnObjectOffered(InstantMessage offerDetails, AssetType type, UUID objectID, bool fromTask)
        { return BooleanOnEvent("On-Object-Offered", paramNamesOnObjectOffered, paramTypesOnObjectOffered, offerDetails, type, objectID, fromTask); }

        static public readonly string[] paramNamesOnFindObjectByPath = new string[] { "path", "inventoryObjectID" };
        static public readonly Type[] paramTypesOnFindObjectByPath = new Type[] { typeof(string), typeof(UUID) };

        public virtual void Inventory_OnFindObjectByPath(string path, UUID inventoryObjectID) { OnEvent("On-Find-Object-By-Path", paramNamesOnFindObjectByPath, paramTypesOnFindObjectByPath, path, inventoryObjectID); }

        static public readonly string[] paramNamesOnTaskItemReceived = new string[] { "itemID", "folderID", "creatorID", "assetID", "type" };
        static public readonly Type[] paramTypesOnTaskItemReceived = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(UUID), typeof(InventoryType) };

        public virtual void Inventory_OnTaskItemReceived(UUID itemID, UUID folderID, UUID creatorID, UUID assetID, InventoryType type) { OnEvent("On-Task-Item-Received", paramNamesOnTaskItemReceived, paramTypesOnTaskItemReceived, itemID, folderID, creatorID, assetID, type); }

        static public readonly string[] paramNamesOnTaskInventoryReply = new string[] { "itemID", "serial", "assetFilename" };
        static public readonly Type[] paramTypesOnTaskInventoryReply = new Type[] { typeof(UUID), typeof(short), typeof(string) };

        public virtual void Inventory_OnTaskInventoryReply(UUID itemID, short serial, string assetFilename) { OnEvent("On-Task-Inventory-Reply", paramNamesOnTaskInventoryReply, paramTypesOnTaskInventoryReply, itemID, serial, assetFilename); }

        static public readonly string[] paramNamesOnClassifiedReply = new string[] { "classifieds" };
        static public readonly Type[] paramTypesOnClassifiedReply = new Type[] { typeof(List<DirectoryManager.Classified>) };

        public virtual void Directory_OnClassifiedReply(List<DirectoryManager.Classified> classifieds) { OnEvent("On-Classified-Reply", paramNamesOnClassifiedReply, paramTypesOnClassifiedReply, classifieds); }

        static public readonly string[] paramNamesOnDirLandReply = new string[] { "dirParcels" };
        static public readonly Type[] paramTypesOnDirLandReply = new Type[] { typeof(List<DirectoryManager.DirectoryParcel>) };

        public virtual void Directory_OnDirLandReply(List<DirectoryManager.DirectoryParcel> dirParcels) { OnEvent("On-Dir-Land-Reply", paramNamesOnDirLandReply, paramTypesOnDirLandReply, dirParcels); }

        static public readonly string[] paramNamesOnDirPeopleReply = new string[] { "queryID", "matchedPeople" };
        static public readonly Type[] paramTypesOnDirPeopleReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.AgentSearchData>) };

        public virtual void Directory_OnDirPeopleReply(UUID queryID, List<DirectoryManager.AgentSearchData> matchedPeople) { OnEvent("On-Dir-People-Reply", paramNamesOnDirPeopleReply, paramTypesOnDirPeopleReply, queryID, matchedPeople); }

        static public readonly string[] paramNamesOnDirGroupsReply = new string[] { "queryID", "matchedGroups" };
        static public readonly Type[] paramTypesOnDirGroupsReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.GroupSearchData>) };

        public virtual void Directory_OnDirGroupsReply(UUID queryID, List<DirectoryManager.GroupSearchData> matchedGroups) { OnEvent("On-Dir-Groups-Reply", paramNamesOnDirGroupsReply, paramTypesOnDirGroupsReply, queryID, matchedGroups); }

        static public readonly string[] paramNamesOnPlacesReply = new string[] { "queryID", "matchedPlaces" };
        static public readonly Type[] paramTypesOnPlacesReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.PlacesSearchData>) };

        public virtual void Directory_OnPlacesReply(UUID queryID, List<DirectoryManager.PlacesSearchData> matchedPlaces) { OnEvent("On-Places-Reply", paramNamesOnPlacesReply, paramTypesOnPlacesReply, queryID, matchedPlaces); }

        static public readonly string[] paramNamesOnEventsReply = new string[] { "queryID", "matchedEvents" };
        static public readonly Type[] paramTypesOnEventsReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.EventsSearchData>) };

        public virtual void Directory_OnEventsReply(UUID queryID, List<DirectoryManager.EventsSearchData> matchedEvents) { OnEvent("On-Events-Reply", paramNamesOnEventsReply, paramTypesOnEventsReply, queryID, matchedEvents); }

        static public readonly string[] paramNamesOnEventInfo = new string[] { "matchedEvent" };
        static public readonly Type[] paramTypesOnEventInfo = new Type[] { typeof(DirectoryManager.EventInfo) };

        public virtual void Directory_OnEventInfo(DirectoryManager.EventInfo matchedEvent) { OnEvent("On-Event-Info", paramNamesOnEventInfo, paramTypesOnEventInfo, matchedEvent); }

        static public readonly string[] paramNamesOnLandPatch = new string[] { "simulator", "x", "y", "width", "data" };
        static public readonly Type[] paramTypesOnLandPatch = new Type[] { typeof(Simulator), typeof(int), typeof(int), typeof(int), typeof(float[]) };

        public virtual void Terrain_OnLandPatch(Simulator simulator, int x, int y, int width, float[] data) { OnEvent("On-Land-Patch", paramNamesOnLandPatch, paramTypesOnLandPatch, simulator, x, y, width, data); }

        static public readonly string[] paramNamesOnAttachSound = new string[] { "soundID", "ownerID", "objectID", "gain", "flags" };
        static public readonly Type[] paramTypesOnAttachSound = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(float), typeof(byte) };

        public virtual void Sound_OnAttachSound(UUID soundID, UUID ownerID, UUID objectID, float gain, SoundFlags flags) { OnEvent("On-Attach-Sound", paramNamesOnAttachSound, paramTypesOnAttachSound, soundID, ownerID, objectID, gain, flags); }

        static public readonly string[] paramNamesOnAttachSoundGainChange = new string[] { "objectID", "gain" };
        static public readonly Type[] paramTypesOnAttachSoundGainChange = new Type[] { typeof(UUID), typeof(float) };

        public virtual void Sound_OnAttachSoundGainChange(UUID objectID, float gain) { OnEvent("On-Attach-Sound-Gain-Change", paramNamesOnAttachSoundGainChange, paramTypesOnAttachSoundGainChange, objectID, gain); }

        static public readonly string[] paramNamesOnSoundTrigger = new string[] { "soundID", "ownerID", "objectID", "parentID", "gain", "regionHandle", "position" };
        static public readonly Type[] paramTypesOnSoundTrigger = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(UUID), typeof(float), typeof(ulong), typeof(Vector3) };

        public virtual void Sound_OnSoundTrigger(UUID soundID, UUID ownerID, UUID objectID, UUID parentID, float gain, ulong regionHandle, Vector3 position) { OnEvent("On-Sound-Trigger", paramNamesOnSoundTrigger, paramTypesOnSoundTrigger, soundID, ownerID, objectID, parentID, gain, regionHandle, position); }

        static public readonly string[] paramNamesOnPreloadSound = new string[] { "soundID", "ownerID", "objectID" };
        static public readonly Type[] paramTypesOnPreloadSound = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID) };

        public virtual void Sound_OnPreloadSound(UUID soundID, UUID ownerID, UUID objectID) { OnEvent("On-Preload-Sound", paramNamesOnPreloadSound, paramTypesOnPreloadSound, soundID, ownerID, objectID); }

        public virtual void RegisterAll()
        {
            client.Network.OnLogin += Network_OnLogin;
            client.Network.OnConnected += Network_OnConnected;
            client.Network.OnLogoutReply += Network_OnLogoutReply;
            client.Network.OnSimConnecting += Network_OnSimConnecting;
            client.Network.OnSimConnected += Network_OnSimConnected;
            client.Network.OnSimDisconnected += Network_OnSimDisconnected;
            client.Network.OnDisconnected += Network_OnDisconnected;
            client.Network.OnCurrentSimChanged += Network_OnCurrentSimChanged;
            client.Network.OnEventQueueRunning += Network_OnEventQueueRunning;
            client.Parcels.OnParcelDwell += Parcels_OnParcelDwell;
            client.Parcels.OnParcelInfo += Parcels_OnParcelInfo;
            client.Parcels.OnParcelProperties += Parcels_OnParcelProperties;
            client.Parcels.OnAccessListReply += Parcels_OnAccessListReply;
            client.Parcels.OnPrimOwnersListReply += Parcels_OnPrimOwnersListReply;
            client.Parcels.OnSimParcelsDownloaded += Parcels_OnSimParcelsDownloaded;
            client.Parcels.OnParcelSelectedObjects += Parcels_OnParcelSelectedObjects;
            client.Parcels.OnParcelMediaUpdate += Parcels_OnParcelMediaUpdate;
            client.Self.OnChat += Self_OnChat;
            client.Self.OnScriptDialog += Self_OnScriptDialog;
            client.Self.OnScriptQuestion += Self_OnScriptQuestion;
            client.Self.OnLoadURL += Self_OnLoadURL;
            client.Self.OnInstantMessage += Self_OnInstantMessage;
            client.Self.OnTeleport += Self_OnTeleport;
            client.Self.OnBalanceUpdated += Self_OnBalanceUpdated;
            client.Self.OnMoneyBalanceReplyReceived += Self_OnMoneyBalanceReplyReceived;
            client.Self.OnAgentDataUpdated += Self_OnAgentDataUpdated;
            client.Self.OnAnimationsChanged += Self_OnAnimationsChanged;
            client.Self.OnMeanCollision += Self_OnMeanCollision;
            client.Self.OnRegionCrossed += Self_OnRegionCrossed;
            client.Self.OnGroupChatJoin += Self_OnGroupChatJoin;
            client.Self.OnGroupChatLeft += Self_OnGroupChatLeft;
            client.Self.OnAlertMessage += Self_OnAlertMessage;
            client.Self.OnScriptControlChange += Self_OnScriptControlChange;
            client.Self.OnCameraConstraint += Self_OnCameraConstraint;
            client.Self.OnScriptSensorReply += Self_OnScriptSensorReply;
            client.Self.OnAvatarSitResponse += Self_OnAvatarSitResponse;
            client.Self.OnChatSessionMemberAdded += Self_OnChatSessionMemberAdded;
            client.Self.OnChatSessionMemberLeft += Self_OnChatSessionMemberLeft;
            client.Avatars.OnAvatarAppearance += Avatars_OnAvatarAppearance;
            // not in older LibOMV
            client.Avatars.OnAvatarAnimation += Avatars_OnAvatarAnimation;

            client.Avatars.OnAvatarNames += Avatars_OnAvatarNames;
            client.Avatars.OnAvatarInterests += Avatars_OnAvatarInterests;
            client.Avatars.OnAvatarProperties += Avatars_OnAvatarProperties;
            client.Avatars.OnAvatarGroups += Avatars_OnAvatarGroups;
            client.Avatars.OnAvatarNameSearch += Avatars_OnAvatarNameSearch;
            client.Avatars.OnPointAt += Avatars_OnPointAt;
            client.Avatars.OnLookAt += Avatars_OnLookAt;
            client.Avatars.OnEffect += Avatars_OnEffect;
            client.Avatars.OnAvatarPicks += Avatars_OnAvatarPicks;
            client.Avatars.OnPickInfo += Avatars_OnPickInfo;
            client.Friends.OnFriendNamesReceived += Friends_OnFriendNamesReceived;
            client.Friends.OnFriendOnline += Friends_OnFriendOnline;
            client.Friends.OnFriendOffline += Friends_OnFriendOffline;
            client.Friends.OnFriendRights += Friends_OnFriendRights;
            client.Friends.OnFriendshipOffered += Friends_OnFriendshipOffered;
            client.Friends.OnFriendshipResponse += Friends_OnFriendshipResponse;
            client.Friends.OnFriendshipTerminated += Friends_OnFriendshipTerminated;
            client.Friends.OnFriendFound += Friends_OnFriendFound;
            client.Grid.OnCoarseLocationUpdate += Grid_OnCoarseLocationUpdate;
            client.Grid.OnGridRegion += Grid_OnGridRegion;
            client.Grid.OnGridLayer += Grid_OnGridLayer;
            client.Grid.OnGridItems += Grid_OnGridItems;
            client.Grid.OnRegionHandleReply += Grid_OnRegionHandleReply;
            client.Objects.OnNewPrim += Objects_OnNewPrim;
            client.Objects.OnNewAttachment += Objects_OnNewAttachment;
            client.Objects.OnNewAvatar += Objects_OnNewAvatar;
            //note we dont use client.Objects.OnObjectUpdated += Objects_OnObjectUpdated;
            client.Objects.OnAvatarSitChanged += Objects_OnAvatarSitChanged;
            client.Objects.OnObjectKilled += Objects_OnObjectKilled;
           // client.Objects.OnObjectProperties += Objects_OnObjectProperties;
            client.Objects.OnObjectPropertiesFamily += Objects_OnObjectPropertiesFamily;
            client.Groups.OnCurrentGroups += Groups_OnCurrentGroups;
            client.Groups.OnGroupNames += Groups_OnGroupNames;
            client.Groups.OnGroupProfile += Groups_OnGroupProfile;
            client.Groups.OnGroupMembers += Groups_OnGroupMembers;
            client.Groups.OnGroupRoles += Groups_OnGroupRoles;
            client.Groups.OnGroupRolesMembers += Groups_OnGroupRolesMembers;
            client.Groups.OnGroupTitles += Groups_OnGroupTitles;
            client.Groups.OnGroupAccountSummary += Groups_OnGroupAccountSummary;
            client.Groups.OnGroupCreated += Groups_OnGroupCreated;
            client.Groups.OnGroupJoined += Groups_OnGroupJoined;
            client.Groups.OnGroupLeft += Groups_OnGroupLeft;
            client.Groups.OnGroupDropped += Groups_OnGroupDropped;
            client.Groups.OnGroupMemberEjected += Groups_OnGroupMemberEjected;
            client.Groups.OnGroupNoticesList += Groups_OnGroupNoticesList;
            //client.Assets.OnAssetReceived += Assets_OnAssetReceived;
            client.Assets.OnXferReceived += Assets_OnXferReceived;
            if (ClientManager.DownloadTextures)
            {
               //todo client.Assets.OnImageReceived += Assets_OnImageReceived;
                client.Assets.OnImageRecieveProgress += Assets_OnImageReceiveProgress;
            }
            client.Assets.OnAssetUploaded += Assets_OnAssetUploaded;
            client.Assets.OnUploadProgress += Assets_OnUploadProgress;
            client.Appearance.OnAgentWearables += Appearance_OnAgentWearables;
            //client.Appearance.OnAppearanceUpdated += Appearance_OnAppearanceUpdated;
            client.Inventory.OnItemReceived += Inventory_OnItemReceived;
            client.Inventory.OnFolderUpdated += Inventory_OnFolderUpdated;
            client.Inventory.OnObjectOffered += Inventory_OnObjectOffered;
            client.Inventory.OnFindObjectByPath += Inventory_OnFindObjectByPath;
            client.Inventory.OnTaskItemReceived += Inventory_OnTaskItemReceived;
            client.Inventory.OnTaskInventoryReply += Inventory_OnTaskInventoryReply;
            client.Directory.OnClassifiedReply += Directory_OnClassifiedReply;
            client.Directory.OnDirLandReply += Directory_OnDirLandReply;
            client.Directory.OnDirPeopleReply += Directory_OnDirPeopleReply;
            client.Directory.OnDirGroupsReply += Directory_OnDirGroupsReply;
            client.Directory.OnPlacesReply += Directory_OnPlacesReply;
            client.Directory.OnEventsReply += Directory_OnEventsReply;
            client.Directory.OnEventInfo += Directory_OnEventInfo;
            client.Terrain.OnLandPatch += Terrain_OnLandPatch;
            client.Sound.OnAttachSound += Sound_OnAttachSound;
            client.Sound.OnAttachSoundGainChange += Sound_OnAttachSoundGainChange;
            client.Sound.OnSoundTrigger += Sound_OnSoundTrigger;
            client.Sound.OnPreloadSound += Sound_OnPreloadSound;
        }

        public virtual void UnregisterAll()
        {
            client.Network.OnLogin -= Network_OnLogin;
            client.Network.OnConnected -= Network_OnConnected;
            client.Network.OnLogoutReply -= Network_OnLogoutReply;
            client.Network.OnSimConnecting -= Network_OnSimConnecting;
            client.Network.OnSimConnected -= Network_OnSimConnected;
            client.Network.OnSimDisconnected -= Network_OnSimDisconnected;
            client.Network.OnDisconnected -= Network_OnDisconnected;
            client.Network.OnCurrentSimChanged -= Network_OnCurrentSimChanged;
            client.Network.OnEventQueueRunning -= Network_OnEventQueueRunning;
            client.Parcels.OnParcelDwell -= Parcels_OnParcelDwell;
            client.Parcels.OnParcelInfo -= Parcels_OnParcelInfo;
            client.Parcels.OnParcelProperties -= Parcels_OnParcelProperties;
            client.Parcels.OnAccessListReply -= Parcels_OnAccessListReply;
            client.Parcels.OnPrimOwnersListReply -= Parcels_OnPrimOwnersListReply;
            client.Parcels.OnSimParcelsDownloaded -= Parcels_OnSimParcelsDownloaded;
            client.Parcels.OnParcelSelectedObjects -= Parcels_OnParcelSelectedObjects;
            client.Parcels.OnParcelMediaUpdate -= Parcels_OnParcelMediaUpdate;
            client.Self.OnChat -= Self_OnChat;
            client.Self.OnScriptDialog -= Self_OnScriptDialog;
            client.Self.OnScriptQuestion -= Self_OnScriptQuestion;
            client.Self.OnLoadURL -= Self_OnLoadURL;
            client.Self.OnInstantMessage -= Self_OnInstantMessage;
            client.Self.OnTeleport -= Self_OnTeleport;
            client.Self.OnBalanceUpdated -= Self_OnBalanceUpdated;
            client.Self.OnMoneyBalanceReplyReceived -= Self_OnMoneyBalanceReplyReceived;
            client.Self.OnAgentDataUpdated -= Self_OnAgentDataUpdated;
            client.Self.OnAnimationsChanged -= Self_OnAnimationsChanged;
            client.Self.OnMeanCollision -= Self_OnMeanCollision;
            client.Self.OnRegionCrossed -= Self_OnRegionCrossed;
            client.Self.OnGroupChatJoin -= Self_OnGroupChatJoin;
            client.Self.OnGroupChatLeft -= Self_OnGroupChatLeft;
            client.Self.OnAlertMessage -= Self_OnAlertMessage;
            client.Self.OnScriptControlChange -= Self_OnScriptControlChange;
            client.Self.OnCameraConstraint -= Self_OnCameraConstraint;
            client.Self.OnScriptSensorReply -= Self_OnScriptSensorReply;
            client.Self.OnAvatarSitResponse -= Self_OnAvatarSitResponse;
            client.Self.OnChatSessionMemberAdded -= Self_OnChatSessionMemberAdded;
            client.Self.OnChatSessionMemberLeft -= Self_OnChatSessionMemberLeft;
            client.Avatars.OnAvatarAppearance -= Avatars_OnAvatarAppearance;
            //client.Avatars.OnAvatarAnimation -= Avatars_OnAvatarAnimation;

            client.Avatars.OnAvatarNames -= Avatars_OnAvatarNames;
            client.Avatars.OnAvatarInterests -= Avatars_OnAvatarInterests;
            client.Avatars.OnAvatarProperties -= Avatars_OnAvatarProperties;
            client.Avatars.OnAvatarGroups -= Avatars_OnAvatarGroups;
            client.Avatars.OnAvatarNameSearch -= Avatars_OnAvatarNameSearch;
            client.Avatars.OnPointAt -= Avatars_OnPointAt;
            client.Avatars.OnLookAt -= Avatars_OnLookAt;
            client.Avatars.OnEffect -= Avatars_OnEffect;
            client.Avatars.OnAvatarPicks -= Avatars_OnAvatarPicks;
            client.Avatars.OnPickInfo -= Avatars_OnPickInfo;
            client.Friends.OnFriendNamesReceived -= Friends_OnFriendNamesReceived;
            client.Friends.OnFriendOnline -= Friends_OnFriendOnline;
            client.Friends.OnFriendOffline -= Friends_OnFriendOffline;
            client.Friends.OnFriendRights -= Friends_OnFriendRights;
            client.Friends.OnFriendshipOffered -= Friends_OnFriendshipOffered;
            client.Friends.OnFriendshipResponse -= Friends_OnFriendshipResponse;
            client.Friends.OnFriendshipTerminated -= Friends_OnFriendshipTerminated;
            client.Friends.OnFriendFound -= Friends_OnFriendFound;
            client.Grid.OnCoarseLocationUpdate -= Grid_OnCoarseLocationUpdate;
            client.Grid.OnGridRegion -= Grid_OnGridRegion;
            client.Grid.OnGridLayer -= Grid_OnGridLayer;
            client.Grid.OnGridItems -= Grid_OnGridItems;
            client.Grid.OnRegionHandleReply -= Grid_OnRegionHandleReply;
            client.Objects.OnNewPrim -= Objects_OnNewPrim;
            client.Objects.OnNewAttachment -= Objects_OnNewAttachment;
            client.Objects.OnNewAvatar -= Objects_OnNewAvatar;
            client.Objects.OnObjectUpdated -= Objects_OnObjectUpdated;
            client.Objects.OnAvatarSitChanged -= Objects_OnAvatarSitChanged;
            client.Objects.OnObjectKilled -= Objects_OnObjectKilled;
            client.Objects.OnObjectProperties -= Objects_OnObjectProperties;
            client.Objects.OnObjectPropertiesFamily -= Objects_OnObjectPropertiesFamily;
            client.Groups.OnCurrentGroups -= Groups_OnCurrentGroups;
            client.Groups.OnGroupNames -= Groups_OnGroupNames;
            client.Groups.OnGroupProfile -= Groups_OnGroupProfile;
            client.Groups.OnGroupMembers -= Groups_OnGroupMembers;
            client.Groups.OnGroupRoles -= Groups_OnGroupRoles;
            client.Groups.OnGroupRolesMembers -= Groups_OnGroupRolesMembers;
            client.Groups.OnGroupTitles -= Groups_OnGroupTitles;
            client.Groups.OnGroupAccountSummary -= Groups_OnGroupAccountSummary;
            client.Groups.OnGroupCreated -= Groups_OnGroupCreated;
            client.Groups.OnGroupJoined -= Groups_OnGroupJoined;
            client.Groups.OnGroupLeft -= Groups_OnGroupLeft;
            client.Groups.OnGroupDropped -= Groups_OnGroupDropped;
            client.Groups.OnGroupMemberEjected -= Groups_OnGroupMemberEjected;
            client.Groups.OnGroupNoticesList -= Groups_OnGroupNoticesList;
           // client.Assets.OnAssetReceived -= Assets_OnAssetReceived;
            client.Assets.OnXferReceived -= Assets_OnXferReceived;
            //todo client.Assets.OnImageReceived -= Assets_OnImageReceived;
            client.Assets.OnImageRecieveProgress -= Assets_OnImageReceiveProgress;
            client.Assets.OnAssetUploaded -= Assets_OnAssetUploaded;
            client.Assets.OnUploadProgress -= Assets_OnUploadProgress;
            client.Appearance.OnAgentWearables -= Appearance_OnAgentWearables;
            //client.Appearance.OnAppearanceUpdated -= Appearance_OnAppearanceUpdated;
            client.Inventory.OnItemReceived -= Inventory_OnItemReceived;
            client.Inventory.OnFolderUpdated -= Inventory_OnFolderUpdated;
            client.Inventory.OnObjectOffered -= Inventory_OnObjectOffered;
            client.Inventory.OnFindObjectByPath -= Inventory_OnFindObjectByPath;
            client.Inventory.OnTaskItemReceived -= Inventory_OnTaskItemReceived;
            client.Inventory.OnTaskInventoryReply -= Inventory_OnTaskInventoryReply;
            client.Directory.OnClassifiedReply -= Directory_OnClassifiedReply;
            client.Directory.OnDirLandReply -= Directory_OnDirLandReply;
            client.Directory.OnDirPeopleReply -= Directory_OnDirPeopleReply;
            client.Directory.OnDirGroupsReply -= Directory_OnDirGroupsReply;
            client.Directory.OnPlacesReply -= Directory_OnPlacesReply;
            client.Directory.OnEventsReply -= Directory_OnEventsReply;
            client.Directory.OnEventInfo -= Directory_OnEventInfo;
            client.Terrain.OnLandPatch -= Terrain_OnLandPatch;
            client.Sound.OnAttachSound -= Sound_OnAttachSound;
            client.Sound.OnAttachSoundGainChange -= Sound_OnAttachSoundGainChange;
            client.Sound.OnSoundTrigger -= Sound_OnSoundTrigger;
            client.Sound.OnPreloadSound -= Sound_OnPreloadSound;
        }

    }
#pragma warning restore 0168

}

