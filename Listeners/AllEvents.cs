using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Reflection; //using OpenMetaverse;

namespace cogbot.Listeners
{
#pragma warning disable 0168
    public abstract class AllEvents : Listener
    {
        public AllEvents(BotClient bot)
            : base(bot)
        {
        }

        static readonly string[] paramNamesOnAvatarAnimation = new string[] { "agentID", "agentAnimations" };
        static readonly Type[] paramTypesOnAvatarAnimation = new Type[] { typeof(UUID), typeof(InternalDictionary<UUID, int>) };

        public virtual void Avatars_OnAvatarAnimation(UUID agentID, InternalDictionary<UUID, int> agentAnimations) { OnEvent("On-Avatar-Animation", paramNamesOnAvatarAnimation, paramTypesOnAvatarAnimation, agentID, agentAnimations); }



        public abstract bool BooleanOnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters);
        public virtual void OnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters)
        {
            BooleanOnEvent(eventName, paramNames, paramTypes, parameters);
        }

        static readonly string[] paramNamesOnLogin = new string[] { "login", "message" };
        static readonly Type[] paramTypesOnLogin = new Type[] { typeof(LoginStatus), typeof(string) };

        public virtual void Network_OnLogin(LoginStatus login, string message) { OnEvent("On-Login", paramNamesOnLogin, paramTypesOnLogin, login, message); }

        static readonly string[] paramNamesOnConnected = new string[] { "sender" };
        static readonly Type[] paramTypesOnConnected = new Type[] { typeof(System.Object) };

        public virtual void Network_OnConnected(System.Object sender) { OnEvent("On-Connected", paramNamesOnConnected, paramTypesOnConnected, sender); }

        static readonly string[] paramNamesOnLogoutReply = new string[] { "inventoryItems" };
        static readonly Type[] paramTypesOnLogoutReply = new Type[] { typeof(List<UUID>) };

        public virtual void Network_OnLogoutReply(List<UUID> inventoryItems) { OnEvent("On-Logout-Reply", paramNamesOnLogoutReply, paramTypesOnLogoutReply, inventoryItems); }

        static readonly string[] paramNamesOnSimConnecting = new string[] { "simulator" };
        static readonly Type[] paramTypesOnSimConnecting = new Type[] { typeof(Simulator) };

        public bool Network_OnSimConnecting(Simulator simulator)
        { return BooleanOnEvent("On-Sim-Connecting", paramNamesOnSimConnecting, paramTypesOnSimConnecting, simulator); }

        static readonly string[] paramNamesOnSimConnected = new string[] { "simulator" };
        static readonly Type[] paramTypesOnSimConnected = new Type[] { typeof(Simulator) };

        public virtual void Network_OnSimConnected(Simulator simulator) { OnEvent("On-Sim-Connected", paramNamesOnSimConnected, paramTypesOnSimConnected, simulator); }

        static readonly string[] paramNamesOnSimDisconnected = new string[] { "simulator", "reason" };
        static readonly Type[] paramTypesOnSimDisconnected = new Type[] { typeof(Simulator), typeof(NetworkManager.DisconnectType) };

        public virtual void Network_OnSimDisconnected(Simulator simulator, NetworkManager.DisconnectType reason) { OnEvent("On-Sim-Disconnected", paramNamesOnSimDisconnected, paramTypesOnSimDisconnected, simulator, reason); }

        static readonly string[] paramNamesOnDisconnected = new string[] { "reason", "message" };
        static readonly Type[] paramTypesOnDisconnected = new Type[] { typeof(NetworkManager.DisconnectType), typeof(string) };

        public virtual void Network_OnDisconnected(NetworkManager.DisconnectType reason, string message) { OnEvent("On-Disconnected", paramNamesOnDisconnected, paramTypesOnDisconnected, reason, message); }

        static readonly string[] paramNamesOnCurrentSimChanged = new string[] { "PreviousSimulator" };
        static readonly Type[] paramTypesOnCurrentSimChanged = new Type[] { typeof(Simulator) };

        public virtual void Network_OnCurrentSimChanged(Simulator PreviousSimulator) { OnEvent("On-Current-Sim-Changed", paramNamesOnCurrentSimChanged, paramTypesOnCurrentSimChanged, PreviousSimulator); }

        static readonly string[] paramNamesOnEventQueueRunning = new string[] { "simulator" };
        static readonly Type[] paramTypesOnEventQueueRunning = new Type[] { typeof(Simulator) };

        public virtual void Network_OnEventQueueRunning(Simulator simulator) { OnEvent("On-Event-Queue-Running", paramNamesOnEventQueueRunning, paramTypesOnEventQueueRunning, simulator); }

        static readonly string[] paramNamesOnParcelDwell = new string[] { "parcelID", "localID", "dwell" };
        static readonly Type[] paramTypesOnParcelDwell = new Type[] { typeof(UUID), typeof(int), typeof(float) };

        public virtual void Parcels_OnParcelDwell(UUID parcelID, int localID, float dwell) { OnEvent("On-Parcel-Dwell", paramNamesOnParcelDwell, paramTypesOnParcelDwell, parcelID, localID, dwell); }

        static readonly string[] paramNamesOnParcelInfo = new string[] { "parcel" };
        static readonly Type[] paramTypesOnParcelInfo = new Type[] { typeof(ParcelInfo) };

        public virtual void Parcels_OnParcelInfo(ParcelInfo parcel) { OnEvent("On-Parcel-Info", paramNamesOnParcelInfo, paramTypesOnParcelInfo, parcel); }

        static readonly string[] paramNamesOnParcelProperties = new string[] { "simulator", "parcel", "result", "selectedPrims", "sequenceID", "snapSelection" };
        static readonly Type[] paramTypesOnParcelProperties = new Type[] { typeof(Simulator), typeof(Parcel), typeof(ParcelResult), typeof(int), typeof(int), typeof(bool) };

        public virtual void Parcels_OnParcelProperties(Simulator simulator, Parcel parcel, ParcelResult result, int selectedPrims, int sequenceID, bool snapSelection) { OnEvent("On-Parcel-Properties", paramNamesOnParcelProperties, paramTypesOnParcelProperties, simulator, parcel, result, selectedPrims, sequenceID, snapSelection); }

        static readonly string[] paramNamesOnAccessListReply = new string[] { "simulator", "sequenceID", "localID", "flags", "accessEntries" };
        static readonly Type[] paramTypesOnAccessListReply = new Type[] { typeof(Simulator), typeof(int), typeof(int), typeof(uint), typeof(List<ParcelManager.ParcelAccessEntry>) };

        public virtual void Parcels_OnAccessListReply(Simulator simulator, int sequenceID, int localID, uint flags, List<ParcelManager.ParcelAccessEntry> accessEntries) { OnEvent("On-Access-List-Reply", paramNamesOnAccessListReply, paramTypesOnAccessListReply, simulator, sequenceID, localID, flags, accessEntries); }

        static readonly string[] paramNamesOnPrimOwnersListReply = new string[] { "simulator", "primOwners" };
        static readonly Type[] paramTypesOnPrimOwnersListReply = new Type[] { typeof(Simulator), typeof(List<ParcelManager.ParcelPrimOwners>) };

        public virtual void Parcels_OnPrimOwnersListReply(Simulator simulator, List<ParcelManager.ParcelPrimOwners> primOwners) { OnEvent("On-Prim-Owners-List-Reply", paramNamesOnPrimOwnersListReply, paramTypesOnPrimOwnersListReply, simulator, primOwners); }

        static readonly string[] paramNamesOnSimParcelsDownloaded = new string[] { "simulator", "simParcels", "parcelMap" };
        static readonly Type[] paramTypesOnSimParcelsDownloaded = new Type[] { typeof(Simulator), typeof(InternalDictionary<int, Parcel>), typeof(int[,]) };

        public virtual void Parcels_OnSimParcelsDownloaded(Simulator simulator, InternalDictionary<int, Parcel> simParcels, int[,] parcelMap) { OnEvent("On-Sim-Parcels-Downloaded", paramNamesOnSimParcelsDownloaded, paramTypesOnSimParcelsDownloaded, simulator, simParcels, parcelMap); }

        static readonly string[] paramNamesOnParcelSelectedObjects = new string[] { "simulator", "objectIDs", "resetList" };
        static readonly Type[] paramTypesOnParcelSelectedObjects = new Type[] { typeof(Simulator), typeof(List<uint>), typeof(bool) };

        public virtual void Parcels_OnParcelSelectedObjects(Simulator simulator, List<uint> objectIDs, bool resetList) { OnEvent("On-Parcel-Selected-Objects", paramNamesOnParcelSelectedObjects, paramTypesOnParcelSelectedObjects, simulator, objectIDs, resetList); }

        static readonly string[] paramNamesOnParcelMediaUpdate = new string[] { "simulator", "media" };
        static readonly Type[] paramTypesOnParcelMediaUpdate = new Type[] { typeof(Simulator), typeof(ParcelMedia) };

        public virtual void Parcels_OnParcelMediaUpdate(Simulator simulator, ParcelMedia media) { OnEvent("On-Parcel-Media-Update", paramNamesOnParcelMediaUpdate, paramTypesOnParcelMediaUpdate, simulator, media); }

        static readonly string[] paramNamesOnChat = new string[] { "message", "audible", "type", "sourceType", "fromName", "id", "ownerid", "position" };
        static readonly Type[] paramTypesOnChat = new Type[] { typeof(string), typeof(ChatAudibleLevel), typeof(ChatType), typeof(ChatSourceType), typeof(string), typeof(UUID), typeof(UUID), typeof(Vector3) };

        public virtual void Self_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourceType, string fromName, UUID id, UUID ownerid, Vector3 position) { OnEvent("On-Chat", paramNamesOnChat, paramTypesOnChat, message, audible, type, sourceType, fromName, id, ownerid, position); }

        static readonly string[] paramNamesOnScriptDialog = new string[] { "message", "objectName", "imageID", "objectID", "firstName", "lastName", "chatChannel", "buttons" };
        static readonly Type[] paramTypesOnScriptDialog = new Type[] { typeof(string), typeof(string), typeof(UUID), typeof(UUID), typeof(string), typeof(string), typeof(int), typeof(List<string>) };

        public virtual void Self_OnScriptDialog(string message, string objectName, UUID imageID, UUID objectID, string firstName, string lastName, int chatChannel, List<string> buttons) { OnEvent("On-Script-Dialog", paramNamesOnScriptDialog, paramTypesOnScriptDialog, message, objectName, imageID, objectID, firstName, lastName, chatChannel, buttons); }

        static readonly string[] paramNamesOnScriptQuestion = new string[] { "simulator", "taskID", "itemID", "objectName", "objectOwner", "questions" };
        static readonly Type[] paramTypesOnScriptQuestion = new Type[] { typeof(Simulator), typeof(UUID), typeof(UUID), typeof(string), typeof(string), typeof(ScriptPermission) };

        public virtual void Self_OnScriptQuestion(Simulator simulator, UUID taskID, UUID itemID, string objectName, string objectOwner, ScriptPermission questions) { OnEvent("On-Script-Question", paramNamesOnScriptQuestion, paramTypesOnScriptQuestion, simulator, taskID, itemID, objectName, objectOwner, questions); }

        static readonly string[] paramNamesOnLoadURL = new string[] { "objectName", "objectID", "ownerID", "ownerIsGroup", "message", "URL" };
        static readonly Type[] paramTypesOnLoadURL = new Type[] { typeof(string), typeof(UUID), typeof(UUID), typeof(bool), typeof(string), typeof(string) };

        public virtual void Self_OnLoadURL(string objectName, UUID objectID, UUID ownerID, bool ownerIsGroup, string message, string URL) { OnEvent("On-Load-U-R-L", paramNamesOnLoadURL, paramTypesOnLoadURL, objectName, objectID, ownerID, ownerIsGroup, message, URL); }

        static readonly string[] paramNamesOnInstantMessage = new string[] { "im", "simulator" };
        static readonly Type[] paramTypesOnInstantMessage = new Type[] { typeof(InstantMessage), typeof(Simulator) };

        public virtual void Self_OnInstantMessage(InstantMessage im, Simulator simulator) { OnEvent("On-Instant-Message", paramNamesOnInstantMessage, paramTypesOnInstantMessage, im, simulator); }

        static readonly string[] paramNamesOnTeleport = new string[] { "message", "status", "flags" };
        static readonly Type[] paramTypesOnTeleport = new Type[] { typeof(string), typeof(TeleportStatus), typeof(TeleportFlags) };

        public virtual void Self_OnTeleport(string message, TeleportStatus status, TeleportFlags flags) { OnEvent("On-Teleport", paramNamesOnTeleport, paramTypesOnTeleport, message, status, flags); }

        static readonly string[] paramNamesOnBalanceUpdated = new string[] { "balance" };
        static readonly Type[] paramTypesOnBalanceUpdated = new Type[] { typeof(int) };

        public virtual void Self_OnBalanceUpdated(int balance) 
        {
            OnEvent("On-Balance-Updated", paramNamesOnBalanceUpdated, paramTypesOnBalanceUpdated, balance); 
        }

        static readonly string[] paramNamesOnMoneyBalanceReplyReceived = new string[] { "transactionID", "transactionSuccess", "balance", "metersCredit", "metersCommitted", "description" };
        static readonly Type[] paramTypesOnMoneyBalanceReplyReceived = new Type[] { typeof(UUID), typeof(bool), typeof(int), typeof(int), typeof(int), typeof(string) };

        public virtual void Self_OnMoneyBalanceReplyReceived(UUID transactionID, bool transactionSuccess, int balance, int metersCredit, int metersCommitted, string description) { OnEvent("On-Money-Balance-Reply-Received", paramNamesOnMoneyBalanceReplyReceived, paramTypesOnMoneyBalanceReplyReceived, transactionID, transactionSuccess, balance, metersCredit, metersCommitted, description); }

        static readonly string[] paramNamesOnAgentDataUpdated = new string[] { "firstName", "lastName", "activeGroupID", "groupTitle", "groupPowers", "groupName" };
        static readonly Type[] paramTypesOnAgentDataUpdated = new Type[] { typeof(string), typeof(string), typeof(UUID), typeof(string), typeof(GroupPowers), typeof(string) };

        public virtual void Self_OnAgentDataUpdated(string firstName, string lastName, UUID activeGroupID, string groupTitle, GroupPowers groupPowers, string groupName) { OnEvent("On-Agent-Data-Updated", paramNamesOnAgentDataUpdated, paramTypesOnAgentDataUpdated, firstName, lastName, activeGroupID, groupTitle, groupPowers, groupName); }

        static readonly string[] paramNamesOnAnimationsChanged = new string[] { "agentAnimations" };
        static readonly Type[] paramTypesOnAnimationsChanged = new Type[] { typeof(InternalDictionary<UUID, int>) };

        public virtual void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations) { OnEvent("On-Animations-Changed", paramNamesOnAnimationsChanged, paramTypesOnAnimationsChanged, agentAnimations); }

        static readonly string[] paramNamesOnMeanCollision = new string[] { "type", "perp", "victim", "magnitude", "time" };
        static readonly Type[] paramTypesOnMeanCollision = new Type[] { typeof(MeanCollisionType), typeof(UUID), typeof(UUID), typeof(float), typeof(System.DateTime) };

        public virtual void Self_OnMeanCollision(MeanCollisionType type, UUID perp, UUID victim, float magnitude, System.DateTime time) { OnEvent("On-Mean-Collision", paramNamesOnMeanCollision, paramTypesOnMeanCollision, type, perp, victim, magnitude, time); }

        static readonly string[] paramNamesOnRegionCrossed = new string[] { "oldSim", "newSim" };
        static readonly Type[] paramTypesOnRegionCrossed = new Type[] { typeof(Simulator), typeof(Simulator) };

        public virtual void Self_OnRegionCrossed(Simulator oldSim, Simulator newSim) { OnEvent("On-Region-Crossed", paramNamesOnRegionCrossed, paramTypesOnRegionCrossed, oldSim, newSim); }

        static readonly string[] paramNamesOnGroupChatJoin = new string[] { "groupChatSessionID", "sessionName", "tmpSessionID", "success" };
        static readonly Type[] paramTypesOnGroupChatJoin = new Type[] { typeof(UUID), typeof(string), typeof(UUID), typeof(bool) };

        public virtual void Self_OnGroupChatJoin(UUID groupChatSessionID, string sessionName, UUID tmpSessionID, bool success) { OnEvent("On-Group-Chat-Join", paramNamesOnGroupChatJoin, paramTypesOnGroupChatJoin, groupChatSessionID, sessionName, tmpSessionID, success); }

        static readonly string[] paramNamesOnGroupChatLeft = new string[] { "groupchatSessionID" };
        static readonly Type[] paramTypesOnGroupChatLeft = new Type[] { typeof(UUID) };

        public virtual void Self_OnGroupChatLeft(UUID groupchatSessionID) { OnEvent("On-Group-Chat-Left", paramNamesOnGroupChatLeft, paramTypesOnGroupChatLeft, groupchatSessionID); }

        static readonly string[] paramNamesOnAlertMessage = new string[] { "message" };
        static readonly Type[] paramTypesOnAlertMessage = new Type[] { typeof(string) };

        public virtual void Self_OnAlertMessage(string message) { OnEvent("On-Alert-Message", paramNamesOnAlertMessage, paramTypesOnAlertMessage, message); }

        static readonly string[] paramNamesOnScriptControlChange = new string[] { "controls", "pass", "take" };
        static readonly Type[] paramTypesOnScriptControlChange = new Type[] { typeof(ScriptControlChange), typeof(bool), typeof(bool) };

        public virtual void Self_OnScriptControlChange(ScriptControlChange controls, bool pass, bool take) { OnEvent("On-Script-Control-Change", paramNamesOnScriptControlChange, paramTypesOnScriptControlChange, controls, pass, take); }

        static readonly string[] paramNamesOnCameraConstraint = new string[] { "collidePlane" };
        static readonly Type[] paramTypesOnCameraConstraint = new Type[] { typeof(Vector4) };

        public virtual void Self_OnCameraConstraint(Vector4 collidePlane) { OnEvent("On-Camera-Constraint", paramNamesOnCameraConstraint, paramTypesOnCameraConstraint, collidePlane); }

        static readonly string[] paramNamesOnScriptSensorReply = new string[] { "requestorID", "groupID", "name", "objectID", "ownerID", "position", "range", "rotation", "type", "velocity" };
        static readonly Type[] paramTypesOnScriptSensorReply = new Type[] { typeof(UUID), typeof(UUID), typeof(string), typeof(UUID), typeof(UUID), typeof(Vector3), typeof(float), typeof(Quaternion), typeof(ScriptSensorTypeFlags), typeof(Vector3) };

        public virtual void Self_OnScriptSensorReply(UUID requestorID, UUID groupID, string name, UUID objectID, UUID ownerID, Vector3 position, float range, Quaternion rotation, ScriptSensorTypeFlags type, Vector3 velocity) { OnEvent("On-Script-Sensor-Reply", paramNamesOnScriptSensorReply, paramTypesOnScriptSensorReply, requestorID, groupID, name, objectID, ownerID, position, range, rotation, type, velocity); }

        static readonly string[] paramNamesOnAvatarSitResponse = new string[] { "objectID", "autoPilot", "cameraAtOffset", "cameraEyeOffset", "forceMouselook", "sitPosition", "sitRotation" };
        static readonly Type[] paramTypesOnAvatarSitResponse = new Type[] { typeof(UUID), typeof(bool), typeof(Vector3), typeof(Vector3), typeof(bool), typeof(Vector3), typeof(Quaternion) };

        public virtual void Self_OnAvatarSitResponse(UUID objectID, bool autoPilot, Vector3 cameraAtOffset, Vector3 cameraEyeOffset, bool forceMouselook, Vector3 sitPosition, Quaternion sitRotation) { OnEvent("On-Avatar-Sit-Response", paramNamesOnAvatarSitResponse, paramTypesOnAvatarSitResponse, objectID, autoPilot, cameraAtOffset, cameraEyeOffset, forceMouselook, sitPosition, sitRotation); }

        static readonly string[] paramNamesOnChatSessionMemberAdded = new string[] { "sessionID", "agent_key" };
        static readonly Type[] paramTypesOnChatSessionMemberAdded = new Type[] { typeof(UUID), typeof(UUID) };

        public virtual void Self_OnChatSessionMemberAdded(UUID sessionID, UUID agent_key) { OnEvent("On-Chat-Session-Member-Added", paramNamesOnChatSessionMemberAdded, paramTypesOnChatSessionMemberAdded, sessionID, agent_key); }

        static readonly string[] paramNamesOnChatSessionMemberLeft = new string[] { "sessionID", "agent_key" };
        static readonly Type[] paramTypesOnChatSessionMemberLeft = new Type[] { typeof(UUID), typeof(UUID) };

        public virtual void Self_OnChatSessionMemberLeft(UUID sessionID, UUID agent_key) { OnEvent("On-Chat-Session-Member-Left", paramNamesOnChatSessionMemberLeft, paramTypesOnChatSessionMemberLeft, sessionID, agent_key); }

        static readonly string[] paramNamesOnAvatarAppearance = new string[] { "avatarID", "isTrial", "defaultTexture", "faceTextures", "visualParams" };
        static readonly Type[] paramTypesOnAvatarAppearance = new Type[] { typeof(UUID), typeof(bool), typeof(Primitive.TextureEntryFace), typeof(Primitive.TextureEntryFace[]), typeof(List<byte>) };

        public virtual void Avatars_OnAvatarAppearance(UUID avatarID, bool isTrial, Primitive.TextureEntryFace defaultTexture, Primitive.TextureEntryFace[] faceTextures, List<byte> visualParams) { OnEvent("On-Avatar-Appearance", paramNamesOnAvatarAppearance, paramTypesOnAvatarAppearance, avatarID, isTrial, defaultTexture, faceTextures, visualParams); }

        static readonly string[] paramNamesOnAvatarNames = new string[] { "names" };
        static readonly Type[] paramTypesOnAvatarNames = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarNames(Dictionary<UUID, string> names) { OnEvent("On-Avatar-Names", paramNamesOnAvatarNames, paramTypesOnAvatarNames, names); }

        static readonly string[] paramNamesOnAvatarInterests = new string[] { "avatarID", "interests" };
        static readonly Type[] paramTypesOnAvatarInterests = new Type[] { typeof(UUID), typeof(Avatar.Interests) };

        public virtual void Avatars_OnAvatarInterests(UUID avatarID, Avatar.Interests interests) { OnEvent("On-Avatar-Interests", paramNamesOnAvatarInterests, paramTypesOnAvatarInterests, avatarID, interests); }

        static readonly string[] paramNamesOnAvatarProperties = new string[] { "avatarID", "properties" };
        static readonly Type[] paramTypesOnAvatarProperties = new Type[] { typeof(UUID), typeof(Avatar.AvatarProperties) };

        public virtual void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties) { OnEvent("On-Avatar-Properties", paramNamesOnAvatarProperties, paramTypesOnAvatarProperties, avatarID, properties); }

        static readonly string[] paramNamesOnAvatarGroups = new string[] { "avatarID", "avatarGroups" };
        static readonly Type[] paramTypesOnAvatarGroups = new Type[] { typeof(UUID), typeof(List<AvatarGroup>) };

        public virtual void Avatars_OnAvatarGroups(UUID avatarID, List<AvatarGroup> avatarGroups) { OnEvent("On-Avatar-Groups", paramNamesOnAvatarGroups, paramTypesOnAvatarGroups, avatarID, avatarGroups); }

        static readonly string[] paramNamesOnAvatarNameSearch = new string[] { "queryID", "avatars" };
        static readonly Type[] paramTypesOnAvatarNameSearch = new Type[] { typeof(UUID), typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarNameSearch(UUID queryID, Dictionary<UUID, string> avatars) { OnEvent("On-Avatar-Name-Search", paramNamesOnAvatarNameSearch, paramTypesOnAvatarNameSearch, queryID, avatars); }

        static readonly string[] paramNamesOnPointAt = new string[] { "sourceID", "targetID", "targetPos", "pointType", "duration", "id" };
        static readonly Type[] paramTypesOnPointAt = new Type[] { typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(PointAtType), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnPointAt(UUID sourceID, UUID targetID, Vector3d targetPos, PointAtType pointType, float duration, UUID id) { OnEvent("On-Point-At", paramNamesOnPointAt, paramTypesOnPointAt, sourceID, targetID, targetPos, pointType, duration, id); }

        static readonly string[] paramNamesOnLookAt = new string[] { "sourceID", "targetID", "targetPos", "lookType", "duration", "id" };
        static readonly Type[] paramTypesOnLookAt = new Type[] { typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(LookAtType), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnLookAt(UUID sourceID, UUID targetID, Vector3d targetPos, LookAtType lookType, float duration, UUID id) { OnEvent("On-Look-At", paramNamesOnLookAt, paramTypesOnLookAt, sourceID, targetID, targetPos, lookType, duration, id); }

        static readonly string[] paramNamesOnEffect = new string[] { "type", "sourceID", "targetID", "targetPos", "duration", "id" };
        static readonly Type[] paramTypesOnEffect = new Type[] { typeof(EffectType), typeof(UUID), typeof(UUID), typeof(Vector3d), typeof(float), typeof(UUID) };

        public virtual void Avatars_OnEffect(EffectType type, UUID sourceID, UUID targetID, Vector3d targetPos, float duration, UUID id) { OnEvent("On-Effect", paramNamesOnEffect, paramTypesOnEffect, type, sourceID, targetID, targetPos, duration, id); }

        static readonly string[] paramNamesOnAvatarPicks = new string[] { "avatarid", "picks" };
        static readonly Type[] paramTypesOnAvatarPicks = new Type[] { typeof(UUID), typeof(Dictionary<UUID, string>) };

        public virtual void Avatars_OnAvatarPicks(UUID avatarid, Dictionary<UUID, string> picks) { OnEvent("On-Avatar-Picks", paramNamesOnAvatarPicks, paramTypesOnAvatarPicks, avatarid, picks); }

        static readonly string[] paramNamesOnPickInfo = new string[] { "pickid", "pick" };
        static readonly Type[] paramTypesOnPickInfo = new Type[] { typeof(UUID), typeof(ProfilePick) };

        public virtual void Avatars_OnPickInfo(UUID pickid, ProfilePick pick) { OnEvent("On-Pick-Info", paramNamesOnPickInfo, paramTypesOnPickInfo, pickid, pick); }

        static readonly string[] paramNamesOnFriendNamesReceived = new string[] { "names" };
        static readonly Type[] paramTypesOnFriendNamesReceived = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Friends_OnFriendNamesReceived(Dictionary<UUID, string> names) { OnEvent("On-Friend-Names-Received", paramNamesOnFriendNamesReceived, paramTypesOnFriendNamesReceived, names); }

        static readonly string[] paramNamesOnFriendOnline = new string[] { "friend" };
        static readonly Type[] paramTypesOnFriendOnline = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendOnline(FriendInfo friend) { OnEvent("On-Friend-Online", paramNamesOnFriendOnline, paramTypesOnFriendOnline, friend); }

        static readonly string[] paramNamesOnFriendOffline = new string[] { "friend" };
        static readonly Type[] paramTypesOnFriendOffline = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendOffline(FriendInfo friend) { OnEvent("On-Friend-Offline", paramNamesOnFriendOffline, paramTypesOnFriendOffline, friend); }

        static readonly string[] paramNamesOnFriendRights = new string[] { "friend" };
        static readonly Type[] paramTypesOnFriendRights = new Type[] { typeof(FriendInfo) };

        public virtual void Friends_OnFriendRights(FriendInfo friend) { OnEvent("On-Friend-Rights", paramNamesOnFriendRights, paramTypesOnFriendRights, friend); }

        static readonly string[] paramNamesOnFriendshipOffered = new string[] { "agentID", "agentName", "imSessionID" };
        static readonly Type[] paramTypesOnFriendshipOffered = new Type[] { typeof(UUID), typeof(string), typeof(UUID) };

        public virtual void Friends_OnFriendshipOffered(UUID agentID, string agentName, UUID imSessionID) { OnEvent("On-Friendship-Offered", paramNamesOnFriendshipOffered, paramTypesOnFriendshipOffered, agentID, agentName, imSessionID); }

        static readonly string[] paramNamesOnFriendshipResponse = new string[] { "agentID", "agentName", "accepted" };
        static readonly Type[] paramTypesOnFriendshipResponse = new Type[] { typeof(UUID), typeof(string), typeof(bool) };

        public virtual void Friends_OnFriendshipResponse(UUID agentID, string agentName, bool accepted) { OnEvent("On-Friendship-Response", paramNamesOnFriendshipResponse, paramTypesOnFriendshipResponse, agentID, agentName, accepted); }

        static readonly string[] paramNamesOnFriendshipTerminated = new string[] { "agentID", "agentName" };
        static readonly Type[] paramTypesOnFriendshipTerminated = new Type[] { typeof(UUID), typeof(string) };

        public virtual void Friends_OnFriendshipTerminated(UUID agentID, string agentName) { OnEvent("On-Friendship-Terminated", paramNamesOnFriendshipTerminated, paramTypesOnFriendshipTerminated, agentID, agentName); }

        static readonly string[] paramNamesOnFriendFound = new string[] { "agentID", "regionHandle", "location" };
        static readonly Type[] paramTypesOnFriendFound = new Type[] { typeof(UUID), typeof(ulong), typeof(Vector3) };

        public virtual void Friends_OnFriendFound(UUID agentID, ulong regionHandle, Vector3 location) { OnEvent("On-Friend-Found", paramNamesOnFriendFound, paramTypesOnFriendFound, agentID, regionHandle, location); }

        static readonly string[] paramNamesOnCoarseLocationUpdate = new string[] { "sim" };
        static readonly Type[] paramTypesOnCoarseLocationUpdate = new Type[] { typeof(Simulator) };

        public virtual void Grid_OnCoarseLocationUpdate(Simulator sim) { OnEvent("On-Coarse-Location-Update", paramNamesOnCoarseLocationUpdate, paramTypesOnCoarseLocationUpdate, sim); }

        static readonly string[] paramNamesOnGridRegion = new string[] { "region" };
        static readonly Type[] paramTypesOnGridRegion = new Type[] { typeof(GridRegion) };

        public virtual void Grid_OnGridRegion(GridRegion region) { OnEvent("On-Grid-Region", paramNamesOnGridRegion, paramTypesOnGridRegion, region); }

        static readonly string[] paramNamesOnGridLayer = new string[] { "layer" };
        static readonly Type[] paramTypesOnGridLayer = new Type[] { typeof(GridLayer) };

        public virtual void Grid_OnGridLayer(GridLayer layer) { OnEvent("On-Grid-Layer", paramNamesOnGridLayer, paramTypesOnGridLayer, layer); }

        static readonly string[] paramNamesOnGridItems = new string[] { "type", "items" };
        static readonly Type[] paramTypesOnGridItems = new Type[] { typeof(GridItemType), typeof(List<GridItem>) };

        public virtual void Grid_OnGridItems(GridItemType type, List<GridItem> items) { OnEvent("On-Grid-Items", paramNamesOnGridItems, paramTypesOnGridItems, type, items); }

        static readonly string[] paramNamesOnRegionHandleReply = new string[] { "regionID", "regionHandle" };
        static readonly Type[] paramTypesOnRegionHandleReply = new Type[] { typeof(UUID), typeof(ulong) };

        public virtual void Grid_OnRegionHandleReply(UUID regionID, ulong regionHandle) { OnEvent("On-Region-Handle-Reply", paramNamesOnRegionHandleReply, paramTypesOnRegionHandleReply, regionID, regionHandle); }

        static readonly string[] paramNamesOnNewPrim = new string[] { "simulator", "prim", "regionHandle", "timeDilation" };
        static readonly Type[] paramTypesOnNewPrim = new Type[] { typeof(Simulator), typeof(Primitive), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation) { OnEvent("On-New-Prim", paramNamesOnNewPrim, paramTypesOnNewPrim, simulator, prim, regionHandle, timeDilation); }

        static readonly string[] paramNamesOnNewAttachment = new string[] { "simulator", "prim", "regionHandle", "timeDilation" };
        static readonly Type[] paramTypesOnNewAttachment = new Type[] { typeof(Simulator), typeof(Primitive), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewAttachment(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation) { OnEvent("On-New-Attachment", paramNamesOnNewAttachment, paramTypesOnNewAttachment, simulator, prim, regionHandle, timeDilation); }

        static readonly string[] paramNamesOnNewAvatar = new string[] { "simulator", "avatar", "regionHandle", "timeDilation" };
        static readonly Type[] paramTypesOnNewAvatar = new Type[] { typeof(Simulator), typeof(Avatar), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation) { OnEvent("On-New-Avatar", paramNamesOnNewAvatar, paramTypesOnNewAvatar, simulator, avatar, regionHandle, timeDilation); }

        static readonly string[] paramNamesOnObjectUpdated = new string[] { "simulator", "update", "regionHandle", "timeDilation" };
        static readonly Type[] paramTypesOnObjectUpdated = new Type[] { typeof(Simulator), typeof(ObjectUpdate), typeof(ulong), typeof(ushort) };

        public virtual void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation) { OnEvent("On-Object-Updated", paramNamesOnObjectUpdated, paramTypesOnObjectUpdated, simulator, update, regionHandle, timeDilation); }

        static readonly string[] paramNamesOnAvatarSitChanged = new string[] { "simulator", "avatar", "sittingOn", "oldSeat" };
        static readonly Type[] paramTypesOnAvatarSitChanged = new Type[] { typeof(Simulator), typeof(Avatar), typeof(uint), typeof(uint) };

        public virtual void Objects_OnAvatarSitChanged(Simulator simulator, Avatar avatar, uint sittingOn, uint oldSeat) { OnEvent("On-Avatar-Sit-Changed", paramNamesOnAvatarSitChanged, paramTypesOnAvatarSitChanged, simulator, avatar, sittingOn, oldSeat); }

        static readonly string[] paramNamesOnObjectKilled = new string[] { "simulator", "objectID" };
        static readonly Type[] paramTypesOnObjectKilled = new Type[] { typeof(Simulator), typeof(uint) };

        public virtual void Objects_OnObjectKilled(Simulator simulator, uint objectID) { OnEvent("On-Object-Killed", paramNamesOnObjectKilled, paramTypesOnObjectKilled, simulator, objectID); }

        static readonly string[] paramNamesOnObjectProperties = new string[] { "simulator", "props" };
        static readonly Type[] paramTypesOnObjectProperties = new Type[] { typeof(Simulator), typeof(Primitive.ObjectProperties) };

        public virtual void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props) { OnEvent("On-Object-Properties", paramNamesOnObjectProperties, paramTypesOnObjectProperties, simulator, props); }

        static readonly string[] paramNamesOnObjectPropertiesFamily = new string[] { "simulator", "props", "type" };
        static readonly Type[] paramTypesOnObjectPropertiesFamily = new Type[] { typeof(Simulator), typeof(Primitive.ObjectProperties), typeof(ReportType) };

        public virtual void Objects_OnObjectPropertiesFamily(Simulator simulator, Primitive.ObjectProperties props, ReportType type) { OnEvent("On-Object-Properties-Family", paramNamesOnObjectPropertiesFamily, paramTypesOnObjectPropertiesFamily, simulator, props, type); }

        static readonly string[] paramNamesOnCurrentGroups = new string[] { "groups" };
        static readonly Type[] paramTypesOnCurrentGroups = new Type[] { typeof(Dictionary<UUID, Group>) };

        public virtual void Groups_OnCurrentGroups(Dictionary<UUID, Group> groups) { OnEvent("On-Current-Groups", paramNamesOnCurrentGroups, paramTypesOnCurrentGroups, groups); }

        static readonly string[] paramNamesOnGroupNames = new string[] { "groupNames" };
        static readonly Type[] paramTypesOnGroupNames = new Type[] { typeof(Dictionary<UUID, string>) };

        public virtual void Groups_OnGroupNames(Dictionary<UUID, string> groupNames) { OnEvent("On-Group-Names", paramNamesOnGroupNames, paramTypesOnGroupNames, groupNames); }

        static readonly string[] paramNamesOnGroupProfile = new string[] { "group" };
        static readonly Type[] paramTypesOnGroupProfile = new Type[] { typeof(Group) };

        public virtual void Groups_OnGroupProfile(Group group) { OnEvent("On-Group-Profile", paramNamesOnGroupProfile, paramTypesOnGroupProfile, group); }

        static readonly string[] paramNamesOnGroupMembers = new string[] { "members" };
        static readonly Type[] paramTypesOnGroupMembers = new Type[] { typeof(Dictionary<UUID, GroupMember>) };

        public virtual void Groups_OnGroupMembers(Dictionary<UUID, GroupMember> members) { OnEvent("On-Group-Members", paramNamesOnGroupMembers, paramTypesOnGroupMembers, members); }

        static readonly string[] paramNamesOnGroupRoles = new string[] { "roles" };
        static readonly Type[] paramTypesOnGroupRoles = new Type[] { typeof(Dictionary<UUID, GroupRole>) };

        public virtual void Groups_OnGroupRoles(Dictionary<UUID, GroupRole> roles) { OnEvent("On-Group-Roles", paramNamesOnGroupRoles, paramTypesOnGroupRoles, roles); }

        static readonly string[] paramNamesOnGroupRolesMembers = new string[] { "rolesMembers" };
        static readonly Type[] paramTypesOnGroupRolesMembers = new Type[] { typeof(List<KeyValuePair<UUID, UUID>>) };

        public virtual void Groups_OnGroupRolesMembers(List<KeyValuePair<UUID, UUID>> rolesMembers) { OnEvent("On-Group-Roles-Members", paramNamesOnGroupRolesMembers, paramTypesOnGroupRolesMembers, rolesMembers); }

        static readonly string[] paramNamesOnGroupTitles = new string[] { "titles" };
        static readonly Type[] paramTypesOnGroupTitles = new Type[] { typeof(Dictionary<UUID, GroupTitle>) };

        public virtual void Groups_OnGroupTitles(Dictionary<UUID, GroupTitle> titles) { OnEvent("On-Group-Titles", paramNamesOnGroupTitles, paramTypesOnGroupTitles, titles); }

        static readonly string[] paramNamesOnGroupAccountSummary = new string[] { "summary" };
        static readonly Type[] paramTypesOnGroupAccountSummary = new Type[] { typeof(GroupAccountSummary) };

        public virtual void Groups_OnGroupAccountSummary(GroupAccountSummary summary) { OnEvent("On-Group-Account-Summary", paramNamesOnGroupAccountSummary, paramTypesOnGroupAccountSummary, summary); }

        static readonly string[] paramNamesOnGroupCreated = new string[] { "groupID", "success", "message" };
        static readonly Type[] paramTypesOnGroupCreated = new Type[] { typeof(UUID), typeof(bool), typeof(string) };

        public virtual void Groups_OnGroupCreated(UUID groupID, bool success, string message) { OnEvent("On-Group-Created", paramNamesOnGroupCreated, paramTypesOnGroupCreated, groupID, success, message); }

        static readonly string[] paramNamesOnGroupJoined = new string[] { "groupID", "success" };
        static readonly Type[] paramTypesOnGroupJoined = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupJoined(UUID groupID, bool success) { OnEvent("On-Group-Joined", paramNamesOnGroupJoined, paramTypesOnGroupJoined, groupID, success); }

        static readonly string[] paramNamesOnGroupLeft = new string[] { "groupID", "success" };
        static readonly Type[] paramTypesOnGroupLeft = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupLeft(UUID groupID, bool success) { OnEvent("On-Group-Left", paramNamesOnGroupLeft, paramTypesOnGroupLeft, groupID, success); }

        static readonly string[] paramNamesOnGroupDropped = new string[] { "groupID" };
        static readonly Type[] paramTypesOnGroupDropped = new Type[] { typeof(UUID) };

        public virtual void Groups_OnGroupDropped(UUID groupID) { OnEvent("On-Group-Dropped", paramNamesOnGroupDropped, paramTypesOnGroupDropped, groupID); }

        static readonly string[] paramNamesOnGroupMemberEjected = new string[] { "groupID", "success" };
        static readonly Type[] paramTypesOnGroupMemberEjected = new Type[] { typeof(UUID), typeof(bool) };

        public virtual void Groups_OnGroupMemberEjected(UUID groupID, bool success) { OnEvent("On-Group-Member-Ejected", paramNamesOnGroupMemberEjected, paramTypesOnGroupMemberEjected, groupID, success); }

        static readonly string[] paramNamesOnGroupNoticesList = new string[] { "groupID", "notice" };
        static readonly Type[] paramTypesOnGroupNoticesList = new Type[] { typeof(UUID), typeof(GroupNoticeList) };

        public virtual void Groups_OnGroupNoticesList(UUID groupID, GroupNoticeList notice) { OnEvent("On-Group-Notices-List", paramNamesOnGroupNoticesList, paramTypesOnGroupNoticesList, groupID, notice); }

        static readonly string[] paramNamesOnAssetReceived = new string[] { "transfer", "asset" };
        static readonly Type[] paramTypesOnAssetReceived = new Type[] { typeof(AssetDownload), typeof(Asset) };

        public virtual void Assets_OnAssetReceived(AssetDownload transfer, Asset asset) { OnEvent("On-Asset-Received", paramNamesOnAssetReceived, paramTypesOnAssetReceived, transfer, asset); }

        static readonly string[] paramNamesOnXferReceived = new string[] { "xfer" };
        static readonly Type[] paramTypesOnXferReceived = new Type[] { typeof(XferDownload) };

        public virtual void Assets_OnXferReceived(XferDownload xfer) { OnEvent("On-Xfer-Received", paramNamesOnXferReceived, paramTypesOnXferReceived, xfer); }

        static readonly string[] paramNamesOnImageReceived = new string[] { "image", "asset" };
        static readonly Type[] paramTypesOnImageReceived = new Type[] { typeof(ImageDownload), typeof(AssetTexture) };

        public virtual void Assets_OnImageReceived(ImageDownload image, AssetTexture asset) { OnEvent("On-Image-Received", paramNamesOnImageReceived, paramTypesOnImageReceived, image, asset); }

        static readonly string[] paramNamesOnImageReceiveProgress = new string[] { "image", "lastPacket", "recieved", "total" };
        static readonly Type[] paramTypesOnImageReceiveProgress = new Type[] { typeof(UUID), typeof(int), typeof(int), typeof(int) };

        public virtual void Assets_OnImageReceiveProgress(UUID image, int lastPacket, int recieved, int total) { OnEvent("On-Image-Receive-Progress", paramNamesOnImageReceiveProgress, paramTypesOnImageReceiveProgress, image, lastPacket, recieved, total); }

        static readonly string[] paramNamesOnAssetUploaded = new string[] { "upload" };
        static readonly Type[] paramTypesOnAssetUploaded = new Type[] { typeof(AssetUpload) };

        public virtual void Assets_OnAssetUploaded(AssetUpload upload) { OnEvent("On-Asset-Uploaded", paramNamesOnAssetUploaded, paramTypesOnAssetUploaded, upload); }

        static readonly string[] paramNamesOnUploadProgress = new string[] { "upload" };
        static readonly Type[] paramTypesOnUploadProgress = new Type[] { typeof(AssetUpload) };

        public virtual void Assets_OnUploadProgress(AssetUpload upload) { OnEvent("On-Upload-Progress", paramNamesOnUploadProgress, paramTypesOnUploadProgress, upload); }

        static readonly string[] paramNamesOnAgentWearables = new string[] { };
        static readonly Type[] paramTypesOnAgentWearables = new Type[] { };

        public virtual void Appearance_OnAgentWearables() { OnEvent("On-Agent-Wearables", paramNamesOnAgentWearables, paramTypesOnAgentWearables); }

        static readonly string[] paramNamesOnAppearanceUpdated = new string[] { "te" };
        static readonly Type[] paramTypesOnAppearanceUpdated = new Type[] { typeof(Primitive.TextureEntry) };

        public virtual void Appearance_OnAppearanceUpdated(Primitive.TextureEntry te) { OnEvent("On-Appearance-Updated", paramNamesOnAppearanceUpdated, paramTypesOnAppearanceUpdated, te); }

        static readonly string[] paramNamesOnItemReceived = new string[] { "item" };
        static readonly Type[] paramTypesOnItemReceived = new Type[] { typeof(InventoryItem) };

        public virtual void Inventory_OnItemReceived(InventoryItem item) { OnEvent("On-Item-Received", paramNamesOnItemReceived, paramTypesOnItemReceived, item); }

        static readonly string[] paramNamesOnFolderUpdated = new string[] { "folderID" };
        static readonly Type[] paramTypesOnFolderUpdated = new Type[] { typeof(UUID) };

        public virtual void Inventory_OnFolderUpdated(UUID folderID) { OnEvent("On-Folder-Updated", paramNamesOnFolderUpdated, paramTypesOnFolderUpdated, folderID); }

        static readonly string[] paramNamesOnObjectOffered = new string[] { "offerDetails", "type", "objectID", "fromTask" };
        static readonly Type[] paramTypesOnObjectOffered = new Type[] { typeof(InstantMessage), typeof(AssetType), typeof(UUID), typeof(bool) };

        public bool Inventory_OnObjectOffered(InstantMessage offerDetails, AssetType type, UUID objectID, bool fromTask)
        { return BooleanOnEvent("On-Object-Offered", paramNamesOnObjectOffered, paramTypesOnObjectOffered, offerDetails, type, objectID, fromTask); }

        static readonly string[] paramNamesOnFindObjectByPath = new string[] { "path", "inventoryObjectID" };
        static readonly Type[] paramTypesOnFindObjectByPath = new Type[] { typeof(string), typeof(UUID) };

        public virtual void Inventory_OnFindObjectByPath(string path, UUID inventoryObjectID) { OnEvent("On-Find-Object-By-Path", paramNamesOnFindObjectByPath, paramTypesOnFindObjectByPath, path, inventoryObjectID); }

        static readonly string[] paramNamesOnTaskItemReceived = new string[] { "itemID", "folderID", "creatorID", "assetID", "type" };
        static readonly Type[] paramTypesOnTaskItemReceived = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(UUID), typeof(InventoryType) };

        public virtual void Inventory_OnTaskItemReceived(UUID itemID, UUID folderID, UUID creatorID, UUID assetID, InventoryType type) { OnEvent("On-Task-Item-Received", paramNamesOnTaskItemReceived, paramTypesOnTaskItemReceived, itemID, folderID, creatorID, assetID, type); }

        static readonly string[] paramNamesOnTaskInventoryReply = new string[] { "itemID", "serial", "assetFilename" };
        static readonly Type[] paramTypesOnTaskInventoryReply = new Type[] { typeof(UUID), typeof(short), typeof(string) };

        public virtual void Inventory_OnTaskInventoryReply(UUID itemID, short serial, string assetFilename) { OnEvent("On-Task-Inventory-Reply", paramNamesOnTaskInventoryReply, paramTypesOnTaskInventoryReply, itemID, serial, assetFilename); }

        static readonly string[] paramNamesOnClassifiedReply = new string[] { "classifieds" };
        static readonly Type[] paramTypesOnClassifiedReply = new Type[] { typeof(List<DirectoryManager.Classified>) };

        public virtual void Directory_OnClassifiedReply(List<DirectoryManager.Classified> classifieds) { OnEvent("On-Classified-Reply", paramNamesOnClassifiedReply, paramTypesOnClassifiedReply, classifieds); }

        static readonly string[] paramNamesOnDirLandReply = new string[] { "dirParcels" };
        static readonly Type[] paramTypesOnDirLandReply = new Type[] { typeof(List<DirectoryManager.DirectoryParcel>) };

        public virtual void Directory_OnDirLandReply(List<DirectoryManager.DirectoryParcel> dirParcels) { OnEvent("On-Dir-Land-Reply", paramNamesOnDirLandReply, paramTypesOnDirLandReply, dirParcels); }

        static readonly string[] paramNamesOnDirPeopleReply = new string[] { "queryID", "matchedPeople" };
        static readonly Type[] paramTypesOnDirPeopleReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.AgentSearchData>) };

        public virtual void Directory_OnDirPeopleReply(UUID queryID, List<DirectoryManager.AgentSearchData> matchedPeople) { OnEvent("On-Dir-People-Reply", paramNamesOnDirPeopleReply, paramTypesOnDirPeopleReply, queryID, matchedPeople); }

        static readonly string[] paramNamesOnDirGroupsReply = new string[] { "queryID", "matchedGroups" };
        static readonly Type[] paramTypesOnDirGroupsReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.GroupSearchData>) };

        public virtual void Directory_OnDirGroupsReply(UUID queryID, List<DirectoryManager.GroupSearchData> matchedGroups) { OnEvent("On-Dir-Groups-Reply", paramNamesOnDirGroupsReply, paramTypesOnDirGroupsReply, queryID, matchedGroups); }

        static readonly string[] paramNamesOnPlacesReply = new string[] { "queryID", "matchedPlaces" };
        static readonly Type[] paramTypesOnPlacesReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.PlacesSearchData>) };

        public virtual void Directory_OnPlacesReply(UUID queryID, List<DirectoryManager.PlacesSearchData> matchedPlaces) { OnEvent("On-Places-Reply", paramNamesOnPlacesReply, paramTypesOnPlacesReply, queryID, matchedPlaces); }

        static readonly string[] paramNamesOnEventsReply = new string[] { "queryID", "matchedEvents" };
        static readonly Type[] paramTypesOnEventsReply = new Type[] { typeof(UUID), typeof(List<DirectoryManager.EventsSearchData>) };

        public virtual void Directory_OnEventsReply(UUID queryID, List<DirectoryManager.EventsSearchData> matchedEvents) { OnEvent("On-Events-Reply", paramNamesOnEventsReply, paramTypesOnEventsReply, queryID, matchedEvents); }

        static readonly string[] paramNamesOnEventInfo = new string[] { "matchedEvent" };
        static readonly Type[] paramTypesOnEventInfo = new Type[] { typeof(DirectoryManager.EventInfo) };

        public virtual void Directory_OnEventInfo(DirectoryManager.EventInfo matchedEvent) { OnEvent("On-Event-Info", paramNamesOnEventInfo, paramTypesOnEventInfo, matchedEvent); }

        static readonly string[] paramNamesOnLandPatch = new string[] { "simulator", "x", "y", "width", "data" };
        static readonly Type[] paramTypesOnLandPatch = new Type[] { typeof(Simulator), typeof(int), typeof(int), typeof(int), typeof(float[]) };

        public virtual void Terrain_OnLandPatch(Simulator simulator, int x, int y, int width, float[] data) { OnEvent("On-Land-Patch", paramNamesOnLandPatch, paramTypesOnLandPatch, simulator, x, y, width, data); }

        static readonly string[] paramNamesOnAttachSound = new string[] { "soundID", "ownerID", "objectID", "gain", "flags" };
        static readonly Type[] paramTypesOnAttachSound = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(float), typeof(byte) };

        public virtual void Sound_OnAttachSound(UUID soundID, UUID ownerID, UUID objectID, float gain, byte flags) { OnEvent("On-Attach-Sound", paramNamesOnAttachSound, paramTypesOnAttachSound, soundID, ownerID, objectID, gain, flags); }

        static readonly string[] paramNamesOnAttachSoundGainChange = new string[] { "objectID", "gain" };
        static readonly Type[] paramTypesOnAttachSoundGainChange = new Type[] { typeof(UUID), typeof(float) };

        public virtual void Sound_OnAttachSoundGainChange(UUID objectID, float gain) { OnEvent("On-Attach-Sound-Gain-Change", paramNamesOnAttachSoundGainChange, paramTypesOnAttachSoundGainChange, objectID, gain); }

        static readonly string[] paramNamesOnSoundTrigger = new string[] { "soundID", "ownerID", "objectID", "parentID", "gain", "regionHandle", "position" };
        static readonly Type[] paramTypesOnSoundTrigger = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID), typeof(UUID), typeof(float), typeof(ulong), typeof(Vector3) };

        public virtual void Sound_OnSoundTrigger(UUID soundID, UUID ownerID, UUID objectID, UUID parentID, float gain, ulong regionHandle, Vector3 position) { OnEvent("On-Sound-Trigger", paramNamesOnSoundTrigger, paramTypesOnSoundTrigger, soundID, ownerID, objectID, parentID, gain, regionHandle, position); }

        static readonly string[] paramNamesOnPreloadSound = new string[] { "soundID", "ownerID", "objectID" };
        static readonly Type[] paramTypesOnPreloadSound = new Type[] { typeof(UUID), typeof(UUID), typeof(UUID) };

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
            client.Objects.OnObjectUpdated += Objects_OnObjectUpdated;
            client.Objects.OnAvatarSitChanged += Objects_OnAvatarSitChanged;
            client.Objects.OnObjectKilled += Objects_OnObjectKilled;
            client.Objects.OnObjectProperties += Objects_OnObjectProperties;
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
            client.Assets.OnAssetReceived += Assets_OnAssetReceived;
            client.Assets.OnXferReceived += Assets_OnXferReceived;
            if (TextForm.DownloadTextures)
            {
                client.Assets.OnImageReceived += Assets_OnImageReceived;
                client.Assets.OnImageReceiveProgress += Assets_OnImageReceiveProgress;
            }
            client.Assets.OnAssetUploaded += Assets_OnAssetUploaded;
            client.Assets.OnUploadProgress += Assets_OnUploadProgress;
            client.Appearance.OnAgentWearables += Appearance_OnAgentWearables;
            client.Appearance.OnAppearanceUpdated += Appearance_OnAppearanceUpdated;
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
            client.Avatars.OnAvatarAnimation -= Avatars_OnAvatarAnimation;

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
            client.Assets.OnAssetReceived -= Assets_OnAssetReceived;
            client.Assets.OnXferReceived -= Assets_OnXferReceived;
            client.Assets.OnImageReceived -= Assets_OnImageReceived;
            client.Assets.OnImageReceiveProgress -= Assets_OnImageReceiveProgress;
            client.Assets.OnAssetUploaded -= Assets_OnAssetUploaded;
            client.Assets.OnUploadProgress -= Assets_OnUploadProgress;
            client.Appearance.OnAgentWearables -= Appearance_OnAgentWearables;
            client.Appearance.OnAppearanceUpdated -= Appearance_OnAppearanceUpdated;
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

