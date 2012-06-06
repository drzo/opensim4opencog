using System;
using System.Collections.Generic;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using Cogbot.Actions;
using Cogbot.World;

#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif
//using RadegastTab = Radegast.SleekTab;

// older LibOMV
//using TeleportFlags = OpenMetaverse.AgentManager.TeleportFlags;
//using TeleportStatus = OpenMetaverse.AgentManager.TeleportStatus;

namespace Cogbot
{
    public partial class BotClient
    {
        public string MasterName
        {
            get
            {
                if (string.IsNullOrEmpty(_masterName) && UUID.Zero != _masterKey)
                {
                    MasterName = WorldSystem.GetUserName(_masterKey);
                }
                return _masterName;
            }
            set
            {
                if (!string.IsNullOrEmpty(value))
                {
                    UUID found;
                    if (UUID.TryParse(value, out found))
                    {
                        MasterKey = found;
                        return;
                    }
                    _masterName = value;
                    SetSecurityLevel(OWNERLEVEL, value, BotPermissions.Owner);
                    found = WorldSystem.GetUserID(value);
                    if (found != UUID.Zero)
                    {
                        MasterKey = found;
                    }
                }
            }
        }

        // permissions "NextOwner" means banned "Wait until they are an owner before doing anything!"
        public Dictionary<UUID, BotPermissions> SecurityLevels = new Dictionary<UUID, BotPermissions>();
        public Dictionary<string, BotPermissions> SecurityLevelsByName = new Dictionary<string, BotPermissions>();

        private UUID _masterKey = UUID.Zero;
        public UUID MasterKey
        {
            get
            {
                if (UUID.Zero == _masterKey && !string.IsNullOrEmpty(_masterName))
                {
                    UUID found = WorldSystem.GetUserID(_masterName);
                    if (found != UUID.Zero)
                    {
                        MasterKey = found;
                    }
                }
                return _masterKey;
            }
            set
            {
                if (UUID.Zero != value)
                {
                    _masterKey = value;
                    if (string.IsNullOrEmpty(_masterName))
                    {
                        string maybe = WorldSystem.GetUserName(value);
                        if (!string.IsNullOrEmpty(maybe)) MasterName = maybe;
                    }
                    lock (SecurityLevels) SecurityLevels[value] = BotPermissions.Owner;
                }
            }
        }
        public bool AllowObjectMaster
        {
            get
            {
                return _masterKey != UUID.Zero;
            }
        }

        public BotPermissions GetSecurityLevel(UUID uuid, string name)
        {
            BotPermissions bp;
            if (uuid != UUID.Zero)
            {
                lock (SecurityLevels)
                    if (SecurityLevels.TryGetValue(uuid, out bp))
                    {
                        return bp;
                    }
            }
            if (!string.IsNullOrEmpty(name))
            {
                lock (SecurityLevelsByName)
                    if (SecurityLevelsByName.TryGetValue(name, out bp))
                    {
                        return bp;
                    }
            }
            return BotPermissions.Stranger;
        }

        public void SetSecurityLevel(UUID uuid, string name, BotPermissions perms)
        {
            BotPermissions bp;
            if (uuid != UUID.Zero)
            {
                lock (SecurityLevels) SecurityLevels[uuid] = perms;
            }
            if (!string.IsNullOrEmpty(name))
            {
                // dont take whitepaces
                name = name.Trim();
                if (name != "") lock (SecurityLevelsByName) SecurityLevelsByName[name] = perms;
            }
        }

        public readonly static UUID OWNERLEVEL = UUID.Parse("ffffffff-ffff-ffff-ffff-ffffffffffff");

        public static UUID SessionToCallerId(object callerSession)
        {
            if (callerSession is BotClient) return OWNERLEVEL;
            if (callerSession == null)
            {
                return UUID.Zero;
            }
            UUID callerId = UUID.Zero;
            if (callerSession is UUID) callerId = (UUID)callerSession;
            if (callerId != UUID.Zero) return callerId;
            CmdRequest request = callerSession as CmdRequest;
            if (request != null) return SessionToCallerId(request.CallerAgent);
            return OWNERLEVEL;
            throw new NotImplementedException();
        }

        void Self_OnChat(object sender, ChatEventArgs e)
        {
            InstantMessageDialog Dialog = InstantMessageDialog.MessageFromAgent;
            switch (e.SourceType)
            {
                case ChatSourceType.System:
                    break;
                case ChatSourceType.Agent:
                    break;
                case ChatSourceType.Object:
                    Dialog = InstantMessageDialog.MessageFromObject;
                    break;
            }
            UUID regionID = UUID.Zero;
            if (e.Simulator != null)
            {
                regionID = e.Simulator.RegionID;
            }
            Self_OnMessage(e.FromName, e.SourceID, e.OwnerID,
                           e.Message, UUID.Zero, false,
                           regionID, e.Position,
                           Dialog, e.Type, e);
            ;

        }

        private void Self_OnInstantMessage(object sender, InstantMessageEventArgs e)
        {
            InstantMessage im = e.IM;
            ChatType Type = ChatType.Normal;
            switch (im.Dialog)
            {
                case InstantMessageDialog.StartTyping:
                    Type = ChatType.StartTyping;
                    break;
                case InstantMessageDialog.StopTyping:
                    Type = ChatType.StopTyping;
                    break;
            }
            Self_OnMessage(im.FromAgentName, im.FromAgentID, im.ToAgentID,
                           im.Message, im.IMSessionID, im.GroupIM,
                           im.RegionID, im.Position,
                           im.Dialog, Type, e);
        }

        private void Self_OnMessage(string FromAgentName, UUID FromAgentID, UUID ToAgentID,
            string Message, UUID IMSessionID, bool GroupIM,
            UUID RegionID, Vector3 Position,
            InstantMessageDialog Dialog, ChatType Type, EventArgs origin)
        {
            bool IsOwner = (Type == ChatType.OwnerSay);
            if (Dialog == InstantMessageDialog.GroupNotice)
            {
                GroupIM = true;
            }

            // Received an IM from someone that is authenticated
            if (FromAgentID == MasterKey || FromAgentName == MasterName)
            {
                IsOwner = true;
            }
            BotPermissions perms = GetSecurityLevel(FromAgentID, FromAgentName);

            if (perms == BotPermissions.Owner)
            {
                IsOwner = true;
            }

            bool displayedMessage = false;
            if (origin is ChatEventArgs && Message.Length > 0 && Dialog == InstantMessageDialog.MessageFromAgent)
            {
                WriteLine(String.Format("{0} says, \"{1}\".", FromAgentName, Message));
                PosterBoard["/posterboard/onchat"] = Message;
                if (FromAgentName == Self.Name)
                {
                    PosterBoard["/posterboard/onchat-said"] = Message;
                }
                else
                {
                    PosterBoard["/posterboard/onchat-heard"] = Message;
                }
            }

            bool groupIM = GroupIM && GroupMembers != null && GroupMembers.ContainsKey(FromAgentID) ? true : false;


            switch (Dialog)
            {
                case InstantMessageDialog.MessageBox:
                    break;
                case InstantMessageDialog.GroupInvitation:
                    if (IsOwner)
                    {
                        string groupName = Message;
                        int found = groupName.IndexOf("Group:");
                        if (found > 0) groupName = groupName.Substring(found + 6);
                        Self.InstantMessage(Self.Name, FromAgentID, string.Empty, IMSessionID,
                                            InstantMessageDialog.GroupInvitationAccept, InstantMessageOnline.Offline,
                                            Self.SimPosition,
                                            UUID.Zero, new byte[0]);
                        found = groupName.IndexOf(":");
                        if (found > 0)
                        {
                            groupName = groupName.Substring(0, found).Trim();
                            ExecuteCommand("joingroup " + groupName);
                        }
                    }
                    break;
                case InstantMessageDialog.InventoryOffered:
                    break;
                case InstantMessageDialog.InventoryAccepted:
                    break;
                case InstantMessageDialog.InventoryDeclined:
                    break;
                case InstantMessageDialog.GroupVote:
                    break;
                case InstantMessageDialog.TaskInventoryOffered:
                    break;
                case InstantMessageDialog.TaskInventoryAccepted:
                    break;
                case InstantMessageDialog.TaskInventoryDeclined:
                    break;
                case InstantMessageDialog.NewUserDefault:
                    break;
                case InstantMessageDialog.SessionAdd:
                    break;
                case InstantMessageDialog.SessionOfflineAdd:
                    break;
                case InstantMessageDialog.SessionGroupStart:
                    break;
                case InstantMessageDialog.SessionCardlessStart:
                    break;
                case InstantMessageDialog.SessionSend:
                    break;
                case InstantMessageDialog.SessionDrop:
                    break;
                case InstantMessageDialog.BusyAutoResponse:
                    break;
                case InstantMessageDialog.ConsoleAndChatHistory:
                    break;
                case InstantMessageDialog.Lure911:
                case InstantMessageDialog.RequestTeleport:
                    if (IsOwner)
                    {
                        TheSimAvatar.StopMoving();
                        if (RegionID != UUID.Zero)
                        {
                            if (!displayedMessage)
                            {
                                DisplayNotificationInChat("TP to Lure from " + FromAgentName);
                                displayedMessage = true;
                            }
                            SimRegion R = SimRegion.GetRegion(RegionID, gridClient);
                            if (R != null)
                            {
                                Self.Teleport(R.RegionHandle, Position);
                                return;
                            }
                        }
                        DisplayNotificationInChat("Accepting TP Lure from " + FromAgentName);
                        displayedMessage = true;
                        Self.TeleportLureRespond(FromAgentID, IMSessionID, true);
                    }
                    break;
                case InstantMessageDialog.AcceptTeleport:
                    break;
                case InstantMessageDialog.DenyTeleport:
                    break;
                case InstantMessageDialog.GodLikeRequestTeleport:
                    break;
                case InstantMessageDialog.CurrentlyUnused:
                    break;
                case InstantMessageDialog.GotoUrl:
                    break;
                case InstantMessageDialog.Session911Start:
                    break;
                case InstantMessageDialog.FromTaskAsAlert:
                    break;
                case InstantMessageDialog.GroupNotice:
                    break;
                case InstantMessageDialog.GroupNoticeInventoryAccepted:
                    break;
                case InstantMessageDialog.GroupNoticeInventoryDeclined:
                    break;
                case InstantMessageDialog.GroupInvitationAccept:
                    break;
                case InstantMessageDialog.GroupInvitationDecline:
                    break;
                case InstantMessageDialog.GroupNoticeRequested:
                    break;
                case InstantMessageDialog.FriendshipOffered:
                    if (IsOwner)
                    {
                        DisplayNotificationInChat("Accepting Friendship from " + FromAgentName);
                        Friends.AcceptFriendship(FromAgentID, IMSessionID);
                        displayedMessage = true;
                    }
                    break;
                case InstantMessageDialog.FriendshipAccepted:
                    break;
                case InstantMessageDialog.FriendshipDeclined:
                    break;
                case InstantMessageDialog.StartTyping:
                    break;
                case InstantMessageDialog.StopTyping:
                    break;
                case InstantMessageDialog.MessageFromObject:
                case InstantMessageDialog.MessageFromAgent:
                    // message from self
                    if (FromAgentName == GetName()) return;
                    // message from system
                    if (FromAgentName == "System") return;
                    // message from others
                    CommandInfo ci;
                    if (Commands.TryGetValue("im", out ci))
                    {
                        var whisper = ci.WithBotClient as Cogbot.Actions.Communication.ImCommand;
                        if (whisper != null)
                        {
                            whisper.currentAvatar = FromAgentID;
                            whisper.currentSession = IMSessionID;
                        }
                    }
                    if (IsOwner)
                    {
                        OutputDelegate WriteLine;
                        if (origin is InstantMessageEventArgs)
                        {
                            WriteLine = new OutputDelegate((string text, object[] ps) =>
                            {
                                string reply0 = DLRConsole.SafeFormat(text, ps);
                                InstantMessage(FromAgentID, reply0, IMSessionID);
                            });
                        }
                        else
                        {
                            WriteLine = new OutputDelegate((string text, object[] ps) =>
                            {
                                string reply0 = DLRConsole.SafeFormat(text, ps);
                                Talk(reply0, 0, Type);
                            });
                        }
                        string cmd = Message;

                        if (cmd.StartsWith("cmcmd "))
                        {
                            cmd = cmd.Substring(6);
                            WriteLine("");
                            WriteLine(string.Format("invokecm='{0}'", cmd));
                            ClientManager.DoCommandAll(cmd, FromAgentID, WriteLine);
                        }
                        else if (cmd.StartsWith("cmd "))
                        {
                            cmd = cmd.Substring(4);
                            WriteLine(string.Format("invoke='{0}'", cmd));
                            var res = ExecuteCommand(cmd, FromAgentID, WriteLine);
                            WriteLine("iresult='" + res + "'");
                        }
                        else if (cmd.StartsWith("/") || cmd.StartsWith("@"))
                        {
                            cmd = cmd.Substring(1);
                            WriteLine("");
                            WriteLine(string.Format("invoke='{0}'", cmd));
                            var res = ExecuteCommand(cmd, FromAgentID, WriteLine);
                            WriteLine("iresult='" + res + "'");
                        }
                    }
                    break;
                default:
                    break;
            }
            if (Dialog != InstantMessageDialog.MessageFromAgent && Dialog != InstantMessageDialog.MessageFromObject)
            {
                string debug = String.Format("{0} {1} {2} {3} {4}: {5}",
                                             IsOwner ? "IsOwner" : "NonOwner",
                                             groupIM ? "GroupIM" : "IM", Dialog, Type, perms,
                                             Helpers.StructToString(origin));
                if (!displayedMessage)
                {
                    DisplayNotificationInChat(debug);
                    displayedMessage = true;
                }
            }
        }
        public static bool AcceptAllInventoryItems = false;
        public static bool HasPermission(BotPermissions them, BotPermissions testFor)
        {
            if ((them & BotPermissions.Ignore) != 0) return false;
            return (them & testFor) != 0;
        }

        private void Inventory_OnInventoryObjectReceived(object sender, InventoryObjectOfferedEventArgs e)
        {
            if (AcceptAllInventoryItems)
            {
                e.Accept = true;
                return; // accept everything}
            }
            BotPermissions them = GetSecurityLevel(e.Offer.FromAgentID, e.Offer.FromAgentName);
            if (HasPermission(them, BotPermissions.AcceptInventory))
            {
                e.Accept = false;
                return;
            }
            if (_masterKey != UUID.Zero)
            {
                if (e.Offer.FromAgentID != _masterKey)
                {
                    e.Accept = false;
                    return;
                }
            }
            else if (GroupMembers != null && !GroupMembers.ContainsKey(e.Offer.FromAgentID))
            {
                e.Accept = false;
                return;
            }

            e.Accept = true;
        }
    }
    /// <summary>
    ///  
    /// </summary>
    [Flags]
    public enum BotPermissions : uint
    {
        None,
        Ignore,
        ChatWith,
        AcceptInventory,
        ExecuteCommands,
        ExecuteCode,
        AcceptGroupAndFriendRequests,
        AcceptTeleport,

        Stranger = ChatWith,
        Friend = AcceptInventory | ChatWith | AcceptGroupAndFriendRequests | AcceptTeleport,
        Trusted = Friend | ExecuteCommands,
        Owner = Trusted | ExecuteCode,
    }

}