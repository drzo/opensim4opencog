using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading;
using AIMLbot;
using AltAIMLbot;
using AltAIMLParser;
using Cogbot.Actions;
using Cogbot;
using Cogbot.ScriptEngines;
using Cogbot.World;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using RTParser;
using Cogbot;
using Cogbot.Utilities;
using RTParser.Utils;
using RTParser.Variables;
using Exception=System.Exception;
using Math=System.Math;
using String=System.String;
using Thread=System.Threading.Thread;
using PathSystem3D.Navigation;
using SUnifiable=System.String;
using User=AltAIMLbot.User;

namespace AIMLBotModule
{
    public class WorldObjectsForAimLBot : WorldObjectsModule
    {
        private readonly ICollectionProvider provideAIMLVars;
        private readonly ISettingsDictionary provideWorldUserVars;
        private readonly ISettingsDictionary provideWorldBotVars;
 
        public override void InvokeCommand(string cmd, OutputDelegate output)
        {
            if (string.IsNullOrEmpty(cmd)) return;
            if (cmd.Contains("noserv")) UseServitorEngine = false;
        }

        public static bool UseServitorEngine = true;

        private static int _DefaultMaxRespondToChatPerMinute = 20;
        public static int DefaultMaxRespondToChatPerMinute
        {
            get
            {
                return _DefaultMaxRespondToChatPerMinute;
            }
            set
            {
                _DefaultMaxRespondToChatPerMinute = value;

            }
        }

        public static string UNKNOWN_PARTNER = Bot.UNKNOWN_PARTNER;
        /// <summary>
        ///  false = wont respond to user until they say something like "turn chat on" 
        ///  See next function to change the keywords
        /// </summary>
        public bool RespondToChatByDefaultAllUsers = false;
        /// <summary>
        /// Respond to group chat
        /// </summary>
        public static bool RespondToGroup = false;
        /// <summary>
        /// Respond to personal IM chat
        /// </summary>
        public static bool RespondToUserIM = true;
        /// <summary>
        /// Respond to chat EVER
        /// </summary>
        public static bool RespondToChatEver = true;
        /// <summary>
        /// Accept all friendship requests
        /// </summary>
        public static bool AcceptFriends = true;
        /// <summary>
        /// Send events to AIML processor
        /// </summary>
        public static bool EventsToAIML = false;
        /// <summary>
        /// Use animation to look like tpying
        /// </summary>
        public static bool UseRealism = false;
        /// <summary>
        /// Turn towards interesting objects
        /// </summary>
        public static bool UseLookAttention = true;
        /// <summary>
        /// Turn towards interesting objects
        /// </summary>
        public static bool UseNameAttention = true;
        /// <summary>
        /// Turn towards interesting objects
        /// </summary>
        public static bool UseMessageTurnsOnChat = true;
        /// <summary>
        /// Move towards interesting objects
        /// </summary>
        public static bool UseMoveAttention = false;
        /// <summary>
        /// Max Distance for attention objects
        /// </summary>
        public static double MaxDistance = 11;
        /// <summary>
        /// Max Distance for attention objects
        /// </summary>
        public static double MaxAvDistance = 21;
        /// <summary>
        /// Max Distance for attention objects on Z plane
        /// </summary>
        public static double MaxZDistance = 1;

        public bool NeedPersonalConfig = true;
        /// <summary>
        /// REgister the Lisp Version of TalkToObject
        /// </summary>
        public static bool RegisterTalkToCmd = true;
        readonly static object RegisterTalkToCmdLock = new object();


        object BotExecHandler(string cmd, Request request)
        {
            User prev = MyUser;
            try
            {
                MyUser = request.Requester;
                StringWriter sw = new StringWriter();
                {

                    cmd = cmd.Trim();
                    //if (cmd.StartsWith("@"))
                    {
                        if (MyBot.BotDirective(MyUser, request, cmd, sw.WriteLine))
                        {
                            return sw.ToString();
                        }
                    }
                    CmdResult s = client.ExecuteCommand(cmd, request, sw.WriteLine, CMDFLAGS.Foregrounded);
                    if (cmd.StartsWith("anim"))
                    {
                        AddAnimToNextResponse(sw.ToString());
                    }
                    return String.Format("{0}{1}", sw.ToString(), s);
                }
            }
            finally
            {
                MyUser = prev;

            }
        }

        object BotExecHandlerNew(string cmd, Result request)
        {
            User prev = MyUser;
            try
            {
                MyUser = request.Requester.Value;
                StringWriter sw = new StringWriter();
                {
                    CmdResult s;
                    if (cmd.StartsWith("anim") || cmd.StartsWith("thread anim 4"))
                    {
                        AddAnimToNextResponse(sw.ToString());
                        s = ACmdResult.Complete("BotExecHandlerNew", "", true);
                    }
                    else
                    {
                        s = client.ExecuteCommand(cmd, request, sw.WriteLine, CMDFLAGS.Foregrounded);
                    }
                    return String.Format("{0}{1}", sw, s);
                }
            }
            finally
            {
                MyUser = prev;

            }
        }

        object LispExecHandler(string cmd, Request request)
        {
            User prev = MyUser;
            try
            {
                MyUser = request.Requester;
                return client.evalLispString(cmd);
            }
            finally
            {
                MyUser = prev;

            }
        }

        static public void TalkToObject(SimObject av, SimObject obj)
        {
            try
            {
                ((WorldObjectsForAimLBot)av["AIMLBotModule"]).TalkToObject0(av, obj);
            }
            catch (Exception e)
            {
                Logger.Log("[AIMLBOT] exception " + e, Helpers.LogLevel.Error, e);
            }
        }
        private void TalkToObject0(SimObject av, SimObject obj)
        {
            if (MyBotNullWarning()) return;
            string objName = obj.GetName();
            MyUser = GetMyUser(objName);
            MyUser.RespondToChat = true;
            String str = String.Format("{0}, {1}", objName, AIMLInterp("RANDOM PICKUP LINE", MyUser)).Trim();
            while (str.EndsWith("?")) str = str.Substring(0, str.Length - 1).Trim();
            StringChat(str);
        }

        public Bot _MyBot;
        private User _MyUser;

        readonly TaskQueueHandler AimlBotReadSimData;
        readonly TaskQueueHandler AimlBotRespond;
        public static bool StartupBlocking = true;

        public override void StartupListener()
        {
            if (StartupBlocking)
            {
                StartupListener0();
                return;
            }
            AimlBotReadSimData.Enqueue(StartupListener0);
            AimlBotReadSimData.Enqueue(() => AimlBotRespond.Start());
            AimlBotReadSimData.Start();
        }

        readonly object SILStartupListener00 = new object();
        private bool SILStartupListenerDone;
        public void StartupListener0()
        {
            lock (SILStartupListener00)
            {
                if (SILStartupListenerDone) return;
                SILStartupListenerDone = true;
                StartupListener00();
            }
        }

        private void EnsureRegisteredTalkCommand()
        {
            lock (RegisterTalkToCmdLock)
            {
                if (RegisterTalkToCmd)
                {
                    RegisterTalkToCmd = false;
                    SimTypeUsage u = SimTypeSystem.CreateTypeUsage("TalkToObject");
                    u.SpecifiedProperties.Add("LispScript");
                    u.LispScript = "(AIMLBotModule.WorldObjectsForAimLBot:TalkToObject TheBot TheTarget)";
                    u = SimTypeSystem.CreateTypeUsage("KissTheObject");
                    u.SpecifiedProperties.Add("LispScript");
                    u.LispScript = "(AIMLBotModule.WorldObjectsForAimLBot:TalkToObject TheBot TheTarget)";
                }
            }
        }
        public void StartupListener00()
        {
            EnsureRegisteredTalkCommand();
            try
            {
                var MyBot = new Bot();
                MyBot.ObjectRequester = client;
                MyBot.outputDelegate = WriteLine;
                MyBot.isAcceptingUserInput = false;
                MyBot.useServitor = UseServitorEngine;
                MyBot.loadGlobalBotSettings();
                //MyBot.GlobalSettings.addSetting("name", client.BotLoginParams.FirstName+ " " + client.BotLoginParams.LastName);
                MyBot.loadAIMLFromDefaults();
                MyBot.isAcceptingUserInput = true;
                MyBot.outputDelegate = WriteLine;
                String ss = client.GetName();
                this.MyBot = MyBot;
                LoadPersonalConfig();
                MyBot.WriteConfig();
                // wont get here unless there was no problem
                client.Self.ChatFromSimulator += AIML_OnChat;
                client.Self.IM += AIML_OnInstantMessage;
                client.Network.LoginProgress += AIML_OnLogin;
                client.Network.EventQueueRunning += AIML_OnEventQueueRunning;
                client.Friends.FriendshipOffered += AIML_OnFriendshipOffered;
                client.Avatars.ViewerEffectPointAt += AIML_OnPointAt;
                client.Avatars.ViewerEffectLookAt += AIML_OnLookAt;
                client.Avatars.ViewerEffect += AINL_OnEffect;
                client.OnInstantMessageSent += OnSelfSentIM;
                if (client.Network.Connected)
                {
                    ReadSimSettings0();
                }
                if (EventsToAIML)
                {
                    SimEventSubscriber evtSub = new AIMLEventSubscriber(MyBot, this);
                    client.AddBotMessageSubscriber(evtSub);
                }
                if (MyBot.useServitor)
                {
                    MyBot.updateRTP2Sevitor();
                    MyBot.servitor.curBot.sayProcessor = new sayProcessorDelegate(TalkActive);
                    MyBot.servitor.loadComplete();
                }
            }
            catch (Exception e)
            {
                WriteLine("ERROR {0}", e);
            }
        }

        private void OnSelfSentIM(object sender, IMessageSentEventArgs args)
        {
            HeardMyselfSay(args.TargetID, args.Message);
        }

        private void AINL_OnEffect(object sender, ViewerEffectEventArgs e)
        {
            if (e.SourceID == client.Self.AgentID) return;
            if (e.Type == EffectType.LookAt) return;
            SetInterest(e.SourceID, e.TargetID, false, PCode.None);
        }

        private void AIML_OnLookAt(object sender, ViewerEffectLookAtEventArgs e)
        {
            if (e.SourceID == client.Self.AgentID) return;
            //if (targetid==client.Self.AgentID) SetInterest(sourceid, targetid, false, PCode.Avatar);
        }

        private void AIML_OnPointAt(object sender, ViewerEffectPointAtEventArgs e)
        {
            if (PointAtType.None == e.PointType) return;
            if (e.SourceID == client.Self.AgentID) return;
            SetInterest(e.SourceID, e.TargetID, true, PCode.Avatar);
        }
        // handler.Enqueue(() => 
        private void SetInterest(UUID sourceid, UUID targetid, bool forced, PCode hint)
        {
            SetInterest0(sourceid, targetid, forced, hint);
        }
        private void SetInterest0(UUID sourceid, UUID targetid, bool forced, PCode hint)
        {
            if (MyBotNullWarning()) return;
            if (targetid == client.Self.AgentID) AttendTo(null, sourceid, PCode.Avatar);
            else AttendTo(null, targetid, PCode.None);
            if (sourceid == UUID.Zero) return;
            if (targetid == UUID.Zero) return;
            if (targetid == sourceid) return;
            SimObject source = WorldObjects.GetSimObjectFromUUID(sourceid);
            if (source == null) return;
            SimObject target = WorldObjects.GetSimObjectFromUUID(targetid);
            if (target == null) return;
            string name = source.GetName();
            if (string.IsNullOrEmpty(name)) return;
            User user = GetMyUser(name);
            SettingsDictionary myUserPredicates = user.Predicates;
            myUserPredicates.addSetting("it", targetid.ToString());
            myUserPredicates.addSetting("what", targetid.ToString());
           // myUserPredicates.addSetting("object", targetid.ToString());
        }


        private void AIML_OnFriendshipOffered(object sender, FriendshipOfferedEventArgs e)
        {
            if (AcceptFriends) client.Friends.AcceptFriendship(e.AgentID, e.SessionID);
            //else client.Friends.DeclineFriendship(agentid, imsessionid);
        }

        private void AIML_OnEventQueueRunning(object sender, EventQueueRunningEventArgs e)
        {
            ReadSimSettings();
        }

        private void AIML_OnLogin(object sender, LoginProgressEventArgs e)
        {
            if (e.Status == LoginStatus.Success)
            {
                ReadSimSettings();
            }
        }
        private void ReadSimSettings()
        {
            AimlBotReadSimData.Enqueue(() => ReadSimSettings0());
        }
        private void ReadSimSettings0()
        {
            client.InternType(this.GetType());
            string myName = GetName().Trim();
            String[] sname = myName.Split(' ');
            if (MyBotNullWarning()) return;
            MyBot.GlobalSettings.addSetting("name", String.Format("{0}", myName));
            MyBot.GlobalSettings.addSetting("firstname", sname[0]);
            MyBot.GlobalSettings.addSetting("lastname", sname[1]);
            MyBot.GlobalSettings.addSetting("master", client.MasterName);
            client.WorldSystem.TheSimAvatar["AIMLBotModule"] = this;
            client.WorldSystem.TheSimAvatar["MyBot"] = MyBot;
            ScriptManager.AddGroupProvider(client, this.provideAIMLVars);

            LoadPersonalConfig();

        }

        private void LoadPersonalConfig()
        {
            if (MyBotNullWarning()) return;
            if (!NeedPersonalConfig) return;
            string myName = GetName().ToLower().Trim().Replace("_", " ");
            if (string.IsNullOrEmpty(myName)) return;
            NeedPersonalConfig = false;
            LoadPersonalDirectories(myName);
        }

        private void LoadPersonalDirectories(string myName)
        {
            if (MyBotNullWarning()) return;
            MyBot.SetName(myName);
            MyBot.BotAsUser.Predicates.InsertOverrides(() => provideWorldBotVars);
            MyBot.BotAsUser.Predicates.InsertListener(() => provideWorldBotVars);
        }

        public void SetChatOnOff(string username, bool value)
        {
            if (MyBotNullWarning()) return;
            MyBot.SetChatOnOff(username, value);
        }

        private User GetMyUser(string fromname)
        {
            bool newlyCreated;
            if (MyBotNullWarning()) return MyUser;
            if (String.IsNullOrEmpty(fromname))
            {
                fromname = "UNKNOWN_PARTNER";
            }
            if (!MyBot.IsLegalUserName(fromname))
            {
                return MyUser;
            }
            User user = MyBot.FindOrCreateUser(fromname, out newlyCreated);
            if (newlyCreated)
            {
                user.InsertProvider(() => this.provideWorldUserVars);
                user.RespondToChat = RespondToChatByDefaultAllUsers;
            }
            user.MaxRespondToChatPerMinute = DefaultMaxRespondToChatPerMinute;
            user.Predicates.addSetting("me", fromname);

            return user;
        }

        public string GetName()
        {
            return client.GetName();
        }

        public void AIML_OnInstantMessage(object sender, InstantMessageEventArgs e)
        {
            var im = e.IM;
            DLRConsole.DebugWriteLine("InstantMessage=" + im.Dialog);
            //DLRConsole.DebugWriteLine("FromAgentID=" + WorldSystem.GetObject(im.FromAgentID));
            object toObject = WorldSystem.GetObject(im.ToAgentID);
            //if (toObject!=null) DLRConsole.DebugWriteLine("ToAgentID=" + toObject.GetType());
            object sessionObject = WorldSystem.GetObject(im.IMSessionID);
            //if (sessionObject != null) DLRConsole.DebugWriteLine("SessionID=" + sessionObject.GetType());            


            if (im.Dialog == InstantMessageDialog.StartTyping || im.Dialog == InstantMessageDialog.StopTyping)
            {
                return;
            }
            if (im.FromAgentID == client.Self.AgentID)
            {
                HeardMyselfSay(im.ToAgentID, im.Message);
                return;
            }
            if (im.FromAgentName == GetName())
            {
                HeardMyselfSay(im.ToAgentID, im.Message);
                return;
            }
            if (im.FromAgentName == "System" || im.FromAgentName == "Second Life") return;
            if (im.FromAgentID == UUID.Zero)
            {
                return;
            }
            User myUser = GetMyUser(im.FromAgentName);
            SettingsDictionary myUserPredicates = myUser.Predicates;
            myUserPredicates.addSetting("host", im.FromAgentID.ToString());
            // myUser.Predicates.addObjectFields(im);
            if (im.Dialog == InstantMessageDialog.GroupNotice || im.Dialog == InstantMessageDialog.SessionSend)
            {
                im.GroupIM = true;
            }
            bool UseThrottle = im.GroupIM;
            string groupName = null;
            if (im.Dialog != InstantMessageDialog.MessageFromObject &&
                im.Dialog != InstantMessageDialog.MessageFromAgent &&
                im.Dialog != InstantMessageDialog.MessageBox &&
                im.Dialog != InstantMessageDialog.GroupNotice &&
                im.Dialog != InstantMessageDialog.SessionSend
                )
            {
                im.Message = String.Format("{0} {1}", im.Dialog, im.Message);
            }

            if (im.GroupIM)
            {
                SimGroup g = sessionObject as SimGroup;
                if (g != null)
                {
                    groupName = g.Group.Name;
                }
                WriteLine("Group IM {0}", groupName);
            }

            string message = im.Message;
            if (string.IsNullOrEmpty(message)) return;
            if (message == "typing") return;
            // Message is not group IM (sessionID == groupID) 
            if (!(sessionObject is SimGroup) && im.BinaryBucket.Length > 1)
            {
                // Session is ad-hoc friends conference                 
                groupName = Utils.BytesToString(im.BinaryBucket);
                im.GroupIM = true;
            }
            HandleIM(im, myUser, groupName, message, UseThrottle);

        }
        private void HandleIM(InstantMessage im, User myUser, string groupName, string message, bool UseThrottle)
        {
            RunTask(()=>HandleIM0(im,myUser,groupName,message,UseThrottle), "AIML_OnInstantMessage: " + myUser + ": " + message);
        }
        private void HandleIM0(InstantMessage im, User myUser, string groupName, string message, bool UseThrottle)
                        {

                            UUID toSession = im.ToAgentID ^ im.IMSessionID;
                            string resp = AIMLInterp(message, myUser);
                            // if (im.Offline == InstantMessageOnline.Offline) return;
                            if (String.IsNullOrEmpty(resp)) return;
                            if (UseThrottle)
                            {
                                if ((!myUser.CanGiveResponseNow()))
                                {
                                    WriteLine("AIML_OnInstantMessage Reply is too fast: {0}: {1}->{2}", myUser, message, resp);
                                    return; //too early to respond.. but still listened
                                }
                            }
                            UseRealism = true;
                            foreach (string ting in SplitChatSmart(resp))
                            {
                                string tsing = ting.Trim();
                                if (tsing.Length > 1000)
                                {
                                    tsing = tsing.Substring(0, 1000);
                                }
                                Thread.Sleep(100);
                                if (im.GroupIM)
                                {
                                    WriteLine("InstantMessageGroup={0} {1} {2} {3}",
                                              RespondToGroup, im.FromAgentName + "/" + groupName, im.FromAgentID,
                                              ting.Trim());
                                    if (!myUser.RespondToChat)
                                    {
                                        WriteLine("AIML_OnInstantMessage Reply is quietly {0}: {1}->{2}", myUser, message, resp);
                                        return;
                                    }
                                    if (!RespondToGroup)
                                    {
                                        WriteLine("!RespondToGroup {0}: {1}->{2}", myUser, message, resp);
                                        return;
                                    }
                                    client.Self.InstantMessageGroup(GetName(), im.IMSessionID, tsing);
                                }
                                else
                                {
                                    WriteLine("InstantMessage={0} {1} {2} {3}", RespondToUserIM,
                                              im.FromAgentName, im.FromAgentID, ting.Trim());

                                    if (!RespondToUserIM)
                                    {
                                        WriteLine("!RespondToUserIM {0}: {1}->{2}", myUser, message, resp);
                                        return;
                                    }
                                    // todo maybe send a typing message for the UseRealism
                                    if (UseRealism)
                                    {
                                        client.Self.InstantMessage(GetName(), im.FromAgentID, "typing",
                                                                   im.IMSessionID,
                                                                   InstantMessageDialog.StartTyping,
                                                                   InstantMessageOnline.Offline,
                                                                   client.Self.SimPosition,
                                                                   UUID.Zero, Utils.EmptyBytes);
                                        Thread.Sleep(1900);
                                        client.Self.InstantMessage(GetName(), im.FromAgentID, "typing",
                                                                   im.IMSessionID,
                                                                   InstantMessageDialog.StopTyping,
                                                                   InstantMessageOnline.Online,
                                                                   client.Self.SimPosition,
                                                                   UUID.Zero, Utils.EmptyBytes);

                                    }
                                    client.InstantMessage(im.FromAgentID, tsing, im.IMSessionID);
                                }
                                UseRealism = false;

                            }
                            myUser.StampResponseGiven();
        }

        public static bool RunInThreadPool = false;
        private void RunTask(ThreadStart action, string name)
            {
            if (RunInThreadPool)
                {
                    Enqueue(name, () => client.InvokeThread(name, action));
                }
            else
            {
                Enqueue(name, action);
            }
        }

        public WorldObjectsForAimLBot(BotClient testClient)
            : base(testClient)
        {
            AimlBotRespond = new TaskQueueHandler(client, "AIMLBot ChatRespond");
            AimlBotReadSimData = new TaskQueueHandler(client, "AIMLBot ReadSim");
            provideWorldUserVars = provideWorldBotVars = new ProvideWorldVars(this);
            provideAIMLVars = new ProvideAIMLVars(this);
        }

        private DateTime lastFollow = DateTime.Now;

        private void AIML_OnChat(object sender, ChatEventArgs e)// string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position)
        {

            var type = e.Type;
            var message = e.Message;
            var id = e.SourceID;
            var fromname = e.FromName;
            var sourcetype = e.SourceType;

            if (String.IsNullOrEmpty(message) || message.Length < 2) return;
            if (sourcetype == ChatSourceType.System) return;
            if (fromname == GetName())
            {
                HeardMyselfSay(UUID.Zero, message);
                return;
            }
            if (message.StartsWith("/me (")) return;
            if (string.IsNullOrEmpty(fromname))
            {
                Primitive prim = WorldSystem.GetPrimitive(id, e.Simulator);
                if (prim != null)
                {
                    fromname = GetSimObject(prim).GetName();
                }
            }
            PCode pCode = PCode.None;
            if (sourcetype == ChatSourceType.Agent)
            {
                pCode = PCode.Avatar;
            }
            if (sourcetype == ChatSourceType.Object)
            {
                pCode = PCode.Prim;
            }
            AttendTo(fromname, id, pCode);
            if (string.IsNullOrEmpty(fromname))
            {
                fromname = "" + id;
            }
            User myUser = GetMyUser(fromname);
            // todo hard coded to be changed
            if (!myUser.RespondToChat && MessageTurnsOnChat(message))
            {
                myUser.RespondToChat = true;
                return;
            }
            if (myUser.RespondToChat && MessageTurnsOffChat(message))
            {
                myUser.RespondToChat = false;
                return;
            }

            UseRealism = true;

            RunTask(() => OnChatTaskItem(message, myUser, type), "AIML_OnChat: " + myUser + ": " + message);
        }

        private void OnChatTaskItem(string message, User myUser, ChatType type)
                        {
                            string resp = AIMLInterp(message, myUser);
                            if (String.IsNullOrEmpty(resp)) return;
                            if (!MyUser.CanGiveResponseNow())
                            {
                                WriteLine("AIML_OnChat Reply is too fast {0}: {1}->{2}", myUser, message, resp);

                return;
                            }
                            if (!myUser.RespondToChat)
                            {
                                WriteLine("AIML_OnChat Reply is quietly {0}: {1}->{2}", myUser, message, resp);
                                return;
                            }
                            if (!RespondToChatEver)
                            {
                                WriteLine("!RespondToChatEver {0}: {1}->{2}", myUser, message, resp);
                                return;
                            }
                            StringChat(resp, type);
                            myUser.StampResponseGiven();
        }

        public void HeardMyselfSay(UUID uuid, string message)
        {
            if (MyBotNullWarning()) return;
            if (MyBot.useServitor) return;

            Enqueue("HeardMyselfSay: " + message,
                    () => MyBot.HeardSomeoneSay1Sentence(MyBot.BotAsUser, MyUser, message, null, null));
        }

        private void Enqueue(String name, ThreadStart action)
        {
            AimlBotRespond.Enqueue(() =>
                                       {
                                           try
                                           {
                                               AimlBotRespond.WaitingString = name;
                                               action();
                                           }
                                           catch (Exception e)
                                           {
                                               WriteLine("ERROR " + name + " " + e);
                                           }
                                       });
        }

        static bool MessageTurnsOffChat(string message)
        {
            message = message.ToLower();
            if (message.Contains("chat off")) return true;
            if (message.Contains("shut")) return true;
            if (message.Contains("bye")) return true;
            return false;
        }

        public void StringChat(string resp)
        {
            StringChat(resp, ChatType.Normal);
        }
        private void StringChat(string resp, ChatType type)
        {
            foreach (string ting in SplitChatSmart(resp))
            {
                string sting = ting.Trim();
                if (UseRealism)
                    Chat(client, sting, type, 6);
                else client.Talk(sting, 0, type);
                UseRealism = false;
            }
        }

        static string[] SplitChatSmart(string resp)
        {
            resp = resp.Replace("\r", "\n").Replace("\n\n", "\n").TrimEnd();
            int respLen = resp.Length;
            if (respLen > 800)
            {
                var slits = new List<String>();
                if (resp.Contains("\n"))
                {
                    foreach (string s in resp.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries))
                    {
                        slits.AddRange(SplitChatSmart(s));
                    }
                    return slits.ToArray();
                }
                int easySpace = resp.IndexOf(" ", 700);
                // find a space between 700-1000
                if (easySpace > 0 && easySpace < 1000)
                {
                    slits.AddRange(SplitChatSmart(resp.Substring(0, easySpace)));
                    slits.AddRange(SplitChatSmart(resp.Substring(easySpace)));
                }
                else
                {
                    slits.AddRange(SplitChatSmart(resp.Substring(0, 800)));
                    slits.AddRange(SplitChatSmart(resp.Substring(800)));
                }
                return slits.ToArray();
            }
            // split newlines
            return resp.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        }

        private bool MessageTurnsOnChat(string message)
        {
            if (!UseMessageTurnsOnChat) return false;
            message = message.ToLower();
            if (message.Contains("chat on")) return true;
            string n = client.Self.Name;
            if (string.IsNullOrEmpty(n)) return false;
            n = n.ToLower().Trim();
            if (n == "") return false;
            if (!UseNameAttention) return false;
            if (message.Contains(n)) return true;
            foreach (var c in n.Split(' '))
            {
                if (message.Contains(c)) return true;
            }
            return false;
        }

        private void AttendTo(string fromname, UUID fromID, PCode isAvatar)
        {
            if (!UseLookAttention && !UseMoveAttention) return;
            TimeSpan ts = new TimeSpan(DateTime.Now.Ticks - lastFollow.Ticks);
            SimObject talker = WorldSystem.AsObject(fromname, fromID, isAvatar);
            if (ts.TotalSeconds < 30)
            {
                if (talker == null) return;
                WriteLine("too soon for {0}", talker.ToString());
                return;
            }
            if (talker == null)
            {
                // WriteLine("cannot follow NULL");
                return;
            }
            if (!talker.IsRegionAttached)
            {
                WriteLine("Talker: !IsRegionAttached " + talker);
                return;
            }
            SimActor a = WorldSystem.TheSimAvatar;
            if (!a.IsRegionAttached)
            {
                WriteLine("!IsRegionAttached " + talker);
                return;
            }
            Vector3 aSimPos, tSimPos;
            if (!a.TryGetSimPosition(out aSimPos) || !talker.TryGetSimPosition(out tSimPos)) return;

            if (Math.Abs(aSimPos.Z - aSimPos.Z) > MaxZDistance)
            {
                WriteLine("Z Too far " + talker);
                return;
            }
            double dist = talker.Distance(a);
            if (talker is SimAvatar && dist > MaxAvDistance)
            {
                WriteLine("X,Y " + dist + " Too far to " + talker);
                return;
            }
            if (UseLookAttention)
            {
                a.TurnToward(talker);
            }
            if (dist > MaxDistance)
            {
                WriteLine("X,Y " + dist + " Too far to " + talker);
                return;
            }
            if (a.CurrentAction == null || a.CurrentAction is MoveToLocation)
            {
                if (UseMoveAttention)
                {
                    FollowerAction fa = new FollowerAction(a, talker);
                    client.output("" + fa);
                    a.CurrentAction = fa;
                    lastFollow = DateTime.Now;
                }
                else
                {
                    if (UseLookAttention)
                    {
                        a.TurnToward(talker);
                    }
                }
            }
        }

        static readonly TaskQueueHandler writeLock = new TaskQueueHandler(null, "AIMLBot Console Writer");
        public void WriteLine(string s, params object[] args)
        {
            if (args == null || args.Length == 0)
            {
                args = new object[] { s };
                s = "{0}";
            }
            if (Monitor.TryEnter(writeLock, 2000))
            {
                writeLock.Enqueue(() =>
                {
                    if (logAimlToClient && client != null) client.DebugWriteLine(s, args);
                    Logger.DebugLog(DLRConsole.SafeFormat(DLRConsole.SafeFormat("[AIMLBOT] {0} {1}", GetName(), s), args));
                });
                Monitor.Exit(writeLock);
            }
            else
            {
                DLRConsole.DebugWriteLine("cant even get a Enqueue! " + MethodInfo.GetCurrentMethod());
            }
        }

        /// <summary>
        /// A psuedo-realistic chat function that uses the typing sound and
        /// animation, types at a given rate, and randomly pauses. This 
        /// function will block until the message has been sent
        /// </summary>
        /// <param name="client">A reference to the client that will chat</param>
        /// <param name="message">The chat message to send</param>
        /// <param name="type">The chat type (usually Normal, Whisper or Shout)</param>
        /// <param name="cps">Characters per second rate for chatting</param>
        public static void Chat(BotClient client, string message, ChatType type, int cps)
        {
            Random rand = new Random();
            int characters = 0;
            bool typing = true;

            // Start typing
            try
            {
                client.Self.Chat(String.Empty, 0, ChatType.StartTyping);
                client.Self.AnimationStart(Animations.TYPE, false);

                int messageLength = message.Length;
                if (messageLength > 60)
                {
                    messageLength = 60;
                }
                while (characters < messageLength)
                {
                    if (!typing)
                    {
                        // Start typing again
                        client.Self.Chat(String.Empty, 0, ChatType.StartTyping);
                        client.Self.AnimationStart(Animations.TYPE, false);
                        typing = true;
                    }
                    else
                    {
                        // Randomly pause typing
                        if (rand.Next(10) >= 9)
                        {
                            client.Self.Chat(String.Empty, 0, ChatType.StopTyping);
                            client.Self.AnimationStop(Animations.TYPE, false);
                            typing = false;
                        }
                    }

                    // Sleep for a second and increase the amount of characters we've typed
                    System.Threading.Thread.Sleep(1000);
                    characters += cps;
                }

                // Send the message
                client.Talk(message, 0, type);
            }
            finally
            {
                // Stop typing
                client.Self.Chat(String.Empty, 0, ChatType.StopTyping);
                client.Self.AnimationStop(Animations.TYPE, false);
            }
        }
        public void TalkActive(string message)
        {
            Chat(client, message, ChatType.Normal, 20);
        }
        private string AddedToNextResponse = "";// new StringWriter();
        private string firstUser = null;
        private string lastKnownUser = null;
        public static bool logAimlToClient = false;

        private void AddAnimToNextResponse(string s)
        {
            if (AddedToNextResponse.Contains(s)) return;
            AddedToNextResponse += "\r\n" + s;
        }
        public SUnifiable AIMLInterp(string input)
        {
            return AIMLInterp(input, MyUser);
        }
        public SUnifiable AIMLInterp(string input, string myUser)
        {
            return AIMLInterp(input, GetMyUser(myUser));
        }
        public SUnifiable AIMLInterp(string input, User myUser)
        {
            if (MyBot.useServitor)
            {
                AddedToNextResponse = "";
                MyBot.DefaultPredicates.updateSetting("name", myUser.UserName);
                MyBot.updateRTP2Sevitor(myUser);
                MyBot.servitor.curBot.sayProcessor = new sayProcessorDelegate(TalkActive);

                string answer = MyBot.servitor.respondToChat(input);
                SUnifiable result = answer;
                if (result == null)
                {
                    DLRConsole.DebugWriteLine("-no-response- for -" + input + "-");
                    return null;
                }
                String append = AddedToNextResponse.ToString().Trim();
                if (append.Length > 0)
                {
                    result = AddToResult(result, append);
                }
                MyBot.updateServitor2RTP(myUser);
                return result.Replace("ISYOURFAV", " IS YOUR FAVORITE").Replace("  ", " ");

            }


            var old = AddedToNextResponse;
            AddedToNextResponse = "";
            try
            {
                SUnifiable result = AIMLInterp0(input, myUser);
                if (result == null)
                {
                    DLRConsole.DebugWriteLine("-no-response- for -" + input + "-");
                    return null;
                }
                String append = AddedToNextResponse.ToString().Trim();
                if (append.Length > 0)
                {
                    result = AddToResult(result, append);
                }
                return result.Replace("ISYOURFAV", " IS YOUR FAVORITE").Replace("  ", " ");
            }
            finally
            {
                AddedToNextResponse = old;
            }
        }

        private SUnifiable AddToResult(SUnifiable unifiable, string[] splts)
        {
            if (splts.Length == 0) return unifiable;
            if (splts.Length == 1) return AddToResult1(unifiable, splts[0]);
            foreach (var s in splts)
            {
                unifiable = AddToResult1(unifiable, s);
            }
            return unifiable;
        }

        private string AddToResult1(string unifiable, string s)
        {
            if (String.IsNullOrEmpty(s)) return unifiable;
            s = s.Trim();
            if (s.Length == 0) return unifiable;
            if (s.StartsWith("unknown animation "))
            {
                return AddToResult1(unifiable, asAnim(s.Substring("unknown animation ".Length)));
            }
            if (s.StartsWith("Start anim "))
            {
                return AddToResult1(unifiable, asAnim(s.Substring("Start anim ".Length)));
            }
            if (s.StartsWith("Stop anim "))
            {
                return AddToResult1(unifiable, asAnim(s.Substring("Stop anim ".Length)));
            }
            if (s.StartsWith("Ran "))
            {
                return unifiable;
            }
            return "" + unifiable + "\r\n " + s;
        }

        private string asAnim(string a)
        {
            return "\n<!-- Begin Meta !-->\n" + a.ToLower().Trim() + "\n<!-- End Meta !-->\n";
        }

        private SUnifiable AddToResult(SUnifiable unifiable, string s)
        {
            String[] splts = s.Split(new string[] { "\r", "\n", "\r\n" }, StringSplitOptions.RemoveEmptyEntries);
            return AddToResult(unifiable, splts);
        }

        public SUnifiable AIMLInterp0(string input, User myUser)
        {
            // set a global
            MyUser = myUser;
            if (input == null) return SUnifiable.Empty;
            input = input.Trim().Replace("  ", " ");
            if (string.IsNullOrEmpty(input)) return SUnifiable.Empty;
            string removeName = RemoveNameFromString(input);
            string myName = GetName().ToLower();
            if (!string.IsNullOrEmpty(removeName))
            {
                if (!myName.Contains(removeName.ToLower())) return SUnifiable.Empty;
                input = input.Substring(removeName.Length);
            }
            else
            {
                if (input.Contains(" "))
                {
                    string[] split = input.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                    if (myName.StartsWith(split[0].ToLower()))
                    {
                        input = string.Join(" ", split, 1, split.Length - 1);
                    }

                }
            }
            input = input.Trim();
            if (input.Length == 0)
            {
                return SUnifiable.Empty;
            }
            if (MyBot == null)
            {
                DLRConsole.DebugWriteLine(GetModuleName() + ": not Bot is instenaced yet!!");
                return "";
            }
            var r = MyBot.MakeRequestToBot(input, MyUser);
            r.IsTraced = true;
            Result res = MyBot.ChatWithRequest(r);
            string useOut = AltBot.CleanupCyc(res.Output);
            if (NeverSay(useOut)) return null;
            return useOut;
        }

        private bool NeverSay(string useOut)
        {
            return (useOut != null && useOut.Contains("RANDOM TOPIC."));
        }

        public SUnifiable AIMLInterpScored(string input, User myUser, out double scored)
        {
            scored = 0.0;
            // set a global
            MyUser = myUser;
            if (input == null) return SUnifiable.Empty;
            input = input.Trim().Replace("  ", " ");
            if (string.IsNullOrEmpty(input)) return SUnifiable.Empty;
            string removeName = RemoveNameFromString(input);
            string myName = GetName().ToLower();
            if (!string.IsNullOrEmpty(removeName))
            {
                if (!myName.Contains(removeName.ToLower())) return SUnifiable.Empty;
                input = input.Substring(removeName.Length);
            }
            else
            {
                if (input.Contains(" "))
                {
                    string[] split = input.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                    if (myName.StartsWith(split[0].ToLower()))
                    {
                        input = string.Join(" ", split, 1, split.Length - 1);
                    }

                }
            }
            input = input.Trim();
            if (input.Length == 0)
            {
                return SUnifiable.Empty;
            }
            if (MyBot == null)
            {
                DLRConsole.DebugWriteLine(GetModuleName() + ": not Bot is instenaced yet!!");
                return "";
            }
            var r = MyBot.MakeRequestToBot(input, MyUser);
            if (!r.GraphsAcceptingUserInput)
            {
                return "";
            }
            r.IsTraced = true;
            Result res = MyBot.ChatWithRequest(r);
            scored = res.Score;
            string useOut = AltBot.CleanupCyc(res.Output);
            return useOut;
        }

        private string RemoveNameFromString(string input)
        {
            int f = 0;
            int indexOf = input.IndexOf(' ', 0);
            if (indexOf == -1) return null;
            indexOf = input.IndexOf(' ', indexOf);
            if (indexOf == -1) indexOf = input.IndexOf(',', indexOf);
            if (indexOf == -1) return null;
            String removeName = input.Substring(0, indexOf); //get the name proposal
            indexOf = removeName.IndexOf(' ', indexOf); // get first space
            if (indexOf == -1) return null; // need at least one space
            indexOf = removeName.IndexOf(' ', indexOf); //get second space
            if (indexOf != -1) return null; // two spaces .. not a name
            if (WorldSystem.GetUserID(removeName) != UUID.Zero) return removeName;
            return null;
        }

        public override string GetModuleName()
        {
            return "AIMLBotModule";
        }

        public override void Dispose()
        {
            writeLock.Dispose();
            //todo throw new NotImplementedException();
        }

        public User MyUser
        {
            get
            {
                if (_MyUser == null)
                {
                    if (MyBotNullWarning())
                    {
                        return null;
                    }
                    _MyUser = MyBot.LastUser;
                }
                return _MyUser;
            }
            set { _MyUser = value ?? MyUser; }
        }

        public Bot MyBot
        {
            get
            {
                if (_MyBot == null)
                {
                    if (MyBotNullWarning())
                    {
                        return null;
                    }
                }
                return _MyBot;
            }
            set { _MyBot = value; }
        }

        public void SetDefaultUser(string user)
        {
            MyUser = GetMyUser(user);
        }

        public void RenameUser(string old, string newUser)
        {
            if (MyBotNullWarning()) return;
            MyBot.RenameUser(old, newUser);
        }

        public bool SameUser(string old, string next)
        {
            old = old ?? "";
            next = next ?? "";
            old = old.ToLower().Trim();
            next = next.ToLower().Trim();
            if (MyBotNullWarning()) return old == next;
            return MyBot.SameUser(old, next);
        }

        internal bool DoBotDirective(string[] args, UUID fromAgentID, OutputDelegate writeLine)
        {
            string s = args[0];
            if (s == "@wait")
            {
                return true;
            }
            if (s == "on" || s == "@on")
            {
                RespondToChatByDefaultAllUsers = true;
                SetChatOnOff(String.Join(" ", args, 1, args.Length - 1), true);
                writeLine("WorldObjects.RespondToChatByDefaultAllUsers = true;");
                return true;
            }
            if (s == "off" || s == "@off")
            {
                RespondToChatByDefaultAllUsers = false;
                SetChatOnOff(String.Join(" ", args, 1, args.Length - 1), false);
                writeLine("WorldObjects.RespondToChatByDefaultAllUsers = false;");
                return true;
            }
            if (MyBotNullWarning())
            {
                return false;
            }
            string stringJoin = String.Join(" ", args, 0, args.Length);
            if (MyBotNullWarning()) return false;
            return MyBot.BotDirective(MyUser, stringJoin, writeLine);
        }

        private bool MyBotNullWarning()
        {
            if (_MyBot != null) return false;
            DLRConsole.DebugWriteLine("WARNING! MyBOt NULL");
            client.WriteLine("WARNING! MyBOt NULL");
            return true;
        }

        /// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        public class ProvideAIMLVars : ICollectionProvider
        {
            public Bot MyBot
            {
                get { return PluginModule.MyBot; }
            }
            public ProvideAIMLVars(WorldObjectsForAimLBot param1)
            {
                PluginModule = param1;
            }
            public WorldObjectsForAimLBot PluginModule { get; set; }

            #region Implementation of ITreeable

            public string NameSpace
            {
                get { return MyBot.BotID;  }
            }

            public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
            {
                return MyBot.BotAsUser.Predicates.SettingNames(requester, depth);
            }

            #endregion

            #region Implementation of ICollectionProviderSettable

            public void SetValue(ICollectionRequester requester, string name, object value)
            {
                MyBot.BotAsUser.Predicates.SetValue(requester, name, value);
            }

            public bool AcceptsNewKeys
            {
                get { return true;  }
            }

            public ICollection GetGroup(ICollectionRequester requester, string name)
            {
                SUnifiable v = null;
                v = MyBot.BotAsUser.Predicates.grabSetting(name);
                if (v == null)
                {
                    if (MyBot != null) v = MyBot.GlobalSettings.grabSetting(name);
                    if (v == null) return null;
                }
                if (SUnifiable.IsNullOrEmpty(v)) return null;
                if (name.ToString() == v.ToString())
                {
                    return null;
                }
                var list = new List<string>();
                list.Add(v);
                return list;
            }

            #endregion
        }
        public class ProvideWorldVars : ISettingsDictionary
        {
            public bool IsTraced { get; set; }

            public ProvideWorldVars(WorldObjectsForAimLBot param1)
            {
                PluginModule = param1;
            }
            public WorldObjectsForAimLBot PluginModule { get; set; }

            public static bool FakeClientVars = false;

            public BotClient client
            {
                get { return PluginModule.client; }
            }

            public WorldObjects WorldSystem
            {
                get { return PluginModule.WorldSystem; }
            }
            public Bot MyBot
            {
                get { return PluginModule.MyBot; }
            }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="requester"></param>
            /// <param name="depth"></param>
            /// <returns></returns>        
            public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
            {
                //get
                {
                    return WorldSystem.GroupNames;
                }
            }


            public ICollection GetGroup(ICollectionRequester requester, string name)
            {
                return ScriptManager.GetGroup(requester, NameSpace, name);
            }

            public bool addSetting(string name, Unifiable value)
            {
                return ScriptManager.AddSetting(client, NameSpace, name, FromUnifiable(name, value));
            }

            public bool removeSetting(string name)
            {
                return ScriptManager.AddSetting(client, NameSpace, name, null);
            }

            public bool updateSetting(string name, Unifiable value)
            {
                return ScriptManager.AddSetting(client, NameSpace, name, FromUnifiable(name, value));
            }

            private object FromUnifiable(string named, Unifiable unifiable)
            {
                string ret = unifiable.AsString();
                CheckName(named);
                return ret;
            }

            private void CheckName(string named)
            {
                if (!named.ToLower().Contains("current")) return;
                named.ToLower();
            }

            public Unifiable grabSetting(string name)
            {
                CheckName(name); 
                if (FakeClientVars) if (name == "cogvar") return "botmody";
                int argsUsed;
                ICollection v = WorldSystem.ResolveCollection(name.ToLower(), out argsUsed, PluginModule.provideAIMLVars);
                if (v == null) return String.Empty;
                if (v.Count == 0) return SUnifiable.Empty;
                SUnifiable uu = null;
                int c = 0;
                List<Unifiable> List = new List<Unifiable>();
                foreach (var u in v)
                {
                    c++;
                    uu = ObjectUnifiable(u);
                    List.Add(uu);
                }
                if (c == 1) return List[0];
                if (List.Count > 5) List.RemoveRange(0, List.Count - 5);
                return new BestUnifiable(List);
            }

            private SUnifiable ObjectUnifiable(object o)
            {
                if (o is SimObject) o = ((SimObject)o).ID;
                //            if (o is SimPosition) o = ((SimPosition) o).GlobalPosition;
                return new StringUnifiable(o.ToString());
            }

            public bool containsLocalCalled(string name)
            {
                CheckName(name);
                name = Parser.ToKey(name);
                if (FakeClientVars) return name == "cogvar";
                var provs = ScriptManager.GetProviders(client, NameSpace);
                foreach (ICollectionProvider prov in provs)
                {
                    if (PluginModule.provideAIMLVars == prov) continue;
                    foreach (var sn in prov.SettingNames(client,1))
                    {
                        if (Parser.ToKey(sn) == name)
                        {
                            return true;
                        }
                    }
                }
                return false;
            }
            public bool containsSettingCalled(string name)
            {
                CheckName(name);
                if (FakeClientVars) return name == "cogvar";
                int argsUsed;
                var v = WorldSystem.ResolveCollection(name.ToLower(), out argsUsed, PluginModule.provideAIMLVars);
                return (v != null && v.Count > 0);
            }

            public string NameSpace
            {
                get
                {
                    if (FakeClientVars) return "botmod";
                    return client.GetName();
                }
            }
        }
    }
}
