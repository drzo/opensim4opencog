using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading;
using cogbot.Actions;
using cogbot.Listeners;
using cogbot.ScriptEngines;
using cogbot.TheOpenSims;
using OpenMetaverse;
using RTParser;
using cogbot;
using cogbot.Utilities;
using RTParser.Utils;
using Exception=System.Exception;
using Math=System.Math;
using String=System.String;
using Thread=System.Threading.Thread;
using PathSystem3D.Navigation;
using SUnifiable=System.String;

namespace AIMLBotModule
{
    public class WorldObjectsForAimLBot : WorldObjectsModule, ICollectionProvider, ISettingsDictionary
    {
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
        public static bool UseNameAttention = false;
        /// <summary>
        /// Turn towards interesting objects
        /// </summary>
        public static bool UseMessageTurnsOnChat = false;
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
                MyUser = request.user;
                StringWriter sw = new StringWriter();
                {

                    CmdResult s = client.ExecuteCommand(cmd, sw.WriteLine);
                    if (cmd.StartsWith("anim"))
                    {
                        AddAnimToNextResponse(sw.ToString());
                    }
                    return String.Format("{0}{1}", sw, s);
                }
            }
            finally
            {
                MyUser = prev;

            }
        }

        object BotExecHandlerNew(string cmd, Request request)
        {
            User prev = MyUser;
            try
            {
                MyUser = request.user;
                StringWriter sw = new StringWriter();
                {
                    CmdResult s;
                    if (cmd.StartsWith("anim") || cmd.StartsWith("thread anim 4"))
                    {
                        AddAnimToNextResponse(sw.ToString());
                        s = new CmdResult("", true);
                    }
                    else
                    {
                        s = client.ExecuteCommand(cmd, sw.WriteLine);
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
                MyUser = request.user;
                return client.evalLispString(cmd);
            }
            finally
            {
                MyUser = prev;

            }
        }

        static public void TalkToObject(SimActor av, SimObject obj)
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
        private void TalkToObject0(SimActor av, SimObject obj)
        {
            string objName = obj.GetName();
            MyUser = GetMyUser(objName);
            MyUser.RespondToChat = true;
            String str = String.Format("{0}, {1}", objName, AIMLInterp("RANDOM PICKUP LINE", MyUser)).Trim();
            while (str.EndsWith("?")) str = str.Substring(0, str.Length - 1).Trim();
            StringChat(str);
        }

        public RTPBot MyBot;
        public User MyUser;

        public override void StartupListener()
        {
            lock (RegisterTalkToCmdLock)
            {
                if (RegisterTalkToCmd)
                {
                    RegisterTalkToCmd = false;
                    SimTypeUsage u = SimTypeSystem.CreateTypeUsage("TalkToObject");
                    u.SpecifiedProperties.Add("LispScript");
                    u.LispScript = "(AIMLBotModule:WorldObjectsForAimLBot:TalkToObject TheBot TheTarget)";
                    u = SimTypeSystem.CreateTypeUsage("KissTheObject");
                    u.SpecifiedProperties.Add("LispScript");
                    u.LispScript = "(AIMLBotModule:WorldObjectsForAimLBot:TalkToObject TheBot TheTarget)";
                }
            }
            try
            {
                MyBot = new RTPBot();
                MyBot.isAcceptingUserInput = false;
                MyBot.AddExcuteHandler("bot", BotExecHandler);
                MyBot.AddExcuteHandler("lisp", LispExecHandler);
                MyBot.loadSettings();
                //MyBot.GlobalSettings.addSetting("name", client.BotLoginParams.FirstName+ " " + client.BotLoginParams.LastName);
                MyUser = new User("AIMLInterp", MyBot);
                MyUser.InsertProvider(new ParentProvider(() => this));
                MyBot.loadAIMLFromDefaults();
                MyBot.isAcceptingUserInput = true;
                MyBot.outputDelegate = WriteLine;
                LoadPersonalConfig();
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

                if (EventsToAIML)
                {
                    SimEventSubscriber evtSub = new AIMLEventSubscriber(MyBot, this);
                    client.AddBotMessageSubscriber(evtSub);
                }
                while (false)
                {
                    Console.Write("You: ");
                    string input = Console.ReadLine();
                    WriteLine("RTPBot: " + AIMLInterp(input, MyUser));
                }
            }
            catch (Exception e)
            {
                WriteLine("" + e);
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
            SetInterest(e.SourceID, e.TargetID, false);
        }

        private void AIML_OnLookAt(object sender, ViewerEffectLookAtEventArgs e)
        {
            if (e.SourceID == client.Self.AgentID) return;
            //if (targetid==client.Self.AgentID) SetInterest(sourceid, targetid, false);
        }

        private void AIML_OnPointAt(object sender, ViewerEffectPointAtEventArgs e)
        {
            if (PointAtType.None == e.PointType) return;
            if (e.SourceID == client.Self.AgentID) return;
            SetInterest(e.SourceID, e.TargetID, true);
        }

        private void SetInterest(UUID sourceid, UUID targetid, bool forced)
        {
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
            myUserPredicates.addSetting("object", targetid.ToString());
        }


        private void AIML_OnFriendshipOffered(object sender,FriendshipOfferedEventArgs e)
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
            client.InternType(this.GetType());
            string myName = GetName().Trim();
            String[] sname = myName.Split(' ');
            MyBot.GlobalSettings.addSetting("name", String.Format("{0}", myName));
            MyBot.GlobalSettings.addSetting("firstname", sname[0]);
            MyBot.GlobalSettings.addSetting("lastname", sname[1]);
            MyBot.GlobalSettings.addSetting("master", client.MasterName);
            client.WorldSystem.TheSimAvatar["AIMLBotModule"] = this;
            client.WorldSystem.TheSimAvatar["MyBot"] = MyBot;
            client.WorldSystem.AddGroupProvider(this);
           

            LoadPersonalConfig();

        }

        private void LoadPersonalConfig()
        {
            if (!NeedPersonalConfig) return;
            string myName = GetName().ToLower().Trim().Replace(" ", "_");
            if (string.IsNullOrEmpty(myName)) return;
            NeedPersonalConfig = false;
            LoadPersonalDirectories(myName);
        }

        public void LoadPersonalDirectories(string myName)
        {
            bool loaded = false;
            string file = Path.Combine("config", myName);
            if (Directory.Exists(file))
            {
                WriteLine("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                MyBot.loadSettings(Path.Combine(file, "Settings.xml"));
            }
            file = Path.Combine("aiml", myName);
            if (Directory.Exists(file))
            {
                WriteLine("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                MyBot.isAcceptingUserInput = false;
                MyBot.loadAIMLFromURI(file, MyBot.BotAsRequest);
                MyBot.isAcceptingUserInput = true;
            }

            file = Path.Combine(myName, "config");
            if (Directory.Exists(file))
            {
                WriteLine("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                MyBot.loadSettings(Path.Combine(file, "Settings.xml"));
            }

            file = Path.Combine(myName, "aiml");
            if (Directory.Exists(file))
            {
                WriteLine("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                MyBot.isAcceptingUserInput = false;
                MyBot.loadAIMLFromURI(file, MyBot.BotAsRequest);
                MyBot.isAcceptingUserInput = true;
            }

            if (!loaded)
            {
                file = myName;
                if (Directory.Exists(file))
                {
                    WriteLine("LoadPersonalDirectories: '{0}'", file);
                    loaded = true;
                    MyBot.isAcceptingUserInput = false;
                    MyBot.loadAIMLFromURI(file, MyBot.BotAsRequest);
                    MyBot.isAcceptingUserInput = true;
                }
            }
            if (!loaded)
            {
                WriteLine("Didnt find personal directories with stem: '{0}'", myName);
            }
        }

        public void SetChatOnOff(string username, bool value)
        {
            MyBot.SetChatOnOff(username, value);
        }

        private User GetMyUser(string fromname)
        {
            if (String.IsNullOrEmpty(fromname))
            {
                fromname = "UNKNOWN_PARTNER";
            }
            bool newlyCreated;
            User user = MyBot.FindOrCreateUser(fromname, out newlyCreated);
            if (newlyCreated)
            {
                user.InsertProvider(() => this);
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
            Console.WriteLine("InstantMessage=" + im.Dialog);
            //Console.WriteLine("FromAgentID=" + WorldSystem.GetObject(im.FromAgentID));
            object toObject = WorldSystem.GetObject(im.ToAgentID);
            //if (toObject!=null) Console.WriteLine("ToAgentID=" + toObject.GetType());
            object sessionObject = WorldSystem.GetObject(im.IMSessionID);
            //if (sessionObject != null) Console.WriteLine("SessionID=" + sessionObject.GetType());            

            
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
            if (im.Dialog == InstantMessageDialog.GroupNotice || im.Dialog==InstantMessageDialog.SessionSend)
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
                if (g!=null)
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
            RunTask(() => // this can be long running
                        {
                            UUID toSession = im.ToAgentID ^ im.IMSessionID;
                            string resp = AIMLInterp(message, myUser);
                            // if (im.Offline == InstantMessageOnline.Offline) return;
                            if (String.IsNullOrEmpty(resp)) return;
                            if (UseThrottle)
                            {
                                if (Environment.TickCount - myUser.LastResponseGivenTime <
                                    (60000 / myUser.MaxRespondToChatPerMinute))
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
                            myUser.LastResponseGivenTime = Environment.TickCount;
                        }, "AIML_OnInstantMessage: " + myUser + ": " + message);
        }

        List<Thread> ThreadList
        {
            get
            {
                return WorldSystem.client.botCommandThreads;
            }
        }

        private void RunTask(ThreadStart action, string name)
        {
            Thread tr = new Thread(() =>
                                       {
                                           try
                                           {
                                               action();
                                           }
                                           finally
                                           {
                                               ThreadList.Remove(Thread.CurrentThread);
                                           }
                                       }) {Name = name};
            ThreadList.Add(tr);
            tr.Start();
        }

        public WorldObjectsForAimLBot(BotClient testClient)
            : base(testClient)
        {
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

            RunTask(() => // this can be long running
                        {
                            string resp = AIMLInterp(message, myUser);
                            if (String.IsNullOrEmpty(resp)) return;
                            if (Environment.TickCount - myUser.LastResponseGivenTime <
                                (60000 / myUser.MaxRespondToChatPerMinute))
                            {
                                WriteLine("AIML_OnChat Reply is too fast {0}: {1}->{2}", myUser, message, resp);
                                
                                return; //too early to respond.. but still listened
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
                            myUser.LastResponseGivenTime = Environment.TickCount;
                        }, "AIML_OnChat: " + myUser + ": " + message);
        }

        public void HeardMyselfSay(UUID uuid, string message)
        {
            if (MyBot != null) MyBot.HeardSelfSay(message);
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
            if (Math.Abs(a.SimPosition.Z - talker.SimPosition.Z) > MaxZDistance)
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

        static readonly TaskQueueHandler writeLock = new TaskQueueHandler("AIMLBot Console Writer",0);
        public void WriteLine(string s, params object[] args)
        {
            if (args == null || args.Length == 0)
            {
                args = new object[] { s };
                s = "{0}";
            }
            if (Monitor.TryEnter(writeLock,1000))
            {
                writeLock.Enqueue(()=> Logger.DebugLog(string.Format(string.Format("[AIMLBOT] {0} {1}", GetName(), s), args)));
                Monitor.Exit(writeLock);
            } else
            {
              Console.WriteLine("cant even get a Enqueue! " + MethodInfo.GetCurrentMethod());  
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

        private string AddedToNextResponse = "";// new StringWriter();
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
            var old = AddedToNextResponse;
            AddedToNextResponse = "";
            try
            {
                SUnifiable result = AIMLInterp0(input, myUser);
                String append = AddedToNextResponse.ToString().Trim();
                if (append.Length>0)
                {
                    result = AddToResult(result, append);
                }
                return result;
            } finally
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
            Request r = new Request(input, myUser, MyBot);
            Result res = MyBot.Chat(r);
            return res.Output;
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

        public ICollection GetGroup(string name)
        {
            SUnifiable v = MyUser.Predicates.grabSetting(name);
            if (v == null)
            {
                v = MyBot.GlobalSettings.grabSetting(name);
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

        public bool addSetting(string name, Unifiable value)
        {
            return false;
        }

        public bool removeSetting(string name)
        {
            return false;
        }

        public bool updateSetting(string name, Unifiable value)
        {
            return false;
        }

        public Unifiable grabSetting(string name)
        {
            if (name == "botmod") return "botmody";
            int argsUsed;
            var v = WorldSystem.ResolveCollection(name.ToLower(), out argsUsed, this);
            if (v == null) return String.Empty;
            if (v.Count == 0) return SUnifiable.Empty;
            BestUnifiable us = new BestUnifiable();
            SUnifiable uu = null;
            int c = 0;
            foreach(var u in v)
            {
                c++;
                uu = ObjectUnifiable(u);
                us.List.Add(uu);
            }
            if (c == 1) return uu;
            return name;
            //return us;
        }

        private SUnifiable ObjectUnifiable(object o)
        {
            if (o is SimObject) o = ((SimObject)o).ID;
//            if (o is SimPosition) o = ((SimPosition) o).GlobalPosition;
            return new StringUnifiable(o.ToString());
        }

        public bool containsSettingCalled(string name)
        {
            return name == "botmod";
            int argsUsed;
            var v = WorldSystem.ResolveCollection(name.ToLower(), out argsUsed, this);
            return (v != null && v.Count > 0);

        }

        public string NameSpace
        {
            get { return "botmod"; }
        }

        public void SetDefaultUser(string user)
        {
            MyUser = GetMyUser(user);
        }
    }
}