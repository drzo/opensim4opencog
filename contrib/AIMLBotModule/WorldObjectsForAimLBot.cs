using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using RTParser;
using cogbot;

namespace AIMLBotModule
{
    public class WorldObjectsForAimLBot : WorldObjectsModule
    {
        /// <summary>
        ///  false = wont respond to user until they say something like "turn chat on" 
        ///  See next function to change the keywords
        /// </summary>
        public bool RespondToChatByDefaultAllUsers = false;
        /// <summary>
        /// Respond to group chat
        /// </summary>
        public static bool RespondToGroup = true;
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
        /// Move towards interesting objects
        /// </summary>
        public static bool UseAttention = false;
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

        /// <summary>
        /// REgister the Lisp Version of TalkToObject
        /// </summary>
        public static bool RegisterTalkToCmd = true;
        readonly static object RegisterTalkToCmdLock = new object();


        object BotExecHandler(string cmd, User user)
        {
            User prev = MyUser;
            try
            {
                MyUser = user;
                using (StringWriter sw = new StringWriter())
                {
                    string s = client.ExecuteCommand(cmd, sw.WriteLine);
                    return String.Format("{0}{1}", sw, s);
                }
            }
            finally
            {
                MyUser = prev;

            }
        }

        object LispExecHandler(string cmd, User user)
        {
            User prev = MyUser;
            try
            {
                MyUser = user;
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
                Console.WriteLine("" + e);
            }
        }
        private void TalkToObject0(SimActor av, SimObject obj)
        {
            string objName = obj.GetName();
            StringChat(String.Format("{0}, {1}", objName, AIMLInterp("RANDOM PICKUP LINE", GetMyUser(objName))));
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
                MyBot.AddExcuteHandler("bot", BotExecHandler);
                MyBot.AddExcuteHandler("lisp", LispExecHandler);
                MyBot.loadSettings();
                //MyBot.GlobalSettings.addSetting("name", client.BotLoginParams.FirstName+ " " + client.BotLoginParams.LastName);
                MyUser = new User(Unifiable.Create("AIMLInterp"), MyBot);
                MyBot.isAcceptingUserInput = false;
                MyBot.loadAIMLFromFiles();
                MyBot.isAcceptingUserInput = true;
                MyBot.outputDelegate = WriteLine;
                // wont get here unless there was no problem
                client.Self.OnChat += AIML_OnChat;
                client.Self.OnInstantMessage += AIML_OnInstantMessage;
                client.Network.OnLogin += AIML_OnLogin;
                client.Friends.OnFriendshipOffered += AIML_OnFriendshipOffered;
                client.Avatars.OnPointAt += AIML_OnPointAt;
                client.Avatars.OnLookAt += AIML_OnLookAt;
                client.Avatars.OnEffect += AINL_OnEffect;

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

        private void AINL_OnEffect(EffectType type, UUID sourceid, UUID targetid, Vector3d targetpos, float duration, UUID id)
        {
            if (sourceid == client.Self.AgentID) return;
            if (type == EffectType.LookAt) return;
            SetInterest(sourceid, targetid, false);
        }

        private void AIML_OnLookAt(UUID sourceid, UUID targetid, Vector3d targetpos, LookAtType looktype, float duration, UUID id)
        {
            if (sourceid == client.Self.AgentID) return;
            //if (targetid==client.Self.AgentID) SetInterest(sourceid, targetid, false);
        }

        private void AIML_OnPointAt(UUID sourceid, UUID targetid, Vector3d targetpos, PointAtType pointtype, float duration, UUID id)
        {
            if (PointAtType.None == pointtype) return;
            if (sourceid == client.Self.AgentID) return;
            SetInterest(sourceid, targetid, true);
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
            user.Predicates.addSetting("it", targetid.ToString());
            user.Predicates.addSetting("what", targetid.ToString());
            user.Predicates.addSetting("object", targetid.ToString());
        }


        private void AIML_OnFriendshipOffered(UUID agentid, string agentname, UUID imsessionid)
        {
            if (AcceptFriends) client.Friends.AcceptFriendship(agentid, imsessionid);
            //else client.Friends.DeclineFriendship(agentid, imsessionid);
        }

        private void AIML_OnLogin(LoginStatus login, string message)
        {
            if (login == LoginStatus.Success)
            {
                MyBot.GlobalSettings.addSetting("name",
                    String.Format("{0} {1}", client.BotLoginParams.FirstName, client.BotLoginParams.LastName));
                MyBot.GlobalSettings.addSetting("firstname", client.BotLoginParams.FirstName);
                MyBot.GlobalSettings.addSetting("lastname", client.BotLoginParams.LastName);
                client.WorldSystem.TheSimAvatar["AIMLBotModule"] = this;
                client.WorldSystem.TheSimAvatar["MyBot"] = MyBot;
                client.InternType(this.GetType());
            }
        }

        public void SetChatOnOff(string username, bool value)
        {
            MyBot.SetChatOnOff(username, value);
        }

        private User GetMyUser(string fromname)
        {
            bool newlyCreated;
            User user = MyBot.FindOrCreateUser(fromname, out newlyCreated);
            if (newlyCreated) user.RespondToChat = RespondToChatByDefaultAllUsers;
            return user;
        }

        public string GetName()
        {
            return client.GetName();
        }

        public void AIML_OnInstantMessage(InstantMessage im, Simulator simulator)
        {
            if (im.FromAgentName == GetName()) return;
            if (im.FromAgentName == "System" || im.FromAgentName == "Second Life") return;
            User myUser = GetMyUser(im.FromAgentName);
            myUser.Predicates.addSetting("host", im.FromAgentID.ToString());
            // myUser.Predicates.addObjectFields(im);

            bool UseThrottle = im.GroupIM;
            string groupName = null;
            if (im.Dialog == InstantMessageDialog.StartTyping || im.Dialog == InstantMessageDialog.StopTyping)
            {
                return;
            }
            UUID groupID = UUID.Zero;
            if (im.GroupIM)
            {
                Group group;
                client.Groups.GroupName2KeyCache.ForEach(delegate(KeyValuePair<UUID, string> kv)
                                                             {
                                                                 if (im.FromAgentID == kv.Key)
                                                                 {
                                                                     groupName = kv.Value;
                                                                     groupID = kv.Key;
                                                                 }
                                                             });

                WriteLine("Group IM {0}", groupName);
                if (!myUser.RespondToChat && !RespondToChatByDefaultAllUsers) return;
            }

            //UpdateQueue.Enqueue(() => SendNewEvent("on-instantmessage", , im.Message, im.ToAgentID,
            //                           im.Offline, im.IMSessionID, im.GroupIM, im.Position, im.Dialog,
            //                           im.ParentEstateID));


            string message = im.Message;
            if (message == "typing") return;
            if (message == "") return;
            (new Thread(() => // this can be long running
                            {
                                string resp = AIMLInterp(message, myUser);
                                // if (im.Offline == InstantMessageOnline.Offline) return;
                                if (String.IsNullOrEmpty(resp)) return;
                                if (UseThrottle)
                                {
                                    if (Environment.TickCount - myUser.LastResponseGivenTime <
                                        (60000 / myUser.MaxRespondToChatPerMinute))
                                    {
                                        WriteLine("AIML_OnChat Reply is too fast: " + resp);
                                        return; //too early to respond.. but still listened
                                    }
                                }
                                UseRealism = true;
                                foreach (string ting in resp.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries))
                                {
                                    string tsing = ting.Trim();
                                    if (tsing.Length > 1000)
                                    {
                                        tsing = tsing.Substring(0, 1000);
                                    }
                                    Thread.Sleep(100);
                                    if (im.GroupIM)
                                    {
                                        if (!myUser.RespondToChat) return;
                                        if (!RespondToGroup) return;

                                        WriteLine("InstantMessageGroup {0} {1} {2}",
                                                          im.FromAgentName + "/" + groupName, im.FromAgentID,
                                                          ting.Trim());
                                        client.Self.InstantMessageGroup(GetName(), im.FromAgentID, tsing);
                                    }
                                    else
                                    {
                                        // todo maybe send a typing message for the UseRealism
                                        if (UseRealism)
                                        {
                                            client.Self.InstantMessage(GetName(), im.FromAgentID, "typing", im.IMSessionID,
                                                                       InstantMessageDialog.StartTyping,
                                                                       InstantMessageOnline.Offline,
                                                                       client.Self.SimPosition,
                                                                       UUID.Zero, Utils.EmptyBytes);
                                            Thread.Sleep(1900);
                                            client.Self.InstantMessage(GetName(), im.FromAgentID, "typing", im.IMSessionID,
                                                                       InstantMessageDialog.StopTyping,
                                                                       InstantMessageOnline.Online,
                                                                       client.Self.SimPosition,
                                                                       UUID.Zero, Utils.EmptyBytes);

                                        }

                                        WriteLine("InstantMessage {0} {1} {2}", im.FromAgentName,
                                                          im.FromAgentID, ting.Trim());
                                        client.Self.InstantMessage(im.FromAgentID, tsing, im.IMSessionID);
                                    }
                                    UseRealism = false;

                                }
                                myUser.LastResponseGivenTime = Environment.TickCount;
                            })).Start();

        }

        public WorldObjectsForAimLBot(BotClient testClient)
            : base(testClient)
        {
        }

        private DateTime lastFollow = DateTime.Now;

        private void AIML_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position)
        {

            if (String.IsNullOrEmpty(message) || message.Length < 2) return;
            if (sourcetype == ChatSourceType.System) return;
            if (fromname == GetName()) return;
            if (string.IsNullOrEmpty(fromname))
            {
                Primitive prim = WorldSystem.GetPrimitive(id, null);
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
            if (myUser.RespondToChat && message.Contains("chat off"))
            {
                myUser.RespondToChat = false;
                return;
            }

            if (!myUser.RespondToChat && !RespondToChatByDefaultAllUsers) return;

            UseRealism = true;

            (new Thread(() => // this can be long running
                            {
                                string resp = AIMLInterp(message, myUser);
                                if (String.IsNullOrEmpty(resp)) return;
                                if (Environment.TickCount - myUser.LastResponseGivenTime <
                                    (60000 / myUser.MaxRespondToChatPerMinute))
                                {
                                    WriteLine("AIML_OnChat Reply is too fast: " + resp);
                                    return; //too early to respond.. but still listened
                                }
                                if (!myUser.RespondToChat)
                                {
                                    WriteLine("AIML_OnChat Reply is quietly: " + resp);
                                    return;
                                }
                                StringChat(resp, type);
                                myUser.LastResponseGivenTime = Environment.TickCount;
                            })).Start();
        }

        public void StringChat(string resp)
        {
            StringChat(resp, ChatType.Normal);
        }
        private void StringChat(string resp, ChatType type)
        {
            foreach (string ting in resp.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries))
            {
                string sting = ting.Trim();
                if (UseRealism)
                    Chat(client, sting, type, 6);
                else client.Self.Chat(sting, 0, type);
                UseRealism = false;
            }
        }

        private bool MessageTurnsOnChat(string message)
        {
            message = message.ToLower();
            if (message.Contains("chat on")) return true;
            string n = client.Self.Name;
            if (string.IsNullOrEmpty(n)) return false;
            n = n.ToLower().Trim();
            if (n == "") return false;
            if (message.Contains(n)) return true;
            foreach (var c in n.Split(' '))
            {
                if (message.Contains(c)) return true;
            }
            return false;
        }

        private void AttendTo(string fromname, UUID fromID, PCode isAvatar)
        {
            if (!UseAttention) return;
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
            if (!talker.IsRegionAttached())
            {
                WriteLine("Talker: !IsRegionAttached " + talker);
                return;
            }
            SimActor a = WorldSystem.TheSimAvatar;
            if (!a.IsRegionAttached())
            {
                WriteLine("!IsRegionAttached " + talker);
                return;
            }
            if (Math.Abs(a.GetSimPosition().Z - talker.GetSimPosition().Z) > MaxZDistance)
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
            else if (dist > MaxDistance)
            {
                WriteLine("X,Y " + dist + " Too far to " + talker);
                return;
            }
            if (a.CurrentAction == null || a.CurrentAction is FollowerAction)
            {
                FollowerAction fa = new FollowerAction(a, talker);
                client.output("" + fa);
                a.CurrentAction = fa;
                lastFollow = DateTime.Now;
            }
        }

        public void WriteLine(string s, params object[] args)
        {
            Console.WriteLine("AIML BOT: " + s, args);
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
        public static void Chat(GridClient client, string message, ChatType type, int cps)
        {
            Random rand = new Random();
            int characters = 0;
            bool typing = true;

            // Start typing
            client.Self.Chat(String.Empty, 0, ChatType.StartTyping);
            client.Self.AnimationStart(Animations.TYPE, false);

            while (characters < message.Length)
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
            client.Self.Chat(message, 0, type);

            // Stop typing
            client.Self.Chat(String.Empty, 0, ChatType.StopTyping);
            client.Self.AnimationStop(Animations.TYPE, false);
        }

        public Unifiable AIMLInterp(string input)
        {
            return AIMLInterp(input, MyUser);
        }

        public Unifiable AIMLInterp(string input, User myUser)
        {
            string removeName = RemoveNameFromString(input);
            if (!string.IsNullOrEmpty(removeName))
            {
                if (!GetName().ToLower().Contains(removeName.ToLower())) return Unifiable.Empty;
                input = input.Substring(removeName.Length);
            }
            input = input.Trim();
            if (input.Length == 0)
            {
                return Unifiable.Empty;
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

        public override void ShutdownListener()
        {
            //todo throw new NotImplementedException();
        }
    }
}