using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using RTParser;
using cogbot;

namespace AIMLBotModule
{
    public class WorldObjectsForAimLBot : WorldObjectsModule
    {
        public static bool AcceptFriends = true;
        public static bool UseRealism = false;

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

        public RTPBot MyBot;
        public User MyUser;
        readonly Dictionary<string, User> BotUsers = new Dictionary<string, User>();
        public override void StartupListener()
        {
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
                SimEventSubscriber evtSub = new AIMLEventSubscriber(MyBot, this);
                client.AddBotMessageSubscriber(evtSub);
                while (false)
                {
                    Console.Write("You: ");
                    string input = Console.ReadLine();
                    Console.WriteLine("RTPBot: " + AIMLInterp(input, MyUser));
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
            }
        }

        private void WriteLine(string str)
        {
            Console.WriteLine(str);
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
                                                client.BotLoginParams.FirstName + " " + client.BotLoginParams.LastName);
                MyBot.GlobalSettings.addSetting("firstname", client.BotLoginParams.FirstName);
                MyBot.GlobalSettings.addSetting("lastname", client.BotLoginParams.LastName);
            }
        }

        public void SetChatOnOff(string username, bool value)
        {
            lock (BotUsers)
            {
                foreach (var u in BotUsers.Values)
                {
                    if (u.UserID.ToValue().Contains(username) || username.Contains(u.UserID))
                        u.RespondToChat = value;
                }
            }
        }

        private User GetMyUser(string fromname)
        {
            lock (BotUsers)
            {
                if (BotUsers.ContainsKey(fromname)) return BotUsers[fromname];
                User myUser = new User(fromname, MyBot);
                BotUsers[fromname] = myUser;
                AIMLInterp("My name is " + fromname, myUser);
                myUser.Predicates.addSetting("name", fromname);
                myUser.RespondToChat = RespondToChatByDefaultAllUsers;
                return myUser;
            }
        }

        public string GetName()
        {
            return client.GetName();
        }

        public void AIML_OnInstantMessage(InstantMessage im, Simulator simulator)
        {
            if (im.FromAgentName == GetName()) return;
            if (im.FromAgentName == "System" || im.FromAgentName=="Second Life") return;
            User myUser = GetMyUser(im.FromAgentName);
            bool UseThrottle = im.GroupIM;
            //if (im.GroupIM)
            //{
            //    string groupName = null;
            //    Group group;
            //    client.Groups.GroupName2KeyCache.ForEach(delegate(KeyValuePair<UUID, string> kv)
            //                                                 {
            //                                                     if (im.FromAgentID == kv.Key) groupName = kv.Value;
            //                                                 });
            //    //if (groupName == null) return;
            //}

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
                                foreach (string ting in resp.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries))
                                {
                                    Thread.Sleep(100);
                                    if (im.GroupIM)
                                    {
                                        client.Self.InstantMessageGroup(GetName(), im.FromAgentID, ting.Trim());
                                    }
                                    else
                                    {
                                        client.Self.InstantMessage(im.FromAgentID, ting.Trim(), im.IMSessionID);
                                    }
                                }
                                myUser.LastResponseGivenTime = Environment.TickCount;
                            })).Start();

        }

        /// <summary>
        ///  false = wont respond to user until they say something like "turn chat on" 
        ///  See next function to change the keywords
        /// </summary>
        public bool RespondToChatByDefaultAllUsers = false;

        public WorldObjectsForAimLBot(BotClient testClient)
            : base(testClient)
        {
        }

        private void AIML_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position)
        {

            if (String.IsNullOrEmpty(message) || message.Length < 3) return;
            if (fromname == GetName()) return;
            User myUser = GetMyUser(fromname);
            // todo hard coded to be changed
            if (!myUser.RespondToChat && (message.Contains("chat on") || client.Self.Name.Contains(message)))
            {
                myUser.RespondToChat = true;
                return;
            }
            if (myUser.RespondToChat && message.Contains("chat off"))
            {
                myUser.RespondToChat = false;
                return;
            }
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
                                foreach (string ting in resp.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries))
                                {
                                    string sting = ting.Trim();
                                    if (UseRealism)
                                        Chat(client, sting, type, 6);
                                    else client.Self.Chat(sting, 0, type);
                                    UseRealism = false;
                                }
                                myUser.LastResponseGivenTime = Environment.TickCount;
                            })).Start();
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
            Request r = new Request(input, myUser, MyBot);
            Result res = MyBot.Chat(r);
            return res.Output;
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