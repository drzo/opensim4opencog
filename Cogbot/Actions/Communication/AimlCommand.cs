using System;
using System.Collections.Generic;
using System.Threading;
using RTParser;
using cogbot.Actions;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Utilities;


namespace cogbot.Listeners
{

    public class AimlCommand : Command
    {
        public AimlCommand(BotClient testClient)
        {
            Name = "aiml";
            Description = "Usage: aiml [...text]..";
            Category = CommandCategory.Communication;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length == 0) return "Usage: aiml [[on|off]|text]";
            string s = args[0].ToLower();
            if (s == "on")
            {
                WorldSystem.RespondToChatByDefaultAllUsers = true;
                WorldSystem.SetChatOnOff(String.Join(" ", args, 1, args.Length - 1), true);
                return "WorldObjects.RespondToChatByDefaultAllUsers = true;";
            }
            else
                if (s == "off")
                {
                    WorldSystem.RespondToChatByDefaultAllUsers = false;
                    WorldSystem.SetChatOnOff(String.Join(" ", args, 1, args.Length - 1), false);
                    return "WorldObjects.RespondToChatByDefaultAllUsers = false;";
                }
            string joined = String.Join(" ", args);
            return WorldSystem.AIMLInterp(joined);
        }
    }
    public partial class WorldObjects : DebugAllEvents
    {

        object BotExecHandler(string cmd, User user)
        {
            return client.ExecuteCommand(cmd);
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
        void InitConsoleBot()
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
                MyBot.outputDelegate = output;
                // wont get here unless there was no problem
                client.Self.OnChat += AIML_OnChat;
                client.Self.OnInstantMessage += AIML_OnInstantMessage;
                client.Network.OnLogin += AIML_OnLogin;
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

        public void AIML_OnInstantMessage(InstantMessage im, Simulator simulator)
        {
            if (im.FromAgentName == TheSimAvatar.theAvatar.Name) return;
            User myUser = GetMyUser(im.FromAgentName);

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
                                //if (im.GroupIM) ; //todo
                                foreach (string ting in resp.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries))
                                {
                                    client.Self.InstantMessage(im.FromAgentID, ting.Trim(), im.IMSessionID);
                                }
                            })).Start();

        }

        /// <summary>
        ///  false = wont respond to user until they say something like "turn chat on" 
        ///  See next function to change the keywords
        /// </summary>
        public bool RespondToChatByDefaultAllUsers = false;
        private void AIML_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position)
        {

            if (String.IsNullOrEmpty(message) || message.Length < 3) return;
            if (fromname == TheSimAvatar.theAvatar.Name) return;
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
            (new Thread(() => // this can be long running
                            {
                                string resp = AIMLInterp(message, myUser);
                                if (String.IsNullOrEmpty(resp)) return;
                                if (Environment.TickCount - myUser.LastResponseGivenTime <
                                    (60000 / myUser.MaxRespondToChatPerMinute))
                                {
                                    output("AIML_OnChat Reply is too fast: " + resp);
                                    return; //too early to respond.. but still listened
                                }
                                if (!myUser.RespondToChat)
                                {
                                    output("AIML_OnChat Reply is quietly: " + resp);
                                    return;
                                }
                                foreach (string ting in resp.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries))
                                {
                                    Realism.Chat(client, ting.Trim(), type, 6);
                                }
                                myUser.LastResponseGivenTime = Environment.TickCount;
                            })).Start();
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
    }


}

