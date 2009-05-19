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
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
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
        readonly static Dictionary<string, User> BotUsers = new Dictionary<string, User>();
        void InitConsoleBot()
        {
            try
            {
                MyBot = new RTPBot();
                MyBot.AddExcuteHandler("bot", BotExecHandler);
                MyBot.AddExcuteHandler("lisp", LispExecHandler);
                MyBot.loadSettings();
                MyUser = new User("AIMLInterp", MyBot);
                MyBot.isAcceptingUserInput = false;
                MyBot.loadAIMLFromFiles();
                MyBot.isAcceptingUserInput = true;
                MyBot.outputDelegate = output;
                // wont get here unless there was no problem
                client.Self.OnChat += AIML_OnChat;
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

        /// <summary>
        ///  false = wont respond to user until they say something like "turn chat on" 
        ///  See next function to change the keywords
        /// </summary>
        static public bool RespondToChatByDefaultAllUsers = false;
        private void AIML_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position)
        {

            if (String.IsNullOrEmpty(message) || message.Length < 3) return;
            if (fromname == TheSimAvatar.theAvatar.Name) return;
            User myUser;
            if (!BotUsers.ContainsKey(fromname))
            {
                myUser = new User(fromname, MyBot);
                AIMLInterp("My name is " + fromname, myUser);
                myUser.Predicates.addSetting("name", fromname);
                BotUsers[fromname] = myUser;
                myUser.RespondToChat = RespondToChatByDefaultAllUsers;
            }
            else
            {
                myUser = BotUsers[fromname];
            }
            // todo hard coded to be changed
            if (message.Contains("chat on"))
            {
                myUser.RespondToChat = true;
                return;
            }
            if (message.Contains("chat off"))
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
                                Realism.Chat(client, resp, type, 6);
                                myUser.LastResponseGivenTime = Environment.TickCount;
                            })).Start();
        }

        public string AIMLInterp(string input)
        {
            return AIMLInterp(input, MyUser);
        }

        public string AIMLInterp(string input, User myUser)
        {
            Request r = new Request(input, myUser, MyBot);
            Result res = MyBot.Chat(r);
            return res.Output;
        }
    }


}

