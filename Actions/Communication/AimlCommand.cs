using System;
using System.Collections.Generic;
using AIMLbot;
using cogbot.Actions;
using cogbot.TheOpenSims;
using OpenMetaverse;


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
        public Bot MyBot;
        private User MyUser;
        readonly static Dictionary<string, User> BotUsers = new Dictionary<string, User>();
        void InitConsoleBot()
        {
            try
            {
                MyBot = new Bot();
                MyBot.loadSettings();
                MyUser = new User("AIMLInterp", MyBot);
                MyBot.isAcceptingUserInput = false;
                MyBot.loadAIMLFromFiles();
                MyBot.isAcceptingUserInput = true;
                // wont get here unless there was no problem
                client.Self.OnChat += AIML_OnChat;
                while (false)
                {
                    Console.Write("You: ");
                    string input = Console.ReadLine();
                    Console.WriteLine("Bot: " + AIMLInterp(input, MyUser));
                }
            }
            catch (Exception)
            {
            }
        }


        private void AIML_OnChat(string message, ChatAudibleLevel audible, ChatType type, ChatSourceType sourcetype, string fromname, UUID id, UUID ownerid, Vector3 position)
        {

            if (String.IsNullOrEmpty(message) || message.Length < 3) return;
            if (fromname == TheSimAvatar.theAvatar.Name) return;
            User myUser;
            if (!BotUsers.ContainsKey(fromname))
            {
                myUser = new User(fromname, MyBot);
                AIMLInterp("My name is " + fromname, myUser);
                BotUsers[fromname] = myUser;
            }
            else
            {
                myUser = BotUsers[fromname];
            }
            string resp = AIMLInterp(message, myUser);
            client.Self.Chat(resp, 0, ChatType.Normal);
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

