using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Login : Action, BotSystemCommand
    {

        public Login(BotClient Client)
            : base(Client)
        {
            helpString = "Login to World Server";
            usageString = "login <first name> <last name> <password> [<simurl>] [<location>]";
        }

        public override string acceptInput(string verb, Parser args)
        {
            //base.acceptInput(verb, args);
            string[] tokens = args.objectPhrase.Split(null);

            BotClient Client = TheBotClient;
            //if ((tokens.Length != 1) && (tokens.Length != 3))
            //{
            //    return ("Please enter login FirstName LastName and Password to login to the SL");
            //}
            //else
            {
                if (tokens.Length > 0 && !String.IsNullOrEmpty(tokens[0]))
                {
                    Client.BotLoginParams.FirstName = tokens[0];
                }
                if (tokens.Length > 1)
                {
                    Client.BotLoginParams.LastName = tokens[1];
                }
                if (tokens.Length > 2)
                {
                    Client.BotLoginParams.Password = tokens[2];
                }
                if (tokens.Length > 3)
                {
                    Client.BotLoginParams.URI = tokens[3];
                }
                if (tokens.Length > 4)
                {
                    Client.BotLoginParams.Start = tokens[4];
                }
                if (!Client.Network.Connected && !Client.Network.LoginMessage.StartsWith("Logging"))
                {
                    Client.Settings.LOGIN_SERVER = TheBotClient.BotLoginParams.URI;// TextForm.SingleInstance.config.simURL; // "http://127.0.0.1:8002/";
                    ///                    Client.Network.Login(Client.BotLoginParams.FirstName, Client.BotLoginParams.LastName, Client.BotLoginParams.Password, "OnRez", "UNR");
                    Client.Login();
                }
                else
                    return ("You are already logged in.");
            }
            return "loging in...";
        }
    }
}