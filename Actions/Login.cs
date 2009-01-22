using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Login : Action
    {
        protected string firstName = "Eelke";
		protected string lastName = "Forder";
		protected string password = "geheim";

        public Login(BotClient Client)
            : base(Client)
        {
            helpString = "Login to Secondlife";
			usageString = "login <first name> <last name> <password>";
        }

		public override void acceptInput(string verb, Parser args)
        {
			//base.acceptInput(verb, args);
            string[] tokens = args.objectPhrase.Split(null);

            if ((tokens.Length!=1)&& (tokens.Length != 3))
            {
                WriteLine("Please enter login FirstName LastName and Password to login to the SL");
                return;
            }
            else
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
                if (!Client.Network.Connected && !Client.Network.LoginMessage.StartsWith("Logging"))
                {
                    Client.Settings.LOGIN_SERVER = Client.BotLoginParams.URI;// TextForm.SingleInstance.config.simURL; // "http://127.0.0.1:8002/";
///                    Client.Network.Login(Client.BotLoginParams.FirstName, Client.BotLoginParams.LastName, Client.BotLoginParams.Password, "OnRez", "UNR");
                    Client.Login();
                }
                else
                    WriteLine("You are already logged in.");
            }
		}
    }
}