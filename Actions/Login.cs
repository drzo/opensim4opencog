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
        
		public Login(TextForm parent)
            : base(parent)
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
                parent.output("Please enter login FirstName LastName and Password to login to the SL");
                return;
            }
            else
            {
                if (tokens.Length == 3)
                {
                    firstName = tokens[0];
                    lastName = tokens[1];
                    password = tokens[2];
                }
                else
                {
                 firstName = parent.config.firstName;// "Eelke";
		         lastName = parent.config.lastName; //"Forder";
		         password = parent.config.password; //"geheim";

                }
                if (!Client.Network.Connected)
                {
                    Client.Settings.LOGIN_SERVER = parent.config.simURL; // "http://127.0.0.1:8002/";
                    Client.Network.Login(firstName, lastName, password, "TextSL", "UNR");
                }
                else
                    parent.output("You are already logged in.");
            }
		}
    }
}