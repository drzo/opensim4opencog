using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
#pragma warning disable 0168
    class Teleport : Action
    {
        public Teleport(BotClient Client)
            : base(Client)
        {
            helpString = "Teleport to a location.";
            usageString = "To teleport to a location, type \"teleport to <location name>\"";
            Client.Self.OnTeleport += new AgentManager.TeleportCallback(On_Teleport);
        }

        public void On_Teleport(string message, AgentManager.TeleportStatus status, AgentManager.TeleportFlags flags)
        {            
            Client.describeNext = false;
            if (status == AgentManager.TeleportStatus.Finished)
            {
                WriteLine(message);
                Client.describePeople(false);
                Client.describeObjects(false);
                Client.describeBuildings(false);
            }
        }

        public override void acceptInput(string verb, Parser args)
        {
           // base.acceptInput(verb, args);

            string[] tokens = args.prepPhrases["to"].Split(null);
            if (tokens.Length == 0)
            {
                WriteLine("Provide somewhere to teleport to.");
            }
            else
            {
                string to ="";
                int X =128, Y=128, Z=0;
                bool ifCoordinates = false;

                if (tokens.Length > 3)
                {
                    try
                    {
                        X = int.Parse(tokens[tokens.Length - 3]);
                        Y = int.Parse(tokens[tokens.Length - 2]);
                        Z = int.Parse(tokens[tokens.Length - 1]);
                        ifCoordinates = true;
                    }
                    catch (Exception e)  { }
                }

                if (!ifCoordinates)
                {
                    for (int i = 0; i < tokens.Length; i++)
                        to += tokens[i] + " ";
                    to = to.Trim();
                }
                else
                {
                    for (int i = 0; i < tokens.Length - 3; i++)
                        to += tokens[i] + " ";
                    to = to.Trim();
                }
                WriteLine("Trying to teleport to " + to + ".");
                Client.Self.Teleport(to, new Vector3(128, 128, 0));
            }

            Client.describeNext = false;
        }
    }
#pragma warning restore 0168
}
