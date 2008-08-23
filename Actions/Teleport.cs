using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
#pragma warning disable 0168
    class Teleport : Action
    {
        public Teleport(TextForm parent)
            : base(parent)
        {
            helpString = "Teleport to a location.";
            usageString = "To teleport to a location, type \"teleport to <location name>\"";
            client.Self.OnTeleport += new AgentManager.TeleportCallback(On_Teleport);
        }

        public void On_Teleport(string message, AgentManager.TeleportStatus status, AgentManager.TeleportFlags flags)
        {            
            parent.describeNext = false;
            if (status == AgentManager.TeleportStatus.Finished)
            {
                parent.output(message);
                parent.describePeople(false);
                parent.describeObjects(false);
                parent.describeBuildings(false);
            }
        }

        public override void acceptInput(string verb, Parser args)
        {
            base.acceptInput(verb, args);

            string[] tokens = args.prepPhrases["to"].Split(null);
            if (tokens.Length == 0)
            {
                parent.output("Provide somewhere to teleport to.");
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
                parent.output("Trying to teleport to " + to + ".");
                client.Self.Teleport(to, new Vector3(128, 128, 0));
            }

            parent.describeNext = false;
        }
    }
#pragma warning restore 0168
}
