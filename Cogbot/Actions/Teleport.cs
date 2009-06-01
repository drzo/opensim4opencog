using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading; //using libsecondlife;
// older LibOMV
//using TeleportFlags = OpenMetaverse.AgentManager.TeleportFlags;
//using TeleportStatus = OpenMetaverse.AgentManager.TeleportStatus;

namespace cogbot.Actions
{
#pragma warning disable 0168
    class Teleport : Action
    {
        ManualResetEvent TeleportFinished = new ManualResetEvent(false);

        public Teleport(BotClient testClient)
            : base(testClient)
        {
            TheBotClient = testClient;

            helpString = "Teleport to a location.";
            usageString = "To teleport to a location, type \"teleport to <location name>\"";
        }

                                    //string message, TeleportStatus status, TeleportFlags flags
        public void On_Teleport(string message, TeleportStatus status, TeleportFlags flags)
        {
            BotClient Client = TheBotClient;
            Client.describeNext = false;
            if (status == TeleportStatus.Finished)
            {
                WriteLine(message + " " + status);
                Client.describePeople(false);
                Client.describeObjects(false);
                Client.describeBuildings(false);
                Client.Self.OnTeleport -= On_Teleport;
                TeleportFinished.Set();
            }
        }

        public override string acceptInput(string verb, Parser args)
        {
            acceptInput0(verb, args);
            return writeBuffer.ToString();
        }

        void acceptInput0(string verb, Parser parser)
        {
            String[] args = parser.tokens;
            string ToS = parser.prepPhrases["to"];
            if (String.IsNullOrEmpty(ToS))
            {
                ToS = parser.str;
            }
            char[] splitchar = null;
            if (ToS.Contains("/"))
            {
                splitchar = new char[] { '/' };
            }           
            string[] tokens = ToS.Split(splitchar);
            if (tokens.Length == 0)
            {
                WriteLine("Provide somewhere to teleport to.");
            }
            else
            {
                Vector3 coords = new Vector3(128, 128, 40);
                string simName = "";//Client.Network.CurrentSim.Name;

                bool ifCoordinates = false;

                if (tokens.Length >= 3)
                {
                    try
                    {
                        coords.X = float.Parse(tokens[tokens.Length - 3]);
                        coords.Y = float.Parse(tokens[tokens.Length - 2]);
                        coords.Z = float.Parse(tokens[tokens.Length - 1]);
                        ifCoordinates = true;
                    }
                    catch (Exception e) { }
                }

                if (!ifCoordinates)
                {
                    for (int i = 0; i < tokens.Length; i++)
                        simName += tokens[i] + " ";
                    simName = simName.Trim();
                }
                else
                {
                    for (int i = 0; i < tokens.Length - 3; i++)
                        simName += tokens[i] + " ";
                    simName = simName.Trim();
                }
                {
                    if (String.IsNullOrEmpty(simName)) simName = Client.Network.CurrentSim.Name;
                    TeleportFinished.Reset();
                    Client.Self.OnTeleport += On_Teleport;
                    WriteLine("Trying to teleport to " + simName + " " + coords);
                    Client.Self.Teleport(simName, coords);
                    // wait 30 seconds
                    if (!TeleportFinished.WaitOne(30000, false))
                    {
                        Client.Self.OnTeleport -= On_Teleport;
                        WriteLine("Timeout on teleport to " + simName + " " + coords);
                    }
                }
            }

            TheBotClient.describeNext = false;
        }
    }
#pragma warning restore 0168
}
