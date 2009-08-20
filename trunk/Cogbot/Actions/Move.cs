using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    class Move : Action
    {
        Vector3 PrevPosition;
        float moveDist = 0;
        string moveTo;
        int precision = 2;
        public Move(BotClient Client)
            : base(Client)
        {
            helpString = "Move to a person or object, or in a direction: west, east, north or south."; //Client.RM.GetString("smove");
            usageString = "Type \"west/east/north/south\" to move 5 meters in a direction. Or Type \"west distance/east distance/north distance/south distance\" to move a specific distance in that direction.";// Client.RM.GetString("umove");
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            acceptInput0(verb, args, WriteLine);
            return verb + " complete";
        }

        void acceptInput0(string verb, Parser args, OutputDelegate WriteLine)
        {
            GridClient client = Client;
            string temp;
            SimActor sitter = WorldSystem.TheSimAvatar;

            if (sitter.IsSitting)
            {
                WriteLine("$bot is standing.");
                sitter.StandUp();
                // WriteLine("$bot is sitting, Please stand up to move.");
            }
            {
                Vector3 moveVec, TurnTo;
                Primitive prim;
                Avatar avatar;
                moveDist = 5;
                PrevPosition = GetSimPosition();

                string[] tokens = args.objectPhrase.Split(null);
                if ((verb == "west") || (verb == "east") || (verb == "north") || (verb == "south"))
                {
                    if (tokens[0] == "")
                        tokens[0] = verb;
                    else
                    {
                        try
                        {
                            moveDist = int.Parse(tokens[0]);
                        }
                        catch (Exception e)
                        {
                            moveDist = 5;
                            temp = e.Message;
                        }
                        tokens[0] = verb;
                    }
                }
                else if (tokens.Length == 2)
                {
                    try
                    {
                        moveDist = int.Parse(tokens[1]);
                    }
                    catch (Exception e)
                    {
                        moveDist = 5;
                        temp = e.Message;
                    }
                }
                else if (tokens[0] == "")
                    tokens[0] = "north";

                moveTo = tokens[0];



                if (args.prepPhrases["to"].Length > 0)
                {
                    if ((WorldSystem).tryGetAvatar(args.prepPhrases["to"], out avatar))
                    {
                        WriteLine("Moving to person " + avatar.Name + ".");
                        AutoPilot(avatar.Position, WriteLine);
                        return;
                    }
                    else if ((WorldSystem).tryGetPrim(args.prepPhrases["to"], out prim))
                    {
                        WriteLine("Moving to object " + prim.Properties.Name + ".");
                        AutoPilot(prim.Position, WriteLine);
                        return;
                    }
                    else
                    {
                        WriteLine("I don't know how to move to " + args.prepPhrases["to"] + ".");
                        return;
                    }
                }
                else if (tokens[0] == "west")
                {
                    moveVec = new Vector3(-moveDist, 0, 0);
                    TurnTo = new Vector3(-moveDist - 5, 0, 0);
                }
                else if (tokens[0] == "east")
                {
                    moveVec = new Vector3(moveDist, 0, 0);
                    TurnTo = new Vector3(moveDist + 5, 0, 0);
                }
                else if (tokens[0] == "north")
                {
                    moveVec = new Vector3(0, moveDist, 0);
                    TurnTo = new Vector3(0, moveDist + 5, 0);
                }
                else if (tokens[0] == "south")
                {
                    moveVec = new Vector3(0, -moveDist, 0);
                    TurnTo = new Vector3(0, -moveDist - 5, 0);
                }
                else
                {
                    WriteLine("I don't understand how to move " + args.str);
                    return;
                }

                Vector3 pos = client.Self.RelativePosition, dest = pos + moveVec;
                WriteLine("moving " + moveDist + " m towards " + tokens[0]);
                AutoPilot(dest, WriteLine);
            }
        }

        private void AutoPilot(Vector3 Position, OutputDelegate WriteLine)
        {
            const int TIME = 10000;
            using (AutoResetEvent are = new AutoResetEvent(false))
            {
                AgentManager.AlertMessageCallback callback = (message =>
                                                                  {
                                                                      if (message.ToLower().Contains("autopilot"))
                                                                      {
                                                                          DistanceMessage(WriteLine);
                                                                          try
                                                                          {
                                                                              are.Set();
                                                                          } catch(Exception)
                                                                          {
                                                                              // supresses "safe handle has been removed"
                                                                          }
                                                                      }
                                                                  });
                Client.Self.OnAlertMessage += callback;

                int update = Client.Self.Movement.UpdateInterval;
                bool reset = Client.Self.Movement.AutoResetControls;
                try
                {

                    //Client.Self.Movement.UpdateInterval = 100;
                    //Client.Self.Movement.AutoResetControls = true;

                    Client.Self.Movement.SendUpdate(true);
                    Client.Self.Movement.TurnToward(Position);
                    Client.Self.Movement.SendUpdate(true);
                    Client.Self.AutoPilotLocal((int) Position.X, (int) Position.Y, Position.Z);
                    if (!are.WaitOne(TIME))
                    {
                        WriteLine("Autopilot timed out moving to {0}", Position);
                        Client.Self.AutoPilotCancel();
                    }
                    Client.Self.OnAlertMessage -= callback;
                    Client.Self.Movement.SendUpdate(true);
                    Client.Self.Movement.TurnToward(Position);
                    Client.Self.Movement.SendUpdate(true);
                    DistanceMessage(WriteLine);
                }
                finally
                {

                    Client.Self.Movement.UpdateInterval = update;
                    Client.Self.Movement.AutoResetControls = reset;

                }
            }
        }

        private void DistanceMessage(OutputDelegate WriteLine)
        {
            if (moveDist >= precision)
            {
                System.Threading.Thread.Sleep(1000);
                //WriteLine("Prev: " + PrevPosition.ToString() + " Now: " + CurrentClient.Self.SimPosition.ToString());
                if (moveTo == "west")
                {
                    if (!((PrevPosition.X - GetSimPosition().X) > precision))
                    {
                        WriteLine("$bot bumped into something, Please try moving in a different direction!");
                    }
                }
                else if (moveTo == "east")
                {
                    if (!((GetSimPosition().X - PrevPosition.X) > precision))
                    {
                        WriteLine("$bot bumped into something, Please try moving in a different direction!");
                    }
                }
                else if (moveTo == "north")
                {
                    if (!((GetSimPosition().Y - PrevPosition.Y) > precision))
                    {
                        WriteLine("$bot bumped into something, Please try moving in a different direction!");
                    }
                }
                else if (moveTo == "south")
                {
                    if (!((PrevPosition.Y - GetSimPosition().Y) > precision))
                    {
                        WriteLine("$bot bumped into something, Please try moving in a different direction!");
                    }
                }
            }
            else
            {
                WriteLine("$bot completed");
            }

        }
    }
}
