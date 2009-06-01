using System;
using System.Collections.Generic;
using System.Text;

using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    class Move : Action
    {
        Vector3 PrevPosition;
        float moveDist=0;
        string moveTo;
        int precision = 2;
        bool registeredCallback = false;
        public Move(BotClient Client)
            : base(Client)
        {
            helpString = "Move to a person or object, or in a direction: west, east, north or south."; //Client.RM.GetString("smove");
            usageString = "Type \"west/east/north/south\" to move 5 meters in a direction. Or Type \"west distance/east distance/north distance/south distance\" to move a specific distance in that direction.";// Client.RM.GetString("umove");
        }

        private void Self_OnAlertMessage(string message)
        {
            GridClient client = Client;
            if (message == "Autopilot canceled")
            {
                if (moveDist >= precision)
                {
                    System.Threading.Thread.Sleep(1000);
                    //WriteLine("Prev: " + PrevPosition.ToString() + " Now: " + CurrentClient.Self.SimPosition.ToString());
                    if (moveTo == "west")
                    {
                        if (!((PrevPosition.X - client.Self.SimPosition.X) > precision))
                            WriteLine("You bumped into something, Please try moving in a different direction!");
                    }
                    else if (moveTo == "east")
                    {
                        if (!((client.Self.SimPosition.X - PrevPosition.X) > precision))
                            WriteLine("You bumped into something, Please try moving in a different direction!");
                    }
                    else if (moveTo == "north")
                    {
                        if (!((client.Self.SimPosition.Y - PrevPosition.Y) > precision))
                            WriteLine("You bumped into something, Please try moving in a different direction!");
                    }
                    else if (moveTo == "south")
                    {
                        if (!((PrevPosition.Y - client.Self.SimPosition.Y) > precision))
                            WriteLine("You bumped into something, Please try moving in a different direction!");
                    }
                } 
            }
        }
         public override string acceptInput(string verb, Parser args)
         {
                acceptInput0(verb,args);
                return writeBuffer.ToString();
         }

         void acceptInput0(string verb, Parser args)
        {
            GridClient client = Client;
            string temp;
           // base.acceptInput(verb, args);
            Sit sit = (Sit)Client.Commands["sit"];

            if (client.Self.SittingOn != 0 || sit.sittingOnGround)
                WriteLine("You are sitting, Please stand up to move.");
            else
            {                
                Vector3 moveVec, TurnTo;
                Primitive prim;
                Avatar avatar;
                moveDist = 5;
                PrevPosition = client.Self.SimPosition;

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

                if (!registeredCallback)
                {
                    registeredCallback = true;
                    Client.Self.OnAlertMessage += new AgentManager.AlertMessageCallback(Self_OnAlertMessage);
                }
                if (args.prepPhrases["to"].Length > 0)
                {
                    if ((WorldSystem).tryGetAvatar(args.prepPhrases["to"], out avatar))
                    {
                        WriteLine("Moving to person " + avatar.Name + ".");
                        client.Self.AutoPilotLocal((int)avatar.Position.X,
                            (int)avatar.Position.Y, avatar.Position.Z);
                        client.Self.Movement.TurnToward(avatar.Position);
                        return;
                    }
                    else if ((WorldSystem).tryGetPrim(args.prepPhrases["to"], out prim))
                    {
                        WriteLine("Moving to object " + prim.Properties.Name + ".");
                        client.Self.AutoPilotLocal((int)prim.Position.X, (int)prim.Position.Y, prim.Position.Z);
                        client.Self.Movement.TurnToward(prim.Position);
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
                    TurnTo = new Vector3(0, moveDist+ 5, 0);
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

                Vector3 pos = client.Self.RelativePosition,dest = pos + moveVec;
                if (tokens[0] != "west")
                    TurnTo = pos + moveVec;
                Client.Self.Movement.TurnToward(TurnTo);
                Client.Self.Movement.SendUpdate();
                Client.Self.AutoPilotLocal((int)dest.X, (int)dest.Y, dest.Z);
                Client.Self.Movement.SendUpdate();
                WriteLine("moving " + moveDist + " m towards " + tokens[0]);
            }
        }
    }
}
