using System;
using System.Collections.Generic;
using System.Text;

using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Move : Action
    {
        public Move(TextForm parent)
            : base(parent)
        {
            helpString = "Move to a person or object, or in a direction.";
            usageString = helpString + "\r\n you can type \"move to <avatar name>\", \"move to <object name>\" or \"move left/right/forward/back\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
           // base.acceptInput(verb, args);

            float moveDist = 5;
            Vector3 moveVec;
            Primitive prim;
            Avatar avatar;

            if (args.prepPhrases["to"].Length > 0)
            {
                if (((Listeners.Avatars)parent.listeners["avatars"]).tryGetAvatar(args.prepPhrases["to"], out avatar))
                {
                    parent.output("Moving to person " + avatar.Name + ".");
                    client.Self.AutoPilotLocal((int)avatar.Position.X,
                        (int)avatar.Position.Y, avatar.Position.Z);
                    client.Self.Movement.TurnToward(avatar.Position);
                    return;
                }
                else if (((Listeners.Objects)parent.listeners["objects"]).tryGetPrim(args.prepPhrases["to"], out prim))
                {
                    parent.output("Moving to object " + prim.Properties.Name + ".");
                    client.Self.AutoPilotLocal((int)prim.Position.X, (int)prim.Position.Y, prim.Position.Z);
                    client.Self.Movement.TurnToward(prim.Position);
                    return;
                }
                else
                {
                    parent.output("I don't know how to move to " + args.prepPhrases["to"] + ".");
                    return;
                }
            }
            else if (args.str == "left")
            {
                moveVec = new Vector3(-moveDist, 0, 0);
            }
            else if (args.str == "right")
            {
                moveVec = new Vector3(moveDist, 0, 0);
            }
            else if (args.str == "forward")
            {
                moveVec = new Vector3(0, moveDist, 0);
            }
            else if (args.str == "back")
            {
                moveVec = new Vector3(0, -moveDist, 0);
            }
            else
            {
                parent.output("I don't understand how to move " + args.str);
                return;
            }

            Vector3 pos = client.Self.RelativePosition, dest = pos + moveVec;
            client.Self.AutoPilotLocal((int)dest.X, (int)dest.Y, dest.Z);
            client.Self.Movement.TurnToward(dest);
            parent.output("Moving.");
        }
    }
}
