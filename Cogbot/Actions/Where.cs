using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Where : Action
    {
        public Where(BotClient Client)
            : base(Client)
        {
            helpString = "Finds out in which direction an object or a building or a person is.";
            usageString = "To find out wher an object, building or a person is, type \"where is <object/person name>\"";
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            Primitive prim;
            Avatar avatar;
           // base.acceptInput(verb, args);

            if (args.prepPhrases["is"].Length == 0)
            {
                return("Provide something for which you need to know Where is it.");
            }
            else
            {
                if ((WorldSystem).tryGetAvatar(args.prepPhrases["is"], out avatar))
                {
                    //Client.Self.Movement.Camera.AtAxis
                    Vector3 myPos =base.GetSimPosition();
                    Vector3 forward = new Vector3(1, 0, 0);
                    Vector3 offset = Vector3.Normalize(avatar.Position - myPos);
                    Quaternion newRot2 = Vector3.RotationBetween(forward, offset);

                    Quaternion newRot1 = Vector3.RotationBetween(avatar.Position, Client.Self.RelativePosition);
                    double newDist = Vector3.Distance(avatar.Position, Client.Self.RelativePosition);
                    WriteLine(Client.Self.Movement.Camera.AtAxis + ", " + newRot2 + ", " + newDist);

                    //WriteLine(avatar.Position.X + ", " + avatar.Position.Y + ", " + avatar.Position.Z);
                    //WriteLine(Client.Self.RelativePosition.X + ", " + Client.Self.RelativePosition.Y + ", " + Client.Self.RelativePosition.Z +"\n");

                    //WriteLine(avatar.Rotation.X + ", " + avatar.Rotation.Y + ", " + avatar.Rotation.Z);
                    //WriteLine(Client.Self.RelativeRotation.X + ", " + Client.Self.RelativeRotation.Y + ", " + Client.Self.RelativeRotation.Z + "\n");
                }
                else if ((WorldSystem).tryGetPrim(args.prepPhrases["is"], out prim))
                {
                    Quaternion newRot = Vector3.RotationBetween(prim.Position, Client.Self.RelativePosition);
                    double newDist = Vector3.Distance(prim.Position, Client.Self.RelativePosition);
                    WriteLine(newRot + ", " + newDist);

                    //WriteLine(prim.Position.X + ", " + prim.Position.Y + ", " + prim.Position.Z);
                    //WriteLine(Client.Self.RelativePosition.X + ", " + Client.Self.RelativePosition.Y + ", " + Client.Self.RelativePosition.Z + "\n");

                    //WriteLine(prim.Rotation.X + ", " + prim.Rotation.Y + ", " + prim.Rotation.Z);
                    //WriteLine(Client.Self.RelativeRotation.X + ", " + Client.Self.RelativeRotation.Y + ", " + Client.Self.RelativeRotation.Z + "\n");
                }
                else
                {
                    return ("I don't know where is " + args.prepPhrases["is"] + ".");
                }
            }
            return "";
        }
    }
}
