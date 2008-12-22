using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Where : Action
    {
        public Where(TextForm parent)
            : base(parent)
        {
            helpString = "Finds out in which direction an object or a building or a person is.";
            usageString = "To find out wher an object, building or a person is, type \"where is <object/person name>\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            Primitive prim;
            Avatar avatar;
            base.acceptInput(verb, args);

            if (args.prepPhrases["is"].Length == 0)
            {
                parent.output("Provide something for which you need to know Where is it.");
            }
            else
            {
                if (((Listeners.Avatars)parent.listeners["avatars"]).tryGetAvatar(args.prepPhrases["is"], out avatar))
                {
                    //client.Self.Movement.Camera.AtAxis
                    Vector3 myPos = client.Self.SimPosition;
                    Vector3 forward = new Vector3(1, 0, 0);
                    Vector3 offset = Vector3.Normalize(avatar.Position - myPos);
                    Quaternion newRot2 = Vector3.RotationBetween(forward, offset);

                    Quaternion newRot1 = Vector3.RotationBetween(avatar.Position, client.Self.RelativePosition);
                    double newDist = Vector3.Distance(avatar.Position, client.Self.RelativePosition);
                    parent.output(client.Self.Movement.Camera.AtAxis + ", " + newRot2 + ", " + newDist);

                    //parent.output(avatar.Position.X + ", " + avatar.Position.Y + ", " + avatar.Position.Z);
                    //parent.output(client.Self.RelativePosition.X + ", " + client.Self.RelativePosition.Y + ", " + client.Self.RelativePosition.Z +"\n");

                    //parent.output(avatar.Rotation.X + ", " + avatar.Rotation.Y + ", " + avatar.Rotation.Z);
                    //parent.output(client.Self.RelativeRotation.X + ", " + client.Self.RelativeRotation.Y + ", " + client.Self.RelativeRotation.Z + "\n");
                }
                else if (((Listeners.Objects)parent.listeners["objects"]).tryGetPrim(args.prepPhrases["is"], out prim))
                {
                    Quaternion newRot = Vector3.RotationBetween(prim.Position, client.Self.RelativePosition);
                    double newDist = Vector3.Distance(prim.Position, client.Self.RelativePosition);
                    parent.output(newRot + ", " + newDist);

                    //parent.output(prim.Position.X + ", " + prim.Position.Y + ", " + prim.Position.Z);
                    //parent.output(client.Self.RelativePosition.X + ", " + client.Self.RelativePosition.Y + ", " + client.Self.RelativePosition.Z + "\n");

                    //parent.output(prim.Rotation.X + ", " + prim.Rotation.Y + ", " + prim.Rotation.Z);
                    //parent.output(client.Self.RelativeRotation.X + ", " + client.Self.RelativeRotation.Y + ", " + client.Self.RelativeRotation.Z + "\n");
                }
                else
                {
                    parent.output("I don't know where is " + args.prepPhrases["is"] + ".");
                    return;
                }
            }
        }
    }
}
