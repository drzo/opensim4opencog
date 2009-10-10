using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

//using libsecondlife;

namespace cogbot.Actions
{
    class LocationCommand : Command
    {
        public LocationCommand(BotClient Client)
            : base(Client)
        {
            Name = "Location";
            //From locate: Description = "Gives the coordinates of where $bot is.";
            Description = "Finds out in which direction yourself, an object or a building or a person is.";
            Usage = "To find out wher an object, building or a person is, type \"where is [object/person name]\"";
            Usage += "\nTo locate the coordinates of yourself, type in \"locate\"";
            Parameters = new[] { new NamedParam(typeof(SimPosition), typeof(UUID)) };
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            SimPosition position;
            int argUsed;

            if (args.prepPhrases["is"].Length != 0)
            {
                position = WorldSystem.GetVector(Parser.ParseArguments(args.prepPhrases["is"]), out argUsed);
            } else
            {
                position = WorldSystem.GetVector(args.tokens, out argUsed);
            }

            {
                if (position != null)
                {
                    //avatar = ((SimAvatar)position).theAvatar;
                    //Client.Self.Movement.Camera.AtAxis
                    Vector3 myPos =base.GetSimPosition();
                    Vector3 forward = new Vector3(1, 0, 0);
                    Vector3 positionVect = position.SimPosition;
                    Vector3 offset = Vector3.Normalize(positionVect - myPos);
                    Quaternion newRot2 = Vector3.RotationBetween(forward, offset);

                    Quaternion newRot1 = Vector3.RotationBetween(positionVect, Client.Self.RelativePosition);
                    double newDist = Vector3.Distance(positionVect, Client.Self.RelativePosition);
                    WriteLine("Where Found: {0}", position);
                    WriteLine(Client.Self.Movement.Camera.AtAxis + ", " + newRot2 + ", " + newDist);

                    WriteLine(positionVect.X + ", " + positionVect.Y + ", " + positionVect.Z);
                    WriteLine(Client.Self.RelativePosition.X + ", " + Client.Self.RelativePosition.Y + ", " + Client.Self.RelativePosition.Z +"\n");
                    SimObjectImpl o = position as SimObjectImpl;
                    if (o != null) position = o.GetHeading();

                    //WriteLine(avatar.Rotation.X + ", " + avatar.Rotation.Y + ", " + avatar.Rotation.Z);
                    //WriteLine(Client.Self.RelativeRotation.X + ", " + Client.Self.RelativeRotation.Y + ", " + Client.Self.RelativeRotation.Z + "\n");

                    return Success("At: " + position);
                }
                else
                {
                    return Failure("I don't know where " + args.str + ".");
                }
            }            
        }
    }
}
