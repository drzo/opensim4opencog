using System;
using System.Collections.Generic;
using System.Text;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

//using libsecondlife;

namespace cogbot.Actions.Movement
{
    class LocationCommand : Command, BotSystemCommand
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
            }
            else
            {
                string[] parserParseArguments = Parser.ParseArguments(args.str);
                position = WorldSystem.GetVector(parserParseArguments, out argUsed);
            }

            {
                double RAD2DEG = 180 / Math.PI;
                if (position != null)
                {
                    //avatar = ((SimAvatar)position).theAvatar;
                    //Client.Self.Movement.Camera.AtAxis
                    Vector3d myPos = TheSimAvatar.GlobalPosition;
                    Vector3 forward = new Vector3(1, 0, 0);
                    Vector3d positionVect = position.GlobalPosition;
                    Vector3d offsetG = positionVect - myPos;
                    Vector3 offset = new Vector3((float)offsetG.X, (float)offsetG.Y, (float)offsetG.Z);
                    Quaternion newRot2 = Vector3.RotationBetween(forward, offset);

                    //Quaternion newRot1 = Vector3d.RotationBetween(positionVect, Client.Self.RelativePosition);
                    double newDist = Vector3d.Distance(positionVect, myPos);
                    WriteLine("Where Found: {0}", position);

                    // Absolute
                    WriteLine(" SimPosition = " + Vector3Str(position.SimPosition));
                    WriteLine(" SimRotation = {0:0.#}*", WorldObjects.GetZHeading(position.SimRotation)*RAD2DEG);

                    // Relative
                    WriteLine(" RelSimPosition = {0} ", Vector3Str(offset));
                    double relAngle = (Math.Atan2(-offset.X, -offset.Y) + Math.PI); // 2P
                    WriteLine(" RelSimPolar = {0:0.#}*{1:0.0#}", relAngle * RAD2DEG, newDist);

                    double selfFacing = WorldObjects.GetZHeading(TheSimAvatar.SimRotation);
                    WriteLine(" SelfFacingPolar = {0:+0.0#;-0.0#}*{1:0.0#}", (relAngle - selfFacing) * RAD2DEG, newDist);

                    if (false)
                    {
                        //WriteLine(" newRot1 = {0:0.#}*", WorldObjects.GetZHeading(newRot1) * RAD2DEG);
                        //WriteLine(" newRot2 = {0:0.#}*", WorldObjects.GetZHeading(newRot2) * RAD2DEG);
                        WriteLine(" Client.Self.Movement.Camera.AtAxis = " +
                                  Vector3Str(Client.Self.Movement.Camera.AtAxis));
                        WriteLine(" Client.Self.RelativePosition = " + Vector3Str(Client.Self.RelativePosition));
                        WriteLine(" SelfFacing = {0:0.#}*{1:0.0#}", selfFacing*RAD2DEG,
                                  Client.Self.Movement.Camera.AtAxis.Length());
                    }
                    SimObjectImpl o = position as SimObjectImpl;
                    if (o != null) position = o.GetHeading();

                    //WriteLine(avatar.Rotation.X + ", " + avatar.Rotation.Y + ", " + avatar.Rotation.Z);
                    //WriteLine(Client.Self.RelativeRotation.X + ", " + Client.Self.RelativeRotation.Y + ", " + Client.Self.RelativeRotation.Z + "\n");

                    return Success("At: " + position + " " + TheSimAvatar.DistanceVectorString(position));
                }
                else
                {
                    return Failure("I don't know where " + args.str + ".");
                }
            }
        }

        static string Vector3Str(Vector3 position)
        {
            return string.Format("{0:0.0#} {1:0.0#} {2:0.0#}", position.X, position.Y, position.Z);
        }
    }
}
