using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using Cogbot;
using OpenMetaverse;
using Cogbot.World;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class AvatarInfoCommand : Command, RegionMasterCommand, FFIComplete, AsynchronousCommand
    {
        public AvatarInfoCommand(BotClient testClient)
        {
            Name = "avatarinfo";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description = "Print out information on a nearby avatars.";
            Details = AddUsage(Name + " [agent-spec]", "no prim-spec then use $self");
            Category = CommandCategory.Appearance;
            Parameters = CreateParams(Optional("targets", typeof (AgentSpec), "the agent you wish to see " + Name));
            ResultMap = CreateParams(
                "list", typeof (List<SimAvatar>), "list of present agents",
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            ICollection PS;
            if (args.Length == 0)
            {
                PS = WorldObjects.SimAvatars;
            }
            else
            {
                int argsUsed;
                PS = WorldSystem.GetPrimitives(args.GetProperty("targets"), out argsUsed);
                if (IsEmpty(PS)) return Failure("Cannot find objects from " + args.str);
            }
            AppendList("list", PS);
            SetResult("count", PS.Count);
            if (args.IsFFI) return SuccessOrFailure();

            foreach (SimObject O in PS)
            {
                Primitive foundAv = O.Prim;
                StringBuilder output = new StringBuilder();
                output.AppendLine();
                if (foundAv.Textures == null)
                {
                    output.AppendLine("No textures yet");
                }
                else
                {
                    for (int i = 0; i < foundAv.Textures.FaceTextures.Length; i++)
                    {
                        if (foundAv.Textures.FaceTextures[i] != null)
                        {
                            Primitive.TextureEntryFace face = foundAv.Textures.FaceTextures[i];
                            AvatarTextureIndex type = (AvatarTextureIndex) i;

                            output.AppendFormat("{0}: {1}", type, face.TextureID);
                            output.AppendLine();
                        }
                    }
                }
                AddSuccess(output.ToString());
            }
            return SuccessOrFailure();
        }
    }
}