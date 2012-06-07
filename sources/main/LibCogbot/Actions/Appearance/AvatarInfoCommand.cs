using System;
using System.Collections.Generic;
using System.Text;
using Cogbot;
using OpenMetaverse;
using Cogbot.World;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class AvatarInfoCommand : Command, RegionMasterCommand
    {
        public AvatarInfoCommand(BotClient testClient)
        {
            Name = "avatarinfo";
            Description = "Print out information on a nearby avatar.";
            Details = AddUsage(Name + " [agent-spec]", "no prim-spec then use $self");
            Category = CommandCategory.Appearance;
            Parameters = CreateParams(
                Optional("target", typeof(AgentSpec),
                                    "the agent you wish to see " + Name +
                                    " (see meets a specified <a href='wiki/BotCommands#AvatarSpec'>Avatar Spec</a>.)"));
            ResultMap = CreateParams(
                "message", typeof(string), "if success was false, the reason why",
                "success", typeof(bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
            {
                //return ShowUsage();// " avatarinfo [firstname] [lastname]";
                int count = 0;
                foreach (var s in WorldObjects.SimAvatars)
                {
                    WriteLine(" {0}: {1}", s.GetName(), s.DebugInfo());
                    count++;
                }
                return Success("Avatars shown: " + count);
            }

            string targetName = String.Join(" ", args);

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + args.str);
            foreach (var O in PS)
            {
                Primitive foundAv = O.Prim;
                StringBuilder output = new StringBuilder();
                targetName = String.Join(" ", args, 0, argsUsed);
                output.AppendFormat("{0} ({1})", targetName, foundAv.ID);
                output.AppendLine();
                if (foundAv.Textures != null)
                {
                    output.AppendLine("No textures yet");
                }
                else
                    for (int i = 0; i < foundAv.Textures.FaceTextures.Length; i++)
                    {
                        if (foundAv.Textures.FaceTextures[i] != null)
                        {
                            Primitive.TextureEntryFace face = foundAv.Textures.FaceTextures[i];
                            AvatarTextureIndex type = (AvatarTextureIndex)i;

                            output.AppendFormat("{0}: {1}", type, face.TextureID);
                            output.AppendLine();
                        }
                    }
                AddSuccess(output.ToString());
            }
            return SuccessOrFailure();
        }
    }
}
