using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using Cogbot.World;
using OpenMetaverse; //using libsecondlife;
using MushDLR223.ScriptEngines;
using PathSystem3D.Navigation;

namespace Cogbot.Actions.Movement
{
    internal class Sit : Command, BotPersonalCommand, FFIComplete
    {
        public bool sittingOnGround = false;

        private bool registeredCallback = false;

        public Sit(BotClient Client)
            : base(Client)
        {
            TheBotClient = Client;
        }

        public override void MakeInfo()
        {
            Description = "Sit on the ground or on an object. Sit with no params sits on the ground.";
            Details =
                "<p>sit</p><p>sit on &lt;primspec&gt;</p><p>example: sit   <i>sit on ground</i></p><p>example: sit on chair</p>";
            AddVersion(CreateParams(
                           Optional("on", typeof (PrimSpec), "The object to sit on (may use $nearest)"),
                           Optional("down", typeof (bool), "will use ground sitting")),
                       "Sit on Object");
            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if we sat");
            Category = CommandCategory.Movement;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage(); // " siton UUID";

            int argsUsed;
            SimPosition pos;
            if (!args.TryGetValue("on", out pos))
            {
                sittingOnGround = WorldSystem.TheSimAvatar.SitOnGround();
                return !sittingOnGround
                           ? Failure("$bot did not yet sit on the ground.")
                           : Success("$bot sat on the ground.");
            }
            if (pos is SimObject)
            {
                if (WorldSystem.TheSimAvatar.SitOn(pos as SimObject)) Success("$bot sat on " + pos);
                if (!args.IsTrue("down")) return Failure("$bot did not yet sit on " + pos);
            }
            else if (pos == null)
            {
                return Failure("$bot did not yet find " + args.str);
            }
            // @todo use autoppoiolot to the location andf then sit
            TheSimAvatar.GotoTargetAStar(pos);
            sittingOnGround = WorldSystem.TheSimAvatar.SitOnGround();
            if (sittingOnGround) return Success("$bot sat at " + pos);
            return Failure("$bot did not sit at " + pos);
        }
    }
}