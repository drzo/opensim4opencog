using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Movement
{
    class Sit : Command, BotPersonalCommand
    {
        public bool sittingOnGround = false;

        bool registeredCallback = false;
        public Sit(BotClient Client)
            : base(Client)
        {
            Description = "Sit on the ground or on an object. Sit with no params sits on the ground.";
            Usage = "<p>sit</p><p>sit on &lt;primspec&gt;</p><p>example: sit   <i>sit on ground</i></p><p>example: sit on chair</p>";
            ParameterVersions = NamedParam.CreateParamVersions(
               NamedParam.CreateParams(),
               NamedParam.CreateParams(
                   "on", typeof(PrimSpec),
                   "The object to sit on, as specified in <a href='wiki/BotCommands#PrimSpec'>Prim Spec</a>"));
            ResultMap = NamedParam.CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we sat");
            Parameters = new[] { new NamedParam("location", typeof(SimPosition), typeof(SimPosition)) };
            Category = CommandCategory.Movement;
        }

        void Objects_OnAvatarSitChanged(Simulator simulator, Avatar avatar, uint sittingOn, uint oldSeat)
        {
            if (avatar.Name == Client.Self.Name)
            {
                if (sittingOn != 0)
                {
                    //probly going to standing pos
                    sittingOnGround = false;
                    // WriteLine("$bot sat down.");
                }
                //else
                // WriteLine("$bot stood up.");
            }
            else
            {
                //if (sittingOn != 0)
                //    WriteLine(avatar.Name + " sat down.");
                //else
                //    WriteLine(avatar.Name + " stood up.");
            }
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            if (!registeredCallback)
            {
                registeredCallback = true;
                //Client.Objects.OnAvatarSitChanged += Objects_OnAvatarSitChanged;
            }

            TheBotClient.describeNext = true;

            if (args.Length == 0)
            {
                sittingOnGround = WorldSystem.TheSimAvatar.SitOnGround();
                return !sittingOnGround
                           ? Failure("$bot did not yet sit on the ground.")
                           : Success("$bot sat on the ground.");
            }

            //if (Client.Self.SittingOn != 0 || sittingOnGround)
            // return ("$bot is already sitting.");

            string on = args["on"];
            if (on.Length == 0)
            {
                on = String.Join(" ", args.tokens);
            }
            SimObject obj;
            if (WorldSystem.tryGetPrim(on, out obj))
            {
                WriteLine("Trying to sit on {0}.", obj);
                if (!WorldSystem.TheSimAvatar.SitOn(obj))
                {
                    return Failure("$bot did not yet sit on " + obj);
                }
                sittingOnGround = false;
                return Success("$bot did sit on " + obj);
            }

            return Failure("I don't know what " + on + " is.");
        }
    }
}