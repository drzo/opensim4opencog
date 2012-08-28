using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif

namespace Cogbot.Actions
{
    internal class StopCommand : Command, BotSystemCommand
    {
        public StopCommand(BotClient Client)
            : base(Client)
        {
        }

        public override void MakeInfo()
        {
            Description =
                @"If called with args, cancels the current modeless action. 
If called with no args, cancels all currently running actions.";
            Details = "<p>stop</p><p>example:</p><p>move 122/144/12</p><p>stop</p>";
            Parameters = CreateParams(
                OptionalFlag("--all", "stop movement, current action and queue and thread/queue tasks"),
                OptionalFlag("--queue", "stop queue tasks"),
                OptionalFlag("--current", "stop queue tasks"),
                OptionalFlag("--thread", "stop thread tasks"),
                OptionalFlag("--current", "stop current task"),
                OptionalFlag("--movement", "stop movement tasks"),
                OptionalFlag("--pointing", "stop pointing tasks"),
                Optional("taskid", typeof (string),
                         @"in theory the action to stop. In practice, if present the current action is stopped, 
if absent all actions are stopped."));
            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if outfit was worn");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            //base.acceptInput(verb, args);

            BotClient Client = TheBotClient;
            string onlyShow = "";
            args.SetValue("--kill", true);
            bool thread = args.IsTrue("--thread");
            bool queue = args.IsTrue("--queue");
            bool all = args.IsTrue("--all");
            bool current = args.IsTrue("--current");
            bool movement = args.IsTrue("--movement");
            bool pointing = args.IsTrue("--pointing");

            ThreadCommand.ExecuteRequestProc(args, this);
            if (all)
            {
                current = true;
                movement = true;
                pointing = true;
            }
            if (current && TheSimAvatar.CurrentAction != null)
            {
                WriteLine("Killing " + TheSimAvatar.CurrentAction);
                TheSimAvatar.CurrentAction = null;
                IncrResult("killed", 1);
            }
            if (movement)
            {
                WriteLine("Killing movement");
                WorldSystem.TheSimAvatar.StopMoving();
            }
            if (pointing)
            {
                Client.ExecuteCommand("pointat", args.CallerAgent, WriteLine, CMDFLAGS.Backgrounded);
            }
            return Success("Stopped " + base.ResultValue("killed"));
        }
    }
}