using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class SwipCommand : Command, RegionMasterCommand
    {
        private PrologScriptEngine.PrologScriptInterpreter pse = null;
		public SwipCommand(BotClient testClient)
        {
            Name = "swip";
            Description = "runs swi-prolog commands on current sim.";
            Category = CommandCategory.Simulator;
            Parameters = new[] { new NamedParam(typeof(GridClient), null), new NamedParam(typeof(string), null) };
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            int argsUsed;
            string text = args.str;

            return Success("swip is " + text);
        }
    }
}