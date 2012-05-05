using System.Collections;
using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class SysVarCommand : cogbot.Actions.Command, BotSystemCommand
    {
        public SysVarCommand(BotClient client)
        {
            Name = "sysvar";
            Description = "Manipulates system vars. Usage: sysvar CanPhantomize [True]";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int used = 0;
            var sysvars = LockInfo.CopyOf(ScriptManager.SysVars);
            if (args.Length == 0) args = new[] { "" };
            List<ConfigSettingAttribute> setThese = new List<ConfigSettingAttribute>();
            int found = 0;
            string find = args[0].ToLower();
            foreach (var sv in sysvars)
            {
                var svv = sv.Value;
                if (svv.Finds(find))
                {
                    found++;
                    WriteLine("" + svv.DebugInfo);
                    setThese.Add(svv);
                }
            }
            if (args.Length == 1)
            {
                return Success("Found sysvars: " + found);
            }
            foreach(var one in setThese)
            {
                one.Value = args[1];
                Success("Set sysvar: " + one.Name + " to " + one.Value);
            }
            return ShowUsage();
        }
    }
}