using System.Collections;
using System.Collections.Generic;
using System.Text;
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
            Description = "Manipulates system variables." +
                          " These are global settings that affect Cogbot." +
                          "Many SysVars contain the word 'Maintain' (eg MaintainSounds). Generally this means" +
                          "Cogbot won't make a special request from the server to get information about this sort of thing" +
                          "and will provide information about it only if available" +
                          "For booleans anything but no or false (case insensitive) is true.";
            Usage = Htmlize.Usage("sysvar", "List all Sysvars and their settings") +
                    Htmlize.Usage("sysvar <key>", "List the current value of <key>") +
                    Htmlize.Usage("sysvar <key> <value>", "set a system variable") +
                    Htmlize.Example("sysvar CanUseSit True", "allow the bot to sit on things") +
                    Htmlize.Example("sysvar CanUseSit no", "don't allow the bot to sit on things") +
                    Htmlize.Example("sysvar Maintain false",
                                    "set every sysvar that contains Maintain in it's name to false") +
                    Htmlize.Example("sysvar MaintainEffectsDistance 8.0",
                                    "set the maximum distance to notice effects to 8.0") + SysVarHtml();


            Category = CommandCategory.BotClient;
        }

        public string SysVarHtml()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("<table><tr><th>Variable Name</th><th>current value</th><th>Description</th></tr>");
            foreach (var sv in LockInfo.CopyOf(ScriptManager.SysVars))
            {
                ConfigSettingAttribute svv = sv.Value;
                sb.AppendLine(string.Format("<tr name=\"{0}\" id='{0}'><td>{0}</td><td>{1}</td><td>{2}</td></tr>", Htmlize.NoEnts(svv.Name), Htmlize.NoEnts("" + svv.Value), Htmlize.NoEnts(svv.Comments)));
            }
            sb.AppendLine("</table>");
            return sb.ToString();
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