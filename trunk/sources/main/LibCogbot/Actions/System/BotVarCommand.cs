

using System;
using System.Collections;
using System.Collections.Generic;
using Cogbot;
using Cogbot.World;
using Cogbot.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class BotVarCommand : Cogbot.Actions.Command, BotSystemCommand
    {
        public BotVarCommand(BotClient client)
        {
            Name = "botvar";
            Description = 
                "Display bot vars. Bot vars are an interface between botcmd and AIML. " +
                "For more information on bot vars see <a href='wiki/BotVars'>Bot Vars</a>";
            Details = AddUsage("botvar <varname>", "display a bot var") +
                Example("botvar master", "displays the name of the bot's master");

            Parameters = NamedParam.CreateParams(
                "varname", typeof(string), "bot var to display");

            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int used = 0;
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            else
            {
                if (args.ContainsFlag("listns"))
                {
                    foreach (var c in MushDLR223.ScriptEngines.ScriptManager.GetNameSpaces(TheBotClient))
                    {
                        AddSuccess(c);
                    }
                    return SuccessOrFailure();
                }
                if (args.ContainsFlag("vars") || args.ContainsFlag("list"))
                {
                    foreach (var c in ScriptManager.GetNameSpaces(TheBotClient))
                    {
                        foreach (var s in ScriptManager.GetProviders(TheBotClient, c))
                        {
                            foreach (var s1 in s.SettingNames(TheBotClient, 1))
                            {
                                GetValue(c, s1, AddSuccess);
                            }
                        }
                    }
                    return SuccessOrFailure();
                }

                string nss = TheBotClient.GetName();
                string varname = args[0];
                if (args.tokens.Length > 1)
                {
                    string[] sa = Parser.SplitOff(args.tokens, 1);
                    string value = string.Join(" ", sa);
                    ScriptManager.AddSetting(TheBotClient, nss, varname, value);
                }
                GetValue(nss, varname, AddSuccess);
                return SuccessOrFailure();
            }
        }

        private void GetValue(string namespaec, string name, Action<string> AddSuccess)
        {
            var col = ScriptManager.GetGroup(TheBotClient, namespaec, name);
            if (col == null)
            {
                AddSuccess(namespaec + "." + name + "= NULL");
                return;
            }
            if (col.Count == 0)
            {
                AddSuccess(namespaec + "." + name + " .Count == 0");
                return;
            }
            int f = 0;
            foreach (var v in col)
            {
                f++;
                AddSuccess(namespaec + "." + name + " ." + f + " = " + v);
                if (f >= 5)
                {
                    AddSuccess(namespaec + "." + name + " .Count == " + col.Count);
                    break;
                }
            }
        }
    }
}
