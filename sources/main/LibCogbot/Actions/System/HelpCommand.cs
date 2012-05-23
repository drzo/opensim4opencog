using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    [Obsolete]
    public class HelpCommand: Command ,SystemApplicationCommand
    {
        public HelpCommand(BotClient testClient)
		{
            // this version dies
			Name = "help";
			Description = "Lists available commands. If given a botcmd verb, lists help for that verb";
            Usage = Htmlize.Example("help", "prints list of botcmds") +
                Htmlize.Example("help moveto", "Prints help on the moveto command");
            Parameters = NamedParam.CreateParams(
                    NamedParam.Optional("command", typeof(string), "The command to get help on"));
            
            Category = CommandCategory.BotClient;
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            BotClient Client = TheBotClient;
            bool showPLVersionOfHelp = false;
            if (args.Length == 1)
            {
                if (args[0] == "prolog")
                {
                    showPLVersionOfHelp = true;
                    args = Parser.SplitOff(args, 1);
                }
                if (showPLVersionOfHelp)
                {
                    foreach (var s in Client.Commands.Values)
                    {
                        WriteLine(s.ToPrologString() + ".");
                    }
                    return Success("showPLVersion(done).");
                }
            }
            if (args.Length > 0)
            {
                if (Client.Commands.ContainsKey(args[0]))
                    return Success(Client.Commands[args[0]].Description);
                else
                    return Failure( "Command " + args[0] + " Does not exist. \"help\" to display all available commands.");
            }
			StringBuilder result = new StringBuilder();
            var CommandTree = new SortedDictionary<CommandCategory, List<CommandInfo>>();

            CommandCategory cc;
            foreach (CommandInfo c in TheBotClient.Commands.Values)
			{
                if (c.Category.Equals(null))
                    cc = CommandCategory.Unknown;
                else
                    cc = c.Category;

                if (CommandTree.ContainsKey(cc))
                    CommandTree[cc].Add(c);
                else
                {
                    var l = new List<CommandInfo>();
                    l.Add(c);
                    CommandTree.Add(cc, l);
                }
			}

            foreach (var kvp in CommandTree)
            {
                result.AppendFormat(Environment.NewLine + "* {0} Related Commands:" + Environment.NewLine, kvp.Key.ToString());
                int colMax = 0;
                for (int i = 0; i < kvp.Value.Count; i++)
                {
                    if (colMax >= 120)
                    {
                        result.AppendLine();
                        colMax = 0;
                    }

                    result.AppendFormat(" {0,-15}", kvp.Value[i].Name);
                    colMax += 15;
                }
                result.AppendLine();
            }
            result.AppendLine(Environment.NewLine + "Help [command] for usage/information");
            
            return Success(result.ToString());;
		}
    }
}
