using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    internal class Help : Command, SystemApplicationCommand
    {
        public Help(BotClient Client)
            : base(Client)
        {
        }

        public override void MakeInfo()
        {
            Description =
                "Print help on everything, or help on a topic string. The full help text is searched for the string.";
            AddExample("help", "shows the overview of all commands");
            AddExample("help moveto", "shows overvierw and usage/examples on the moveto command");
            Parameters = CreateParams(Optional("topic", typeof (string), "Optional text to search for."));

            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if outfit was worn");

            Category = CommandCategory.BotClient;
            Name = "help";
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            string mustHave = ": "; // everything
            bool detailed = false;
            if (args.Length > 0)
            {
                mustHave = args.str.ToLower().TrimEnd('s') + ": ";
                detailed = true;
            }
            int found = 0;
            var dictionary = TheBotClient.AllCommands();
            foreach (string action in dictionary.Keys)
            {
                CommandInfo info = dictionary[action];
                string overview = action.TrimEnd('s') + ": " + info.Description;
                string s = overview + " " + info.Details;
                if (!s.ToLower().Contains(mustHave))
                {
                    continue;
                }
                found++;
                WriteLine("[HELP] " + overview);
                if (detailed)
                {
                    WriteLine(info.Details);
                }
            }
            if (found == 0) WriteLine("I don't know about the verb " + args.GetString("topic") + ".");
            return Success("Help complete");
        }

        public CmdResult ExecuteRequestTree(CmdRequest args)
        {
            BotClient Client = TheBotClient;
            bool showPLVersionOfHelp = false;
            if (args.Length == 1)
            {
                if (args[0] == "prolog")
                {
                    showPLVersionOfHelp = true;
                    args = args.AdvanceArgs(1);
                }
                if (showPLVersionOfHelp)
                {
                    foreach (var s in Client.AllCommands().Values)
                    {
                        WriteLine(s.ToPrologString() + ".");
                    }
                    return Success("showPLVersion(done).");
                }
            }
            if (args.Length > 0)
            {
                int found = 0;
                foreach (var s in Client.AllCommands().Values)
                {
                    if (s.Matches(args[0]))
                    {
                        found++;
                        WriteLine(s.Name + ": " + s.Description);
                    }
                }
                if (found == 0)
                {
                    return Failure("Command " + args[0] + " Does not exist. \"help\" to display all available commands.");
                }
                return Success("found=" + found);
            }
            StringBuilder result = new StringBuilder();
            var CommandTree = new SortedDictionary<string, List<CommandInfo>>();

            string cc;
            foreach (CommandInfo c in TheBotClient.AllCommands().Values)
            {
                if (c.Category.Equals(null))
                    cc = "Unknown";
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
                result.AppendFormat(Environment.NewLine + "* {0} Related Commands:" + Environment.NewLine,
                                    kvp.Key.ToString());
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

            return Success(result.ToString());
            ;
        }
    }
}