using System;
using System.IO;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Scripting
{
    public class ScriptCommand : Command, BotPersonalCommand
    {
        public ScriptCommand(BotClient testClient)
        {
            Name = "Script";
            Description = "Reads BotClient commands from a file. One command per line, arguments separated by spaces. Usage: script <filename> [type]";
            Category = CommandCategory.BotClient;
            Parameters = new[]
                             {
                                 new NamedParam(typeof (File), typeof (String)),
                                 new NamedParam(typeof (String), typeof (String), null, "bot", "cs")
                             };

        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " script [filename]";

            String scripttype = "bot";
            string filename = args[0];
            var fs = filename.Split(new[] {'.'});
            if (fs.Length>1)
            {
                scripttype = fs[1];
            }
            if (args.Length>1)
            {
                scripttype = args[1];
            }
            var sl = Client.GetSecurityLevel(fromAgentID);
            if (!File.Exists(filename)) return Failure("Cannot find file " + filename);
            FileStream f = File.OpenRead(filename);
            StreamReader r = new StreamReader(f);
            r.BaseStream.Seek(0, SeekOrigin.Begin);
            return Client.ExecuteTask(scripttype, new StringReader(r.ReadToEnd()),WriteLine);
        }
    }
}
