using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Reflection;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class LoadCommand : Command, BotSystemCommand
    {
        public LoadCommand(BotClient testClient)
        {
            Name = "load";
            Description = "Loads commands from a dll. (Usage: load AssemblyNameWithoutExtension)";
            Category = CommandCategory.BotClient;
        }

        private static List<string> LoaderExtensions = new List<string> { "dll", "exe", "jar", "lib", "dynlib", "class", "so" };
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " load AssemblyNameWithoutExtension";

            BotClient Client = TheBotClient;

            string filename = AppDomain.CurrentDomain.BaseDirectory + args[0];
            string loadfilename = filename;
            bool tryexts = !File.Exists(loadfilename);
            string filenameLower = filename.ToLower();
            List<string> LoaderExtensions = new List<string>();
            lock (LoadCommand.LoaderExtensions)
            {
                LoaderExtensions.AddRange(LoadCommand.LoaderExtensions);
            }
            foreach (string extension in LoaderExtensions)
            {
                if (filenameLower.EndsWith("." + extension))
                {
                    tryexts = false;
                    break;
                }
            }

            if (tryexts)
            {
                foreach (var s in LoaderExtensions)
                {
                    string testfile =  loadfilename + "." + s;
                    if (File.Exists(testfile))
                    {
                        loadfilename = testfile;
                        break;
                    }

                }
            }

            try
            {
                Assembly assembly = Assembly.LoadFile(loadfilename);
                ClientManager.SingleInstance.RegisterAssembly(assembly);
                Client.LoadAssembly(assembly);
                string cmd = string.Join(" ", args, 1, args.Length - 1).Trim();
                if (!string.IsNullOrEmpty(cmd))
                    Client.InvokeAssembly(assembly, cmd, WriteLine);
                return Success("Assembly " + filename + " loaded.");
            }
            catch (Exception e)
            {
                return Failure("failed: load " + filename + " " + e);
            }
        }
    }
}
