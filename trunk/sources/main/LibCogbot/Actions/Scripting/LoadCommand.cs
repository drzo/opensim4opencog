using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Reflection;
using MushDLR223.Utilities;
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
            AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
        }

        private Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            var domain = (AppDomain) sender;
            foreach (var assembly in domain.GetAssemblies())
            {
                if (assembly.FullName == args.Name)
                    return assembly;
                if (assembly.ManifestModule.Name == args.Name)
                    return assembly;
            }

            foreach (Assembly assembly in LockInfo.CopyOf(TheBotClient.AssemblyListeners.Keys))
            {
                if (assembly.FullName == args.Name)
                    return assembly;
                if (assembly.ManifestModule.Name == args.Name)
                    return assembly;
            }
            string curDir = new DirectoryInfo(".").FullName;
            string assemblyName = args.Name;
            int comma = assemblyName.IndexOf(",");
            if (comma > 0)
            {
                assemblyName = assemblyName.Substring(0, comma);
            }
            var assemj = FindAssembly0(assemblyName, AppDomain.CurrentDomain.BaseDirectory) ??
                         FindAssembly0(assemblyName, curDir) ??
                         FindAssembly0(assemblyName, Path.GetDirectoryName(typeof (cogbot.BotClient).Assembly.CodeBase));
            if (assemj != null) return assemj;
           return assemj;
        }

        private static List<string> LoaderExtensions = new List<string> { "dll", "exe", "jar", "lib", "dynlib", "class", "so" };
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " load AssemblyNameWithoutExtension";

            BotClient Client = TheBotClient;

            string assemblyName = args[0];
            try
            {
                Assembly assembly = FindAssembly(assemblyName);
                if (assembly == null) return Failure("failed: load " + assemblyName + " cant find it");
                ClientManager.SingleInstance.RegisterAssembly(assembly);
                Client.LoadAssembly(assembly);
                string cmd = string.Join(" ", args, 1, args.Length - 1).Trim();
                if (!string.IsNullOrEmpty(cmd))
                    Client.InvokeAssembly(assembly, cmd, WriteLine);
                return Success("Assembly " + assemblyName + " loaded.");
            }
            catch (ReflectionTypeLoadException e)
            {
                foreach (var s in e.LoaderExceptions)
                {
                    Failure("failed: load " + s.TargetSite + " " + s);                    
                }
                return Failure("failed: load " + assemblyName + " " + e);
            }
            catch (Exception e)
            {
                return Failure("failed: load " + assemblyName + " " + e);
            } finally
            {
               // AppDomain.CurrentDomain.AssemblyResolve -= CurrentDomain_AssemblyResolve;
            }
        }
        public static Assembly FindAssembly(string assemblyName)
        {
            Assembly assembly = null;
            try
            {
                assembly = Assembly.Load(assemblyName);
            }
            catch (FileNotFoundException fnf)
            {
                if (fnf.FileName != assemblyName) throw fnf;
            }
            catch (Exception)
            {                
               throw;
            }
            if (assembly != null) return assembly;
            assembly = FindAssembly0(assemblyName, AppDomain.CurrentDomain.BaseDirectory) ??
                       FindAssembly0(assemblyName, (new DirectoryInfo(".").FullName)) ??
                       FindAssembly0(assemblyName, Path.GetDirectoryName(typeof(cogbot.BotClient).Assembly.CodeBase));
            if (assembly != null) return assembly;
            return Assembly.LoadFrom(assemblyName);

        }
        public static Assembly FindAssembly0(string assemblyName, string dirname)
        {
            string filename = Path.Combine(dirname, assemblyName);
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
            if (File.Exists(loadfilename))
            {
                try
                {
                    return Assembly.LoadFile(loadfilename);
                }
                catch (Exception)
                {                    
                    throw;
                }
            }
            return null;
        }
    }
}
