using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection.Emit;
using System.Text;
using System.Reflection;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class LoadCommand : Command, BotSystemCommand //SystemApplicationCommand
    {
        public LoadCommand(BotClient testClient)
        {
            Name = "load";
            Description = "Loads commands from a dll. (Usage: load AssemblyNameWithoutExtension)";
            Category = CommandCategory.BotClient;
            AddUsage(CreateParams("assembly", typeof (string), "filename to " + Name), Description);
            AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();// " load AssemblyNameWithoutExtension";

            BotClient Client = TheBotClient;

            string assemblyName = args[0];
            try
            {
                Assembly assembly = FindAssembly(assemblyName);
                if (assembly == null) return Failure("failed: load " + assemblyName + " cant find it");
                string cmd = string.Join(" ", args, 1, args.Length - 1).Trim();
                //ClientManager.SingleInstance.LoadBotAssembly(assembly, cmd);
                Client.LoadAssembly(assembly);
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
            assembly = LoadAssemblyByFile(assemblyName);
            if (assembly != null) return assembly;
            return Assembly.LoadFrom(assemblyName);

        }

        static private Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            var domain = (AppDomain)sender;
            foreach (var assembly in LockInfo.CopyOf(domain.GetAssemblies()))
            {
                if (assembly.FullName == args.Name)
                    return assembly;
                if (assembly.ManifestModule.Name == args.Name)
                    return assembly;
            }

            foreach (Assembly assembly in LockInfo.CopyOf(AssembliesLoaded))
            {
                if (assembly.FullName == args.Name)
                    return assembly;
                if (assembly.ManifestModule.Name == args.Name)
                    return assembly;
            }
            string assemblyName = args.Name;
            int comma = assemblyName.IndexOf(",");
            if (comma > 0)
            {
                assemblyName = assemblyName.Substring(0, comma);
            }
            return LoadAssemblyByFile(assemblyName);
        }

        protected static IEnumerable<Assembly> AssembliesLoaded
        {
            get { return new List<Assembly>(); }
        }

        private static Assembly LoadAssemblyByFile(string assemblyName)
        {
            if (File.Exists(assemblyName))
            {
                try
                {
                    var fi = new FileInfo(assemblyName);
                    if (fi.Exists) return Assembly.LoadFile(fi.FullName);
                }
                catch (Exception)
                {
                    throw;
                }
            }
            IList<string> sp = LockInfo.CopyOf((IEnumerable<string>)null);
            foreach (var dir in new[] { AppDomain.CurrentDomain.BaseDirectory, new DirectoryInfo(".").FullName, Path.GetDirectoryName(typeof(BotClient).Assembly.CodeBase), Environment.CurrentDirectory })
            {
                if (!sp.Contains(dir)) sp.Add(dir);
            }
            string lastTested = "";
            foreach (var s in LockInfo.CopyOf(AppDomain.CurrentDomain.GetAssemblies()))
            {
                try
                {
                    if (s is AssemblyBuilder) continue;
                    string dir = Path.GetDirectoryName(s.CodeBase);
                    dir = NormalizePath(dir);
                    if (dir == lastTested) continue;
                    lastTested = dir;
                    if (!sp.Contains(dir)) sp.Add(dir);
                }
                catch (NotSupportedException)
                {
                    // Reflected Assemblies do this
                }
            }
            foreach (string pathname in sp)
            {
                var assemj = FindAssemblyByPath(assemblyName, pathname);
                if (assemj != null) return assemj;
            }

            return null;
        }

        private static string NormalizePath(string dirname1)
        {
            string dirname = dirname1;
            if (dirname.StartsWith("file:\\"))
            {
                dirname = dirname.Substring(6);
            }
            if (dirname.StartsWith("file://"))
            {
                dirname = dirname.Substring(7);
            }
            dirname = new FileInfo(dirname).FullName;
            if (dirname != dirname1)
            {
                return dirname;
            }
            return dirname1;
        }


        private static readonly List<string> LoaderExtensionStrings = new List<string> { "dll", "exe", "jar", "lib", "dynlib", "class", "so" };
        public static Assembly FindAssemblyByPath(string assemblyName, string dirname)
        {

            dirname = NormalizePath(dirname);
            string filename = Path.Combine(dirname, assemblyName);
            string loadfilename = filename;
            bool tryexts = !File.Exists(loadfilename);
            string filenameLower = filename.ToLower();
            List<string> LoaderExtensions = new List<string>();
            lock (LoaderExtensionStrings)
            {
                LoaderExtensions.AddRange(LoaderExtensionStrings);
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
                    string testfile = loadfilename + "." + s;
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
                    return Assembly.LoadFile(new FileInfo(loadfilename).FullName);
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
