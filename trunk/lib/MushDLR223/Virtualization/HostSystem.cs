using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Net;
using MushDLR223.Utilities;

namespace MushDLR223.Virtualization
{
    public static class HostSystem
    {
        public static string BackupExt = ".bak";
        static public readonly object ExistenceLock = new object();
        private static readonly List<Func<string, string[]>> _pathResolvers = new List<Func<string, string[]>>();
        public static readonly string[] NO_EXPANSIONS = new string[0];

        static HostSystem()
        {
            _pathResolvers.Add(ExpandEnvVars);
        }

        private static string[] ExpandEnvVars(string arg)
        {
            if (arg.StartsWith("%") && arg.EndsWith("%"))
            {
                string s = Environment.GetEnvironmentVariable(arg.Substring(1, arg.Length - 2));
                if (!IsNullOrEmpty(s)) return new[] { s };
            }
            return NO_EXPANSIONS;
        }


        public static IEnumerable<Func<string, string[]>> PathResolvers
        {
            get { lock (_pathResolvers) return _pathResolvers.ToArray(); }
        }

        public static void InsertPathResolver(Func<string, string[]> funct)
        {
            lock (_pathResolvers) _pathResolvers.Insert(0, funct);
        }

        public static void RemovePathResolver(Func<string, string[]> funct)
        {
            lock (_pathResolvers)
            {
                while (_pathResolvers.Remove(funct)) ;
            }
        }

        public static string ToRelativePath(string str)
        {
            return ToRelativePath(str, Environment.CurrentDirectory);
        }
        public static string ToRelativePath(string str, string prefix)
        {
            var strSaved = str;
            bool existed = FileOrDirExists(str);

            str = str.Replace("\\", "/");
            if (DirExists(prefix))
            {
                prefix = prefix.Replace("\\", "/");
                if (!prefix.EndsWith("/")) prefix += "/";
                if (str.StartsWith(prefix)) str = str.Substring(prefix.Length);
                str = str.Replace("/", "" + Path.DirectorySeparatorChar);
                if (existed) if (!FileOrDirExists(str))
                    {
                        str = strSaved;
                    }
            }
            return str;
        }

        public static bool FileOrDirExists(string str)
        {
            return FileExists(str) || DirExists(str);
        }

        public static bool DirExists(string pathname)
        {
            string toexitingpath = ResolveToExistingPath(pathname);
            if (!IsNullOrEmpty(toexitingpath)) pathname = toexitingpath;
            if (IsNullOrEmpty(pathname)) return false;
            return Directory.Exists(pathname);
        }

        public static bool BackupFile(string pathname)
        {
            string toexitingpath = ResolveToExistingPath(pathname);
            if (!IsNullOrEmpty(toexitingpath)) pathname = toexitingpath;
            return BackupFile(pathname, true);
        }
        public static bool BackupFile(string pathname, bool onlyOne)
        {
            string toexitingpath = ResolveToExistingPath(pathname);
            if (!IsNullOrEmpty(toexitingpath)) pathname = toexitingpath;

            lock (ExistenceLock)
            {
                if (!FileExists(pathname)) return true;
                if (!onlyOne) return BackupFileMulti(pathname);
                string backname = pathname + ".0" + BackupExt;
                if (FileExists(backname))
                {
                    if (!DeleteFile(backname)) return false;
                }
                return FileMove(pathname, backname);
            }

        }

        public static bool DeleteFile(string pathname)
        {
            string toexitingpath = ResolveToExistingPath(pathname);
            if (!IsNullOrEmpty(toexitingpath)) pathname = toexitingpath;

            if (!FileExists(pathname)) return true;
            try
            {
                File.Delete(pathname);
                return true;
            }
            catch (Exception exception)
            {
                writeToLog("DELETE FILE: '" + pathname + "' " + exception);
                return !FileExists(pathname);
            }
        }

        public static bool BackupFileMulti(string pathname)
        {
            string toexitingpath = ResolveToExistingPath(pathname);
            if (!IsNullOrEmpty(toexitingpath)) pathname = toexitingpath;

            lock (ExistenceLock)
            {                
                if (!FileExists(pathname)) return true;


                string backname = pathname + ".0" + BackupExt;
                int i = 0;
                while (FileExists(backname))
                {
                    i++;
                    backname = pathname + "." + i + BackupExt;
                }
                return FileMove(pathname, backname);
            }
        }

        public static bool FileMove(string pathname, string backname)
        {
            lock (ExistenceLock)
            {
                if (IsNullOrEmpty(pathname)) return false;
                try
                {
                    File.Move(pathname, backname);
                    return true;
                }
                catch (Exception e)
                {
                    writeToLog("Moving " + pathname + " -> " + backname + " caused " + e);
                    return false;
                }
            }
        }

        public static bool FileExists(string pathname)
        {
            if (IsNullOrEmpty(pathname)) return false;
            string toexitingpath = ResolveToExistingPath(pathname);
            if (toexitingpath == null) return false;
            lock (ExistenceLock) return File.Exists(toexitingpath);
        }

        private static bool IsNullOrEmpty(string pathname)
        {
            return pathname == null || pathname.Trim().Length == 0;
        }

        public static bool CreateDirectory(string pathname)
        {
            if (IsNullOrEmpty(pathname)) return false;
            string toexitingpath = ResolveToExistingPath(pathname);
            if (!IsNullOrEmpty(toexitingpath)) pathname = toexitingpath;
            if (!DirExists(pathname))
            {
                try
                {
                    return Directory.CreateDirectory(pathname).Exists;
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    return false;
                }
            }
            return true;
        }

        internal static void writeToLog(string message, params object[] args)
        {
            Console.WriteLine("HostSystem: " + message, args);
        }

        internal static void writeToLog(Exception exception)
        {
            writeToLog(exception, true);
        }

        internal static void writeToLog(Exception exception, bool ignoreable)
        {
            if (ignoreable) writeToLog("" + exception);
            else
            {
                writeToLog("ERROR: " + exception);
                throw exception;
            }
        }

        public static String[] GetFiles(string pathname)
        {
            string exists = ResolveToExistingPath(pathname);
            if (exists != null) pathname = exists;
            if (DirExists(pathname)) return Directory.GetFiles(pathname);
            return new string[0];
        }

        public static string[] GetFiles(string pathname, string ext)
        {
            string exists = ResolveToExistingPath(pathname);
            if (exists != null) pathname = exists;
            if (Directory.Exists(pathname)) return Directory.GetFiles(pathname, ext);
            return new string[0];
        }

        public static AutoClosingStream OpenRead(string path)
        {
            Stream fs = GetStream0(path);
            Func<Stream, Stream> fuct = GetStreamDecoder(path, true);
            return ToAutoCloseStream(fuct(fs), path);
        }

        public static AutoClosingStream ToAutoCloseStream(Stream before, string path)
        {
            if (before == null) return null;
            AutoClosingStream acs = ((before as AutoClosingStream) ?? new AutoClosingStream(before));
            acs.AddToName(path);
            return acs;
        }

        public static AutoClosingStream GetStream(string path)
        {
            Stream before = GetStream0(path);
            if (before == null) return null;
            return ToAutoCloseStream(before, path);
        }
        
        public static Stream GetStream0(string path)
        {
            string toexitingpath = ResolveToExistingPath(path);
            if (File.Exists(toexitingpath)) return File.OpenRead(toexitingpath);
            if (Uri.IsWellFormedUriString(path, UriKind.RelativeOrAbsolute))
            {
                try
                {
                    var uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        return OpenRead(uri.AbsolutePath);
                    }
                }
                catch (Exception)
                {
                }
                WebRequest req = WebRequest.Create(path);
                req.UseDefaultCredentials = true;
                WebResponse resp = req.GetResponse();
                Stream stream = resp.GetResponseStream();
                return stream;
            }
            return null;
        }

        public static string ResolveToExistingPath(string path)
        {
            string physicalPath = ToPhysicalPath(path, true);
            if (!IsNullOrEmpty(physicalPath)) return physicalPath;
           // string betterish = null;
            foreach (var func in PathResolvers)
            {
                string[] existings = func(path);
                if (existings==null) continue;
                foreach (var existing in existings)
                {
                    if (!IsNullOrEmpty(existing))
                    {
                        ///    betterish = existing;
                        string better = ToPhysicalPath(existing, true);
                        if (!String.IsNullOrEmpty(better)) return better;
                        return existing;
                    }                                   
                }
            }
            return null;
        }

        private static string ToPhysicalPath(string path, bool mustExist)
        {
            if (path == null) return null;
            if (File.Exists(path)) return path;
            if (Directory.Exists(path)) return path;
            if (Uri.IsWellFormedUriString(path, UriKind.Absolute))
            {
                try
                {
                    var uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        return ToPhysicalPath(uri.AbsolutePath, mustExist);
                    }
                }
                catch (Exception)
                {
                }
            }
            if (Uri.IsWellFormedUriString(path, UriKind.Relative))
            {
                try
                {
                    var uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        return ToPhysicalPath(uri.AbsolutePath, mustExist);
                    }
                }
                catch (Exception)
                {
                }
            }
            if (!mustExist)
            {
                return Path.GetFullPath(path);
            }
            return null;
        }

        private static Func<Stream, Stream> GetStreamDecoder(string path, bool fakeOne)
        {
            if (!IsNullOrEmpty(path))
            {
                string dir = Path.GetExtension(path);
                if (dir == "gz")
                {
                    return delegate(Stream file0)
                               {
                                   return new GZipStream(file0, CompressionMode.Decompress);
                               };
                }
                if (dir == "z")
                {
                    return delegate(Stream file0)
                    {
                        return new GZipStream(file0, CompressionMode.Decompress);
                    };
                }
            }
            if (fakeOne) return delegate(Stream stream)
                        {
                            return stream;
                        };
            return null;
        }

        public static StreamReader GetStreamReader(string file)
        {
            return new StreamReader(file);
        }

        public static string Combine(string dir, string file)
        {
            return Path.Combine(dir, file);
        }

        public static void AppendAllText(string file, string text)
        {
            File.AppendAllText(file, text);
        }

        public static FileStream Open(string s, FileMode mode)
        {
            return File.Open(s, mode);
        }

        public static FileStream Create(string path)
        {
            return File.Create(path);
        }

        public static string[] GetDirectories(string path)
        {
            return Directory.GetDirectories(path);

        }

        public static string ResolveToURI(string pathIn, IEnumerable<String> combine)
        {

            pathIn = pathIn.Trim();
            foreach (var s in combine)
            {
                string path = Combine(s, pathIn);
                if (HostSystem.DirExists(path))
                {
                    return (new DirectoryInfo(path)).FullName;
                }
                if (HostSystem.FileExists(path))
                {
                    return (new FileInfo(path)).FullName;
                }
                if (Uri.IsWellFormedUriString(path, UriKind.RelativeOrAbsolute))
                {
                    try
                    {
                        var uri = new Uri(path);
                        if (uri.IsFile || uri.IsUnc)
                        {
                            return uri.AbsolutePath;
                        }
                    }
                    catch (Exception exception)
                    {

                    }
                }

            }
            return pathIn;
        }

        public static string GetBaseDir(string pathname)
        {
            string exists = ResolveToExistingPath(pathname);
            if (exists != null) pathname = exists;

            FileSystemInfo info = new DirectoryInfo(pathname);            
            if (info.Exists)
            {
                string maybe = info.FullName;
                if (string.IsNullOrEmpty(maybe)) return maybe;
            }
            if (File.Exists(exists))
            {
                var fi = new FileInfo(pathname);
                string maybe = fi.DirectoryName;
                if (string.IsNullOrEmpty(maybe)) return maybe;
            }
            return Path.GetDirectoryName(pathname);
        }

        public static string GetAbsolutePath(string pathname)
        {
            string exists = ResolveToExistingPath(pathname);
            if (exists == null) exists = pathname;
            if (Directory.Exists(exists))
            {
                return (new DirectoryInfo(exists)).FullName;
            }
            if (File.Exists(exists))
            {
                var fi = new FileInfo(exists);
                return fi.FullName ?? pathname;
            }

            string s = Path.GetFullPath(pathname);
            if (s != pathname)
            {
                writeToLog("Absolute path does not exist " + s + " from " + pathname);

            }
            return pathname;
        }

        public static void Close(Stream stream)
        {
            if (stream == null) return;
            try
            {
                if (stream is AutoClosingStream)
                {
                    ((AutoClosingStream)stream).Close0();
                    return;
                }
                stream.Close();
            }
            catch (Exception e)
            {
                writeToLog("WARNING: closing stream " + e);
            }
        }
    }
}