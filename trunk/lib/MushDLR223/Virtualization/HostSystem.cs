using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Net;
using MushDLR223.Utilities;

namespace MushDLR223.Virtualization
{
    public static class HostSystem
    {
        private static readonly List<Func<string, string[]>> _pathResolvers = new List<Func<string, string[]>>();
        public static readonly object ExistenceLock = new object();
        public static readonly string[] NO_EXPANSIONS = new string[0];
        public static string BackupExt = ".bak";
        public static string DirectorySeparator = "/"; // + Path.DirectorySeparatorChar;

        static HostSystem()
        {
            _pathResolvers.Add(ExpandEnvVars);
        }

        public static IEnumerable<Func<string, string[]>> PathResolvers
        {
            get { lock (_pathResolvers) return _pathResolvers.ToArray(); }
        }

        public static string ToCanonicalDirectory(string directory)
        {
            if (directory == null) return null;
            directory = directory.Trim();
            if (directory.Length == 0)
            {
                directory = ".";
            }
            directory = directory.Replace("/./", "/");
            if (Directory.Exists(directory))
            {
                return Slashify(directory);
            }
            if (File.Exists(directory))
            {
                return directory;
            }
            string dn = Path.GetDirectoryName(directory);
            if (Directory.Exists(dn) && !File.Exists(directory))
            {
                return Slashify(directory);
            }
            return Slashify(directory);
        }

        public static string Slashify(string directory)
        {
            if (directory == null) return directory;
            if (directory == "") return "./";
            if (directory == ".") return "./";
            directory = ToForwardSlashes(directory);
            if (File.Exists(directory))
            {
                return directory;
            }
            if (!directory.EndsWith("\\") && !directory.EndsWith("/")) return directory + "/";
            return directory;
        }

        private static string[] ExpandEnvVars(string arg)
        {
            if (arg!=null && arg.StartsWith("%") && arg.EndsWith("%"))
            {
                string s = Environment.GetEnvironmentVariable(arg.Substring(1, arg.Length - 2));
                if (!IsNullOrEmpty(s)) return new[] {s};
            }
            return NO_EXPANSIONS;
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

        public static bool IsWildPath(string str)
        {
            if (str == null) return false;
            if (str.Contains("*"))
            {
                return true;
            }
            return false;
        }

        public static string ToRelativePath(string str, string prefix)
        {
            if (IsWildPath(str))
            {
                string dirName;
                string mask;
                SplitDirAndFile(str,out dirName, out mask);
                try
                {
                    return Combine(ToRelativePath(dirName, prefix), mask);
                }
                catch (Exception e)
                {
                }
            }
            str = ToForwardSlashes(str);
            string strFull = GetFullPath(str);
            bool existed = FileOrDirExists(str);
            if (existed && prefix == null)
            {
                return str;
            }
            string strSaved = str;
            prefix = ToForwardSlashes(prefix);
            bool dirExisted = DirExists(prefix);
            if (prefix == null)
            {
                return str;
            }
            string prefixFull = GetFullPath(prefix);
            int longer = strFull.Length - prefixFull.Length;
            string shared = "";
            if (ToForwardSlashes(strFull).StartsWith(ToForwardSlashes(prefixFull)))
            {
                if (longer == 0)
                {
                    return "./";
                }
                if (longer > 0)
                {
                    string relName = strFull.Substring(prefixFull.Length);
                    if (relName.Length > 1)
                    {
                        return relName;
                    }
                    return str;
                }
                else
                {
                    writeToLog("!ToRelativePath " + prefixFull + " -> " + strFull);
                }
                return str;
            }
            else
            {
                int lastMatch = LastMatch(strFull, prefixFull);
                shared = strFull.Substring(0, lastMatch);
                strFull = strFull.Substring(lastMatch);
                prefixFull = prefixFull.Substring(lastMatch);
                char[] seps = "/\\".ToCharArray();
                int countOfPrefixFull = prefixFull.Split(seps).Length;
                string relPath = "";
                while (--countOfPrefixFull > 0)
                {
                    countOfPrefixFull--;
                    relPath += "../";
                }
                int countOfStrFull = strFull.Split(seps).Length;
                relPath += strFull;

                if (lastMatch == 0)
                {
                    writeToLog("HERE?! " + relPath);
                }
                return relPath;
            }
            if (longer == 0) return "./";
            // str = str.Replace("\\", "/");
            if (DirExists(prefix))
            {
                //if (!prefix.EndsWith("/")) prefix += "/";
                //if (str.StartsWith(prefix)) str = str.Substring(prefix.Length);
                //  str = str.Replace("/", "" + Path.DirectorySeparatorChar);
                if (existed)
                    if (!FileOrDirExists(str))
                    {
                        str = strSaved;
                    }
            }
            return str;
        }

        private static int LastMatch(string left, string right)
        {
            int llen = left.Length;
            int at = 0;
            foreach (char r in right)
            {
                if (llen <= 0)
                {
                    // short
                    return at;
                }
                char l = left[at];

                if (l != r) return at;

                at++;
                llen--;
            }
            return at;
        }

        private static string ToForwardSlashes(string str)
        {
            if (str == null) return str;
            return str.Replace("\\", "/").ToLower();
            if (Directory.Exists(str))
            {
                str = ToCanonicalDirectory(str);
                str = str.Replace("\\", "/").ToLower();
            }
        }

        private static string GetFullPath(string str)
        {
            if (IsWildPath(str))
            {
                string dirname;
                string mask;
                if (SplitDirAndFile(str, out dirname, out mask))
                    return Combine(GetFullPath(dirname), mask);
            }
            String fp = Path.GetFullPath(str);
            if (Directory.Exists(fp)) fp = ToCanonicalDirectory(fp);
            return ToForwardSlashes(fp);
        }

        public static bool FileOrDirExists(string str)
        {
            if (str == null) return false;
            try
            {
                return FileExists(str) || DirExists(str);
            }
            catch (Exception)
            {
                return false;
            }
        }

        public static bool DirExists(string pathname)
        {
            string toexitingpath = ResolveToExistingPath(pathname);
            if (!IsNullOrEmpty(toexitingpath)) pathname = toexitingpath;
            if (IsNullOrEmpty(pathname)) return false;
            return !IsWildPath(pathname) && Directory.Exists(pathname);
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
            if (toexitingpath == null || IsWildPath(pathname)) return false;
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
            DLRConsole.DebugWriteLine("HostSystem: " + message, args);
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

        public static bool SplitDirAndFile(string pathname, out string dirname, out string filemask)
        {            
            filemask = Path.GetFileName(pathname);
            dirname = GetBaseDir(pathname);
            return true;
        }
        public static String[] GetWildFiles(string pathname, out string dirname, out string filemask)
        {
            if (IsWildPath(pathname))
            {
                if (SplitDirAndFile(pathname, out dirname, out filemask))
                {
                    return GetFiles(dirname, filemask);
                }
            }
            string exists = ResolveToExistingPath(pathname);
            string[] file;
            if (exists != null) pathname = exists;
            if (DirExists(pathname))
            {
                filemask = "*";
                dirname = ToCanonicalDirectory(pathname);
                string[] files = Directory.GetFiles(dirname, filemask);
                if (files.Length==0)
                {
                    return files;
                }
                return files;
            }
            filemask = "";
            dirname = "";
            while (pathname.Length > 0)
            {
                if (DirExists(pathname))
                {
                    dirname = ToCanonicalDirectory(pathname);
                    string[] files = Directory.GetFiles(dirname, filemask);
                    return files;
                }
                {
                    string lastDir = Path.GetFileName(pathname);
                    filemask = Combine(lastDir, filemask);
                    dirname = GetBaseDir(pathname);
                }
            }
            return null;
        }

        public static String[] GetFiles(string pathname)
        {
            string exists = ResolveToExistingPath(pathname);
            if (exists != null) pathname = exists;
            if (DirExists(pathname)) return Directory.GetFiles(pathname);

            string dirname = GetBaseDir(pathname);
            if (dirname != null && DirExists(dirname))
            {
                string filename = Path.GetFileName(pathname);
                return Directory.GetFiles(dirname, filename);
            }
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

        public static string ToRelativePath(string userDir)
        {
            return ToRelativePath(userDir, ".");
        }

        public static string ResolveToExistingPath(string path)
        {
            string ret = path;
            try
            {
                ret = ResolveToExistingPath00(path);
                return ret;
            }
            catch (Exception)
            {
                return null;
            }
            return ret;
        }

        public static string ResolveToExistingPath00(string path)
        {
            string physicalPath = ResolveToExistingPath(path, true);
            if (!IsNullOrEmpty(physicalPath))
            {
                string ss = ActualExistingPath(path, true);
                return ss;
            }
            // string betterish = null;
            foreach (var func in PathResolvers)
            {
                string[] existings = func(path);
                if (existings == null) continue;
                foreach (string existing in existings)
                {
                    if (!IsNullOrEmpty(existing))
                    {
                        ///    betterish = existing;
                        string better = ResolveToExistingPath(existing, true);
                        if (!String.IsNullOrEmpty(better)) return better;
                        return existing;
                    }
                }
            }
            return null;
        }
        private static string ResolveToExistingPath(string path, bool mustExist)
        {            
            string rte = ResolveToExistingPath0(path, mustExist);
            if (string.IsNullOrEmpty(rte)) return rte;
            if (rte.Length < path.Length)
            {
                return path;
            }
            return rte;
        }
        private static string ResolveToExistingPath0(string path, bool mustExist)
        {
            if (path == null) return null;
            var ipn = IsWildPath(path);
            if (!ipn)
            {
                if (File.Exists(path))
                {
                    return path;
                }
                string  aep = ActualExistingPath(path, true);
                if (aep != null) return aep;
            }
            if (Uri.IsWellFormedUriString(path, UriKind.Absolute))
            {
                try
                {
                    var uri = new Uri(path);
                    if (uri.IsFile && uri.AbsolutePath != path)
                    {
                        return ResolveToExistingPath(uri.AbsolutePath, mustExist);
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
                    if (mustExist) return null;
                    var uri = new Uri(path);
                    if (uri.IsFile && uri.AbsolutePath != path)
                    {
                        return ResolveToExistingPath(uri.AbsolutePath, mustExist);
                    }
                }
                catch (Exception)
                {
                }
            }
            if (InvalidPathname(path))
            {
                if (mustExist)
                {
                    string d, f;
                    string[] files = GetWildFiles(path, out d, out f);
                    if (files == null || files.Length == 0) return null;
                    if (files.Length == 1)
                    {
                        return files[0];
                    }
                    string pp = ResolveToExistingPath(d, mustExist);
                    if (pp == null) return null;
                    return Combine(pp, f);
                }
                return null;
            }
            if (!mustExist)
            {
                string dn = Path.GetDirectoryName(path);
                string dnCased = ActualExistingDirectoryPath(dn);
                var fnp = Path.GetFileName(path);
                string nfn = Path.Combine(dnCased, fnp);
                return nfn;
            }
            return null;
        }

        private static string ActualExistingPath(string path, bool mustExist)
        {
            if (String.IsNullOrEmpty(path)) return path;
            string dn = Path.GetDirectoryName(path);
            if (File.Exists(path))
            {
                var fi = new FileInfo(path);
                fi.GetAccessControl();
                var pdn = fi.Directory;
                if (pdn == null)
                {
                    return fi.FullName;
                }
                var pdnDepth = 0;
                var pdnDepthDir = pdn;
                while (pdnDepthDir != null)
                {
                    pdnDepthDir = pdnDepthDir.Parent;
                    pdnDepth++;
                }
                var files = pdn.GetFiles();
                string ffn = Path.GetFileName(fi.FullName).ToLower();
                foreach (var newFileInfo in files)
                {
                    if (newFileInfo.Name.ToLower() == ffn)
                    {
                        if (fi.FullName != newFileInfo.FullName)
                        {
                            if (dn.ToLower() == pdn.FullName.ToLower())
                            {
                                return newFileInfo.FullName;
                            }
                            if (dn == "") return newFileInfo.Name;
                            string dnCased = ActualExistingDirectoryPath(dn);
                            return Path.Combine(dnCased, newFileInfo.Name);
                        }
                    }
                }
                return path;
            }
            if (Directory.Exists(path))
            {
                return ActualExistingDirectoryPath(path);
            }
            if (!String.IsNullOrEmpty(dn))
            {
                string dnCased = ActualExistingDirectoryPath(dn);
                string pfull = Path.GetFullPath(path);
                var fnp = Path.GetFileName(path);
                var fi = new FileInfo(path);
                var pdn = new DirectoryInfo(dn);
                var files = pdn.GetFileSystemInfos();
                string ffn = Path.GetFileName(fi.FullName).ToLower();
                foreach (var newFileInfo in files)
                {
                    if (newFileInfo.Name.ToLower() == ffn)
                    {
                        if (fi.FullName != newFileInfo.FullName)
                        {
                            if (dn.ToLower() == pdn.FullName.ToLower())
                            {
                                return newFileInfo.FullName;
                            }
                            return newFileInfo.Name;
                        }
                    }
                }
                if (mustExist) return null;
                string nfn = Path.Combine(dnCased, fnp);
                return nfn;
            }
            return dn;
        }

        private static string ActualExistingDirectoryPath(string path)
        {
            if (String.IsNullOrEmpty(path)) return path;
            string dn = Path.GetDirectoryName(path);
            var fi = new DirectoryInfo(path);
            if (!fi.Exists)
            {
                string dnCased = ActualExistingDirectoryPath(dn);
                var fnp = Path.GetFileName(path);
                string nfn =  Path.Combine(dnCased, fnp);
                return nfn;
            }
            fi.GetAccessControl();
            var pdn = fi.Parent;
            if (pdn == null)
            {
                return fi.FullName;
            }
            var files = pdn.GetDirectories();
            string ffn = Path.GetFileName(fi.FullName).ToLower();
            foreach (var newFileInfo in files)
            {
                if (newFileInfo.Name.ToLower() == ffn)
                {
                    if (fi.FullName != newFileInfo.FullName)
                    {
                        if (dn.ToLower() == pdn.FullName.ToLower())
                        {
                            return newFileInfo.FullName;
                        }
                        if (dn == "") return newFileInfo.Name;
                        string dnCased = ActualExistingDirectoryPath(dn);
                        return Path.Combine(dnCased, newFileInfo.Name);
                    }
                }
            }
            return path;
        }

        private static Func<Stream, Stream> GetStreamDecoder(string path, bool fakeOne)
        {
            if (!IsNullOrEmpty(path))
            {
                string dir = Path.GetExtension(path);
                if (dir == "gz")
                {
                    return delegate(Stream file0) { return new GZipStream(file0, CompressionMode.Decompress); };
                }
                if (dir == "z")
                {
                    return delegate(Stream file0) { return new GZipStream(file0, CompressionMode.Decompress); };
                }
            }
            if (fakeOne) return delegate(Stream stream) { return stream; };
            return null;
        }

        public static StreamReader GetStreamReader(string file)
        {
            return new StreamReader(file);
        }

        public static string Combine(string dir, string file)
        {
            if (dir == null) return file;
            string dirpart = ToCanonicalDirectory(dir);
            if (file == null) return dirpart;
            file = file.Replace("/./", "/");
            dirpart = dirpart.Replace("/./", "/");
            if (file == "./") return dirpart;
            if (dirpart.EndsWith("./")) dirpart = dirpart.Substring(0, dirpart.Length - 2);
            if (file.StartsWith("./")) file = file.Substring(2);
            string res = Path.Combine(dirpart, file);
            return res;
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

        public static string ResolveToExisting(string pathIn, out string p, params string[] combine)
        {
            pathIn = pathIn.Trim();
            return FirstExisting(pathIn, combine, out p);
        }

        public static string ResolveToURI(string pathIn, string[] combine)
        {
            pathIn = pathIn.Trim();
            string prefix;
            string existing = FirstExisting(pathIn, combine, out prefix);
            if (existing != null) return existing;
            return pathIn;
        }

        public static string ResolveToURI(string pathIn, IEnumerable<String> combine, out string prefix)
        {
            pathIn = pathIn.Trim();
            string existing = FirstExisting(pathIn, combine, out prefix);
            if (existing != null) return existing;
            return pathIn;
        }

        public static string FirstExisting(string pathIn, IEnumerable<String> combine, out string p)
        {
            pathIn = pathIn.Trim();
            string prefix = null, fullPath = null;

            string prevCombine = null;
            foreach (string s in combine)
            {
                if (s == prevCombine) continue;
                prevCombine = s;
                string path = Combine(s, pathIn);
                if (path == null) continue;
                if (!FileOrDirExists(path)) continue;
                string ss = ToPathname(path);
                if (ss == null)
                {
                    continue;
                }
                fullPath = path;
                prefix = s;
            }
            p = prefix;
            if (prefix == null)
            {
                return null;
            }
            return fullPath;
        }

        private static string ToPathname(string path)
        {
            if (DirExists(path))
            {
                return (new DirectoryInfo(path)).FullName;
            }
            if (FileExists(path))
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
            return null;
        }

        public static string GetBaseDir(string pathname)
        {
            string s = GetBaseDir0(pathname);
            string d = ToCanonicalDirectory(Slashify(s));
            return ToForwardSlashes(d);
        }

        public static string GetBaseDir0(string pathname)
        {
            string exists = Path.GetDirectoryName(pathname);
            string file = Path.GetFileName(pathname);
            if (IsWildPath(file))
            {
                return exists + "\\";
            }
            exists = ResolveToExistingPath(pathname);
            if (exists != null)
            {
                if (exists != pathname)
                {
                    pathname = exists;
                }
            }
            exists = Path.GetFullPath(pathname);
            

            if (!InvalidPathname(exists))
            {
                try
                {
                    FileSystemInfo info = new DirectoryInfo(pathname);
                    if (info.Exists)
                    {
                        string maybe = info.FullName;
                        if (string.IsNullOrEmpty(maybe)) return maybe;
                    }
                }
                catch (Exception)
                {
                }
            }
            if (Directory.Exists(exists))
            {
                var fi = new DirectoryInfo(pathname);
                DirectoryInfo maybe = fi.Parent;
                if (maybe != null) return maybe.Name;
            }
            if (File.Exists(exists))
            {
                var fi = new FileInfo(pathname);
                string maybe = fi.DirectoryName;
                if (!string.IsNullOrEmpty(maybe)) return maybe;
            }
            exists = Path.GetDirectoryName(pathname);
            return exists;
        }

        private static bool InvalidPathname(string exists)
        {
            char[] pathGetInvalidPathChars = Path.GetInvalidPathChars();
            return exists.IndexOfAny(pathGetInvalidPathChars) != -1;
        }

        public static string GetAbsolutePath(string pathname)
        {
            if (pathname == null) return pathname;
            string exists = ResolveToExistingPath(pathname);
            if (exists == null) exists = pathname;
            var iwp = IsWildPath(pathname);
            if (!iwp && Directory.Exists(exists))
            {
                return (new DirectoryInfo(exists)).FullName;
            }
            if (!iwp && File.Exists(exists))
            {
                var fi = new FileInfo(exists);
                return fi.FullName ?? pathname;
            }

            string s = GetFullPath(pathname);
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
                    ((AutoClosingStream) stream).Close0();
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