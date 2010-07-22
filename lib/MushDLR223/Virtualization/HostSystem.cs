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
        public static string BackupExt = ".bak";

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

        public static bool DirExists(string prefix)
        {
            if (IsNullOrEmpty(prefix)) return false;
            return Directory.Exists(prefix);
        }

        static public readonly object ExistenceLock = new object();


        public static bool BackupFile(string filename)
        {
            return BackupFile(filename, true);
        }
        public static bool BackupFile(string filename, bool onlyOne)
        {
            lock (ExistenceLock)
            {
                if (!FileExists(filename)) return true;
                if (!onlyOne) return BackupFileMulti(filename);
                string backname = filename + ".0" + BackupExt;
                if (FileExists(backname))
                {
                    if (!DeleteFile(backname)) return false;
                }
                return FileMove(filename, backname);
            }

        }

        public static bool DeleteFile(string filename)
        {
            if (!FileExists(filename)) return true;
            try
            {
                File.Delete(filename);
                return true;
            }
            catch (Exception exception)
            {
                writeToLog("DELETE FILE: '" + filename + "' " + exception);
                return !FileExists(filename);
            }
        }

        public static bool BackupFileMulti(string filename)
        {
            lock (ExistenceLock)
            {                
                if (!FileExists(filename)) return true;


                string backname = filename + ".0" + BackupExt;
                int i = 0;
                while (FileExists(backname))
                {
                    i++;
                    backname = filename + "." + i + BackupExt;
                }
                return FileMove(filename, backname);
            }
        }

        public static bool FileMove(string filename, string backname)
        {
            lock (ExistenceLock)
            {
                if (IsNullOrEmpty(filename)) return false;
                try
                {
                    File.Move(filename, backname);
                    return true;
                }
                catch (Exception e)
                {
                    writeToLog("Moving " + filename + " -> " + backname + " caused " + e);
                    return false;
                }
            }
        }

        public static bool FileExists(string filename)
        {
            if (IsNullOrEmpty(filename)) return false;
            lock (ExistenceLock) return File.Exists(filename);
        }

        private static bool IsNullOrEmpty(string filename)
        {
            return filename == null || filename.Trim().Length == 0;
        }

        public static bool CreateDirectory(string filename)
        {
            if (IsNullOrEmpty(filename)) return false;
            if (!Directory.Exists(filename))
            {
                try
                {
                    return Directory.CreateDirectory(filename).Exists;
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

        public static String[] GetFiles(string userdir)
        {
            if (DirExists(userdir)) return Directory.GetFiles(userdir);
            return new string[0];
        }

        public static string[] GetFiles(string path, string ext)
        {
            if (DirExists(path)) return Directory.GetFiles(path, ext);
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
            if (File.Exists(path)) return File.OpenRead(path);
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

        public static string GetBaseDir(string baseFile)
        {
            if (HostSystem.DirExists(baseFile))
            {
                return (new DirectoryInfo(baseFile)).FullName;
            }
            if (HostSystem.FileExists(baseFile))
            {
                var fi = new FileInfo(baseFile);
                return fi.DirectoryName ?? baseFile;
            }
            return baseFile;
        }

        public static string GetAbsolutePath(string baseFile)
        {
            if (HostSystem.DirExists(baseFile))
            {
                return (new DirectoryInfo(baseFile)).FullName;
            }
            if (HostSystem.FileExists(baseFile))
            {
                var fi = new FileInfo(baseFile);
                return fi.FullName;
            }
            string s = Path.GetFullPath(baseFile);
            if (s != baseFile)
            {
                writeToLog("Absolute path does not exist " + s + " from " + baseFile);

            }
            return baseFile;
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