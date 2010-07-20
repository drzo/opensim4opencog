using System;
using System.IO;

namespace RTParser.Utils
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
            return Directory.Exists(prefix);
        }

        public static bool BackupFile(string filename)
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

        public static bool FileMove(string filename, string backname)
        {
            try
            {
                File.Move(filename, backname);
                return true;
            }
            catch (Exception e)
            {
                writeToLog(e);
                return false;
            }
        }

        public static bool FileExists(string filename)
        {
            return File.Exists(filename);
        }

        public static bool CreateDirectory(string filename)
        {
            if (!Directory.Exists(filename))
            {
                try
                {
                    Directory.CreateDirectory(filename);
                    return true;
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    return false;
                }
            }
            return false;
        }

        private static void writeToLog(Exception exception)
        {
            Console.WriteLine("" + exception);
        }
    }
}