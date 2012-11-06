using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Threading;
using System.Collections;

namespace RaptorDB
{
    public class RaptorFileUtil
    {
        public static void FileMove(string from, string to)
        {
            File.Move(RaptorFileUtil.FileSystemPath(from), RaptorFileUtil.FileSystemPath(to));
        }
        public static bool FileExists(string file)
        {
            return File.Exists(RaptorFileUtil.FileSystemPath(file));
        }

        public static string FileSystemPath(string file)
        {
            if (IsOnMonoUnix)
            {
                if (file.Contains("\\"))
                {
                    file = file.Replace('\\', Path.DirectorySeparatorChar);
                }
            }
            return file;
        }

        protected static bool IsOnMonoUnix
        {
            get { return Type.GetType("Mono.Runtime") != null && Environment.OSVersion.Platform == PlatformID.Unix; }
        }
    }
    public class RaptorDB<T> : KeyStore<T> where T : IComparable<T>
    {
        public RaptorDB(string Filename, byte MaxKeySize, bool AllowDuplicateKeys) : base(Filename, MaxKeySize, AllowDuplicateKeys)
        {
        }

        public RaptorDB(string Filename, bool AllowDuplicateKeys) : base( Filename, AllowDuplicateKeys)
        {
        }

        public new static RaptorDB<T> Open(string Filename, bool AllowDuplicateKeys)
        {
            return new RaptorDB<T>(Filename, AllowDuplicateKeys);
        }

        public new static RaptorDB<T> Open(string Filename, byte MaxKeySize, bool AllowDuplicateKeys)
        {
            return new RaptorDB<T>(Filename, MaxKeySize, AllowDuplicateKeys);
        }
    }
}
