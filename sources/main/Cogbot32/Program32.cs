using System;

namespace Cogbot
{

    public class Program32
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [MTAThread]
        public static void Main(string[] args)
        {
            Program.Main(args);
		}

        static public int SizeOfIntPtr()
        {
            Console.WriteLine("SizeOf IntPtr32 is: {0}", IntPtr.Size);
            return IntPtr.Size;
        }

    }
}
