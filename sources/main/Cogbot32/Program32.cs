using System;

namespace ABuildStartup
{

    public class Program32
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        public static void Main()
        {
			Program.Main();
		}

        static public int SizeOfIntPtr()
        {
            Console.WriteLine("SizeOf IntPtr32 is: {0}", IntPtr.Size);
            return IntPtr.Size;
        }

    }
}
