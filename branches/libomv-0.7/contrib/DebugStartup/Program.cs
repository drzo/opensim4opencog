using System;
using System.Threading;
using OpenMetaverse;
using Radegast;
using System.Windows.Forms;

namespace DebugStartup
{
    class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException);
            Application.ThreadException += HandleThreadException;
            AppDomain.CurrentDomain.UnhandledException += HandleUnhandledException;
            RadegastInstance instance = RadegastInstance.GlobalInstance;
            Application.Run(instance.MainForm);
            instance = null;
        }
        static void HandleThreadException(object sender, ThreadExceptionEventArgs e)
        {
            Console.WriteLine("!!HandleThreadException!! " + e.Exception);
        }

        static void HandleUnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            Console.WriteLine("!!HandleUnhandledException!! " + e.ExceptionObject);
        }
    }
}
