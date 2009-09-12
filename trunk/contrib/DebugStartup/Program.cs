using System;
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

            RadegastInstance instance = RadegastInstance.GlobalInstance;
            Application.Run(instance.MainForm);
            instance = null;
        }
    }
}
