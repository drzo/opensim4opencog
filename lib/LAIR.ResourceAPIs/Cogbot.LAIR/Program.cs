using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace TestApplication
{
    static class Program
    {
        public static string GetDataRoot = "nldata\\";

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Console.Error.WriteLine(".=" + Directory.GetCurrentDirectory());
            Application.Run(new TestFormFrameNet());
        }
    }
}