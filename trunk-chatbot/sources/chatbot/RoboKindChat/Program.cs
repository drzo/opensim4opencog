using System;
using System.IO;
using AltAIMLbot;
using LAIR.ResourceAPIs.WordNet;
using RoboKindAvroQPID;

namespace RoboKindChat
{
    public class Program
    {
        private static RoboKindEventModule _theRoboKindEventModule;
        private static ChatProgram _theChatProg;


        static void Main(string[] args)
        {
            DeletePreArtifacts();
            _theRoboKindEventModule = new RoboKindEventModule();
            _theChatProg = new AltAIMLbot.ChatProgram();
            //_theRoboKindEventModule.Spy();
            //_theRoboKindEventModule.Block();
         
            _theChatProg.SetForegrounded(true);
            //_theChatProg.LoadDataset("justine_degurl");
            //_theChatProg.LoadDataset("kotoko_irata");
            //_theChatProg.LoadDataset("test_suite/ProgramD/AIML.aiml");
            //_theChatProg.LoadDataset("special/blackjack.aiml");
            // _theChatProg.LoadDataset("special/lesson_template.aiml"); 
            _theChatProg.RunMain("Nephrael Rae","consoleUser",(s) => Console.Write(s), () =>
                                                              {
                                                                  Console.Write("You: ");
                                                                  return Console.ReadLine();
                                                              }, true);
            _theChatProg.Terminate();
        }

        private static void DeletePreArtifacts()
        {
            var sgm = "./aiml/servitorgraphmap.aiml";
            if (File.Exists(sgm)) File.Delete(sgm);
            string rapStoreDir = "./rapstore";
            if (Directory.Exists(rapStoreDir)) Directory.Delete(rapStoreDir, true);
        }
    }
}
