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
            _theRoboKindEventModule = new RoboKindEventModule();
            _theChatProg = new AltAIMLbot.ChatProgram();
            _theRoboKindEventModule.Spy();
            _theChatProg.startServitor();
            _theChatProg.SetForegrounded(true);
            _theChatProg.RunMain((s) => Console.Write(s), () =>
                                                              {
                                                                  Console.Write("You: ");
                                                                  return Console.ReadLine();
                                                              }, true);
            _theChatProg.Terminate();
        }
    }
}
