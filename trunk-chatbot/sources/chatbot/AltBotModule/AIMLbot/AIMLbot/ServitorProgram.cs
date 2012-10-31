using System;
using System.IO;
using AIMLbot;
using AltAIMLbot;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using RTParser;

namespace AltAIMLbot
{
    public class ChatProgram
    {
        private ICollectionRequester colreq = new ACollectionRequester();

        private class ACollectionRequester : ICollectionRequester
        {
            public object RequesterID
            {
                get { return this; }
            }

            public object SessionLock
            {
                get { throw new NotImplementedException(); }
            }

            public RequesterSession SessionMananger
            {
                get; set;
            }
        }

        public void TalkActive(string message)
        {
            Console.WriteLine("TalkActive: " + message);
        }


        public static void WriteLine(string s, params object[] args)
        {
            Console.WriteLine("BWL: " + s, args);
        }

        public Servitor servitor = null;
        public bool useServitor = true;
        public string servitorbin = "";
        //public string userID = "consoleUser";
        public string PathToWordNet = null;
        private RTParser.AltBot MyBot;

        public void sayConsole(string message)
        {
            //Default output
            Console.WriteLine("SERVITOR SAYS:{0}", message);
            if (servitor == null) return;
            // Send to blackboard
            servitor.sayResponse(message);
        }

        public void startRobot(string myName)
        {
            //if (servitor == null)
            {

                MyBot = MyBot ?? new Bot();
                try
                {
                    MyBot.ObjectRequester = (ICollectionRequester)colreq;
                    MyBot.outputDelegate = WriteLine;
                    MyBot.SetName(myName);
                    MyBot.sayProcessor = new sayProcessorDelegate(TalkActive);
                }
                catch (Exception e)
                {
                    WriteLine("ERROR {0}", e);
                }
   
                servitor = MyBot.servitor ?? new Servitor(MyBot, null, true, true, true);
                Console.WriteLine("*** Created WN ***");

                servitor.curBot.isAcceptingUserInput = false;
                servitor.curBot.sayProcessor = new sayProcessorDelegate(sayConsole);
                servitor.curBot.useMemcache = true;
 
                Console.WriteLine("WebServitor.serverRoot ={0}", WebServitor.serverRoot);
                Console.WriteLine("servitor.rapStoreDirectory ={0}", servitor.rapStoreDirectory);

                //servitorbin = @"C:\RD4541\Acore\RealBot\RealBot2\RealBot2\RealBot2\bin\Debug\aiml\kotoko_irata\bin\ki.kvt";
                servitorbin = @".\aiml\kotoko_irata\bin\ki.kvt";
                servitor.curBot.myBehaviors.persistantDirectory = @"./BHTrees/";
                //servitor.setBBHash("aimlinstancedir", @"C:\RD4541\Acore\RealBot\RealBot2\RealBot2\RealBot2\bin\Debug\aiml\kotoko_irata");
                // if (File.Exists(servitorbin))
                if (0 == 1)
                {
                    // Load the previous binary data
                    servitor.loadFromBinaryFile(servitorbin);
                    servitor.skiploading = true;
                }
                else
                {
                    // Load the AIML then save the binary
                    //servitor.curBot.loadAIMLFromFiles(@"C:\RD4541\Acore\RealBot\RealBot2\RealBot2\RealBot2\bin\Debug\aiml\kotoko_irata");

                   // servitor.curBot.rapStoreDirectory = null;
                    
                    //servitor.curBot.loadAIMLFromFiles(@"./aiml/chomskyAIML");
                    //servitor.curBot.loadAIMLFromFile(@"./aiml/special/blackjack.aiml");
                    //servitor.curBot.loadAIMLFromFiles(@"./aiml/guest_gurl");
                    // servitor.saveToBinaryFile(servitorbin);
                    servitor.skiploading = true;
                }
                //reloadServitor();
            }
            servitor.curBot.useMemcache = true;
            // FOR DEBUG
            servitor.curBot.inCritical = true;
            servitor.curBot.isAcceptingUserInput = true;
            // FOR TESTING
            servitor.curBot.inCritical = false;
            servitor.curBot.blockCron = false;
            MyBot.WriteConfig();
            MyBot.servitor.loadComplete();

        }
        public bool LoadDataset(string datasetName)
        {
            servitor.DontSkiploading(() => servitor.curBot.loadAIMLFromFiles(@"./aiml/" + datasetName));
            return true;
        }

        public void RunMain(string robotName, string consoleUserName, Action<string> ConsoleWrite, Func<string> ConsoleReadLine, bool sayReposeServ)
        {
            startRobot(robotName);
            servitor.curBot.sayProcessor = (s) => ConsoleWrite(s);
            SetForegrounded(true);
            //saveServitor();
            while (true)
            {
                try
                {
                    string input = ConsoleReadLine();
                    input = input.Trim();
                    if (input.ToLower() == "exit")
                    {
                        break;
                    }
                    else
                    {
                        if (input.Length == 0) continue;
                        string answer = respondToChat(input);
                        ConsoleWrite("Bot: " + answer);
                        if (sayReposeServ) servitor.sayResponse(answer);
                    }
                }
                catch
                { }

            }
        }

        public void SetForegrounded(bool noBackgroundThings)
        {
            return;
            bool enableBG = !noBackgroundThings;
            servitor.tmFSMEnabled = enableBG;
            servitor.tmBehaveEnabled = enableBG;
            //servitor.tmTalkEnabled = false;
            Cron.SuspendCrons = noBackgroundThings;
        }

        public string respondToChat(string input)
        {
            return servitor.respondToChat(input);
        }

        public void Terminate()
        {
            servitor.shutdown();
        }
    }
}
