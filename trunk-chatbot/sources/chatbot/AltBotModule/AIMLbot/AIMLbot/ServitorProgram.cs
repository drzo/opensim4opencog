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

        string myName = "kotoko irata";

        public void StartupListener00()
        {
            try
            {
                MyBot = MyBot ?? new Bot();
                MyBot.ObjectRequester = (ICollectionRequester)colreq;
                MyBot.useServitor = useServitor;
                MyBot.outputDelegate = WriteLine;
                if (MyBot.useServitor)
                {
                    MyBot.servitor.curBot.sayProcessor = new sayProcessorDelegate(TalkActive);
                }
                MyBot.SetName(myName);
                if (MyBot.useServitor)
                {
                    MyBot.updateRTP2Sevitor();
                }
                MyBot.WriteConfig();
            }
            catch (Exception e)
            {
                WriteLine("ERROR {0}", e);
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
        public string userID = "consoleUser";
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

        public void reloadServitor()
        {
            if (servitor.skiploading) return;
            //string servitorbin = GlobalSettings.grabSetting("servitorbin");
            if (servitorbin.Length == 0) return;
            if (File.Exists(servitorbin))
            {
                servitor.loadFromBinaryFile(servitorbin);
                servitor.skiploading = true;
            }
            else
            {
                Console.WriteLine("No file exists for reloadServitor()");
            }
        }
        public void saveServitor()
        {
            servitor.curBot.saveServitor();
        }

        public void startRobot()
        {
            startServitor00();
            servitor.DontSkiploading(StartupListener00);
        }

        public void startServitor00()
        {
            if (useServitor == false) return;
            //if (servitor == null)
            {
                WebServitor.kpfile = @"./wikilink/phraseScore";
                WebServitor.wsfile = @"./wikilink/count.phrase.sense.txt";
                WebServitor.bslfile = @"./wikilink/behavior.stoplist.txt";
                WebServitor.serverRoot = @"http://CogbotServer:8123/";

                RaptorDB.Global.SaveTimerSeconds = 60000;
                Console.WriteLine("*** Create servitor ***");

                MyBot = MyBot ?? AltBot.FindRobot(myName) ?? new AltBot();
                MyBot.useServitor = true;
                servitor = MyBot.servitor ?? new Servitor(MyBot, userID, null, true, true, true);
                Console.WriteLine("*** Created WN ***");

                servitor.curBot.isAcceptingUserInput = false;
                servitor.curBot.sayProcessor = new sayProcessorDelegate(sayConsole);
                servitor.curBot.useMemcache = true;
                servitor.skiploading = false;
                string rapstorSL = servitor.curBot.GlobalSettings.grabSetting("rapstoreslices");
                if ((rapstorSL != null))
                {
                    servitor.rapStoreSlices = int.Parse(rapstorSL);
                }
                string rapstorTL = servitor.curBot.GlobalSettings.grabSetting("rapstoretrunklevel");
                if ((rapstorTL != null))
                {
                    servitor.rapStoreTrunkLevel = int.Parse(rapstorTL);
                }

                string servRoot = servitor.curBot.GlobalSettings.grabSetting("serverRoot");
                if ((servRoot != null) && (servRoot.Length > 7))
                {
                    WebServitor.serverRoot = servRoot;
                }
                string rapDir = servitor.curBot.GlobalSettings.grabSetting("rapstore");
                servitor.rapStoreDirectory = rapDir;

                Console.WriteLine("WebServitor.serverRoot ={0}", WebServitor.serverRoot);
                Console.WriteLine("servitor.rapStoreSlices ={0}", servitor.rapStoreSlices);
                Console.WriteLine("servitor.rapStoreTrunkLevel ={0}", servitor.rapStoreTrunkLevel);
                Console.WriteLine("servitor.rapStoreDirectory ={0}", servitor.rapStoreDirectory);

                //servitorbin = @"C:\RD4541\Acore\RealBot\RealBot2\RealBot2\RealBot2\bin\Debug\aiml\kotoko_irata\bin\ki.kvt";
                servitorbin = @".\aiml\kotoko_irata\bin\ki.kvt";
                servitor.curBot.myBehaviors.persistantDirectory = @"./BHTrees/";
                //servitor.setBBHash("aimlinstancedir", @"C:\RD4541\Acore\RealBot\RealBot2\RealBot2\RealBot2\bin\Debug\aiml\kotoko_irata");
                // if (File.Exists(servitorbin))
                PathToWordNet = servitor.curBot.GlobalSettings.grabSetting("pathtowordnet");

                if ((PathToWordNet == null) || (PathToWordNet.Length == 0))
                {
                    PathToWordNet = @"./wordnet30";
                }
                if (PathToWordNet != null && servitor.curBot.wordNetEngine == null)
                {
                    Console.WriteLine("*** Starting WN ***");
                    servitor.curBot.wordNetEngine = new WordNetEngine(PathToWordNet, true);
                }
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
                    servitor.curBot.rapStoreDirectory = null;

                    //servitor.curBot.rapStoreDirectory = null;
                    
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

        }
        public bool LoadDataset(string datasetName)
        {
            servitor.DontSkiploading(() => servitor.curBot.loadAIMLFromFiles(@"./aiml/" + datasetName));
            return true;
        }

        public void RunMain(Action<string> ConsoleWrite, Func<string> ConsoleReadLine, bool sayReposeServ)
        {
            startRobot();
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
