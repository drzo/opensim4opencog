using System;
using System.IO;
using AltAIMLbot;
using LAIR.ResourceAPIs.WordNet;

namespace RoboKindChat
{
    public class Program
    {
        public static Servitor servitor = null;
        public static bool useServitor = true;
        public static string servitorbin = "";
        public static string userID = "consoleUser";
        public static string PathToWordNet = null;
        private static RoboKindAvroQPIDModuleMain TheRoboKindAvroQPIDModule;

        public static void sayConsole(string message)
        {
            //Default output
            Console.WriteLine("SERVITOR SAYS:{0}", message);
            if (servitor == null) return;
            // Send to blackboard
            servitor.sayResponse(message);
        }

        static void reloadServitor()
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
        static void saveServitor()
        {
           // string servitorbin = GlobalSettings.grabSetting("servitorbin");
            if (servitorbin.Length == 0) return;
            if (!File.Exists(servitorbin))
            {
                servitor.saveToBinaryFile(servitorbin);
                servitor.skiploading = true;
            }
            else
            {
                Console.WriteLine("Skipping saveServitor(): already exists!!!");
            }

        }
        static void startServitor()
        {
            if (useServitor == false) return;
            if (servitor == null)
            {
                WebServitor.kpfile = @"./wikilink/phraseScore";
                WebServitor.wsfile = @"./wikilink/count.phrase.sense.txt";
                WebServitor.bslfile = @"./wikilink/behavior.stoplist.txt";
                WebServitor.serverRoot = @"http://CogbotServer:8123/";

                RaptorDB.Global.SaveTimerSeconds = 60000;
                Console.WriteLine("*** Create servitor ***");

                servitor = new Servitor(userID, null, true,true,true);
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
                if (PathToWordNet != null)
                {
                    Console.WriteLine("*** Starting WN ***");
                    servitor.curBot.wordNetEngine = new WordNetEngine(PathToWordNet, true);
                }
                if (0==1)
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
                    //servitor.curBot.loadAIMLFromFiles(@"./aiml/chomskyAIML");
                    servitor.curBot.loadAIMLFromFiles(@"./aiml/kotoko_irata");
                    servitor.curBot.loadAIMLFromFiles(@"./aiml/justine_degurl");
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

        static void Main(string[] args)
        {
            TheRoboKindAvroQPIDModule = new RoboKindAvroQPIDModuleMain();
            TheRoboKindAvroQPIDModule.Spy();
            startServitor();
            servitor.tmFSMEnabled = false;
            servitor.tmBehaveEnabled = false;
            //servitor.tmTalkEnabled = false;
            Cron.SuspendCrons = true;
            //saveServitor();
            while (true)
            {
                try
                {
                    Console.Write("You: ");
                    string input = Console.ReadLine();
                    if (input.ToLower() == "quit")
                    {
                        break;
                    }
                    else
                    {
                        string answer = servitor.respondToChat(input);
                        Console.WriteLine("Bot: " + answer);
                        servitor.sayResponse(answer);
                    }
                }
                catch
                { }

            }
            servitor.shutdown();

        }
    }
}
