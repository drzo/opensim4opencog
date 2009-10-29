using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using textsl.Utilities.XMLInterpreter;
using OpenMetaverse; //using libsecondlife;


namespace cogbot.Tutorials
{
    public class Tutorial
    {
        public XmlReader XMLTutorial;

        protected ClientManager parent;
        protected GridClient client;
        protected string helpString;
        protected string usageString;
        protected string AcceptableCommand;
        protected string FailureMessage = "Please try again!";
        protected string SuccessMessage = "Congrats! you could do it.";

        private int CommandCnt = 0;
        private int prevMode; 

        public Tutorial(string TutorialPath, ClientManager _parent, BotClient botclient)
        {
            helpString = "No information for what this tutorial will teach.";
            usageString = "No usage instruction for this tutorial.";
            if (File.Exists(TutorialPath))
                XMLTutorial = new XmlReader(TutorialPath);
            else { };
            parent = _parent;
            client = botclient.gridClient;// parent.CurrentClient;                   
        }

        public virtual string makeHelpString()
        {
            return helpString;
        }

        public virtual string makeUsageString()
        {
            return usageString;
        }

        public virtual void SetModeTutorial()
        {
            prevMode = parent.RunningMode;
            parent.RunningMode = (int)Modes.tutorial;
            CommandCnt = XMLTutorial.getAllChildren().Count;
        }

        public virtual void RestoreMode()
        {
            parent.RunningMode = prevMode;
        }

        public virtual int CommandCount
        {
            get { return CommandCnt; }
        }

        public virtual void ExecuteTutorial(string text)
        {
        }
    }
}