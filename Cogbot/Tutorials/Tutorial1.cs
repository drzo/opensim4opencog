using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using textsl.Utilities.XMLInterpreter;

namespace cogbot.Tutorials
{
    class Tutorial1 : Tutorial
    {
        private int CommandIdx = 0;

        public Tutorial1(TextForm parent, BotClient botcleint)
            : base(Directory.GetParent(Directory.GetCurrentDirectory()) + "\\XMLTutorials\\tutorial1.xml", parent,botcleint)
        {
            helpString = "Teaches you how to navigate using basic commands move, sit, stand";
            usageString = helpString;
        }

        private void SetNextCommand()
        {
            XmlReader XMLCommand = XMLTutorial.getAllChildren()[CommandIdx];
            parent.WriteLine(XMLCommand["instruction"]);
            AcceptableCommand = XMLCommand["acceptable"];
            FailureMessage = XMLCommand["failure"];
            SuccessMessage = XMLCommand["success"];
            parent.describeNext = false;       
        }

        public override void ExecuteTutorial(string text)
        {          
            if (CommandIdx == 0)
            {
                SetModeTutorial();

                parent.WriteLine("Welcome to your first Tutorial!");
                parent.groupActions["mute"].acceptInputWrapper("mute", "all", parent.WriteLine);
                
                SetNextCommand();
                CommandIdx++;
            }
            else
            {
                string command = text.Split(null)[0];
                if ((AcceptableCommand == text) || (AcceptableCommand == command))
                {
                    if (parent.ExecuteCommand(text)!=String.Empty)
                    {
                        parent.WriteLine(SuccessMessage);
                        if (CommandIdx < CommandCount)
                            SetNextCommand();
                        CommandIdx++;
                        if (CommandIdx == CommandCount + 1)
                        {
                            RestoreMode();
                            CommandIdx = 0;
                            parent.WriteLine("Congratulations!! You completed your first Tutorial! ");
                            parent.groupActions["mute"].acceptInputWrapper("mute", "all", parent.WriteLine);
                        }                     
                    }
                }
                else
                    parent.WriteLine(FailureMessage);
            }
        }

    }
}
