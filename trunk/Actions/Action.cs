using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;


namespace cogbot.Actions
{
    abstract public class Action
    {
        public TextForm parent
        {
            get
            {
                return TextForm.SingleInstance;
            }
        }
        public GridClient Client
        {
            get
            {
                return parent.client;
            }
        }
        public string Name;
        protected string helpString;
        protected string usageString;

        public Action(TextForm _parent)
        {
            helpString = "No help information for this action.";
            usageString = "No usage instruction for this action.";

           // parent = _parent;
            //Client = parent.client;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="format"></param>
        /// <param name="arg"></param>
        public void WriteLine(string format, params object[] arg)
        {
            String s = String.Format(format, arg);
            parent.output(s);
            Console.WriteLine(format, arg);
        } // method: WriteLine

        public void acceptInputWrapper(string verb, string args)
        {
            acceptInput(verb, new Parser(args));
        }

        public abstract void acceptInput(string verb, Parser args);

        public virtual string makeHelpString()
        {
            return helpString;
        }
        public virtual string makeUsageString()
        {
            return usageString;
        }

        internal object Execute(string[] args, UUID fromAgentID)
        {          
            throw new Exception("The method or operation is not implemented.");
        }

        public virtual string GetDescription()
        {
            return helpString + "  Usage: " + usageString;
        }
    }
}
