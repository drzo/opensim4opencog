using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;


namespace cogbot.Actions
{
    abstract public class Action
    {
        /// <summary>
        /// When set to true, think will be called.
        /// </summary>
        public bool Active;

        /// <summary>
        /// Called twice per second, when Command.Active is set to true.
        /// </summary>
        public virtual void Think()
        {
        }


        public BotClient m_Client = null;
        public BotClient Client
        {
            get
            {
                if (m_Client != null) return m_Client;
                return m_Client;// TextForm.SingleInstance.CurrentClient;
            }
            set
            {
                m_Client = value;
            }
        }

        public cogbot.Listeners.WorldObjects WorldSystem
        {
            get
            {
                return Client.WorldSystem;
            }
        }

        public string Name;
        protected string helpString;
        protected string usageString;

        public Action(BotClient _parent)
        {
            helpString = "No help information for this action.";
            usageString = "No usage instruction for this action.";

           // Client = _parent;
            m_Client = _parent;//.CurrentClient;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="format"></param>
        /// <param name="arg"></param>
        public void WriteLine(string format, params object[] arg)
        {
            String s = String.Format(format, arg);            
            Console.WriteLine(s);
            Client.output(s);
        } // method: WriteLine

        public void acceptInputWrapper(string verb, string args)
        {
            acceptInput(verb, new Parser(args));
        }

        public virtual string Execute(string[] args, UUID fromAgentID)
        {
            Parser p = new Parser(String.Join(" ", args));
            p.tokens = args;
            acceptInput(Name, p);
            return String.Empty;
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

        public virtual string GetDescription()
        {
            return helpString + "  Usage: " + usageString;
        }
    }
}
