using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public enum CommandCategory : int
    {
        Parcel,
        Appearance,
        Movement,
        Simulator,
        Communication,
        Inventory,
        Objects,
        Voice,
        TestClient,
        Friends,
        Groups,
        Other,
        Unknown
    }


    public abstract class Command : cogbot.Actions.Action, IComparable
    {

       // GridClient Client;
        public override string makeHelpString()
        {
            return Description;
        }
        public override string makeUsageString()
        {
            return Description;
        }

        public Command()
            : base(null)
        {
            
        } // constructor


        /// <summary>
        /// 
        /// </summary>
        /// <param name="verb"></param>
        /// <param name="args"></param>
        public override string acceptInput(string verb, cogbot.Actions.Parser args, OutputDelegate WriteLine)
        {
            return Execute(args.tokens, UUID.Zero, WriteLine);
        } // method: acceptInput


		public string Description;
        public CommandCategory Category;

		public new abstract string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine);


        public int CompareTo(object obj)
        {
            if (obj is Command)
            {
                Command c2 = (Command)obj;
                return Category.CompareTo(c2.Category);
            }
            else
                throw new ArgumentException("Object is not of type Command.");
        }
        public override string GetDescription()
        {
            return Description;
        }
    }
}
