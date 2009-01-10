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
            : base(cogbot.TextForm.SingleInstance)
        {
            
        } // constructor


        /// <summary>
        /// 
        /// </summary>
        /// <param name="verb"></param>
        /// <param name="args"></param>
        public override void acceptInput(string verb, cogbot.Actions.Parser args) {
            parent.output(Execute(args.tokens, UUID.Zero));
        } // method: acceptInput


		public string Description;
        public CommandCategory Category;

		public new abstract string Execute(string[] args, UUID fromAgentID);

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
