using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Text;

// the Namespace used for all BotClient commands
using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Friends
{
    /// <summary>
    /// Shows a list of friends
    /// </summary>
    public class RemoveFriendCommand : Command, BotPersonalCommand
    {        
        /// <summary>
        /// Constructor for FriendsCommand class
        /// </summary>
        /// <param name="testClient">A reference to the BotClient object</param>
        public RemoveFriendCommand(BotClient testClient)
        {
            // The name of the command
            Name = "Remove Friend";
            // A short description of the command with usage instructions
            Description = "Remove avatar friend. Usage: removefriend <avatar/uuid>";
            Category = CommandCategory.Friends;
            Parameters = new[] { new NamedParam(typeof(Avatar), typeof(UUID)) };
        }

        /// <summary>
        /// Get a list of current friends
        /// </summary>
        /// <param name="args">optional testClient command arguments</param>
        /// <param name="fromAgentID">The <seealso cref="OpenMetaverse.UUID"/> 
        /// of the agent making the request</param>
        /// <returns></returns>
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            IEnumerable<SimObject> objs = WorldSystem.GetPrimitives(args, out argsUsed);
            if (argsUsed == 0) return Success(FriendsCommand.ListFriends(Client));
            foreach (SimObject o in objs)
            {
                if (!(o is SimAvatar))
                {
                    Failure("Not avatar " + o);
                    continue;
                }
                WriteLine("TerminateFriendship: " + o);
                Client.Friends.TerminateFriendship(o.ID);                
            }
            return Success("Ran " + Name);           
        }
    }
}