using System;
using System.Collections.Generic;
using System.Text;
using AltAIMLParser;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;

namespace AIMLbot
{/*
    public class Bot : RTParser.AltBot
    {
         
        public Bot()
            : base()
        {
            //useServitor = true;
        }
        public void loadAIMLFromFiles()
        {
            base.loadAIMLFromFiles();
        }
        
    }*/
/*    public class MasterUser : RTParser.UserImpl
    {
        public MasterUser(string UserID, string fullname, AltBot bot, SettingsDictionary dict)
            : base(UserID, fullname, bot, dict)
        {
        }
    }*/
#if false
    sealed public class MasterRequest :
#if interface
        RequestImpl, 
#endif
 Request
    {
/*
        public Request(String rawInput, AltAIMLbot.User user, AltBot bot)
            : this(rawInput, user, bot, null)
        {
        }
      
              public MasterRequest(String rawInput, AltAIMLbot.User user, AltBot bot, AltAIMLParser.Request r)
            : base(rawInput, user, bot, r, null)
        {
        }
*/
        public MasterRequest(string rawInput, User user, string thatSaid, User targetUser, AltBot bot, Request parent, GraphMaster graphMaster)
            : base(rawInput, user, thatSaid, targetUser, bot, parent, graphMaster)
        {
        }
    }
#endif

    public class MasterUser : User
    {

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="UserID">The GUID of the user</param>
        /// <param name="bot">the bot the user is connected to</param>
        public MasterUser(string userID, string fullname, AltBot bot, SettingsDictionary dict)
            : base(userID, fullname, bot, dict)
        {
        }
    }

    public sealed class MasterResult : Result
    {
        public MasterResult(User user, AltAIMLbot.AltBot bot, Request request)
            : base(user, bot, request)
        {
        }

        public MasterResult(string rawInput, User user, AltBot bot, Request parent, User targetUser)
            : base(rawInput, user, bot, parent, targetUser)
        {
        }
    }

}
