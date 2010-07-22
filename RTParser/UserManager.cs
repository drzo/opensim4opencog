using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MushDLR223.ScriptEngines;

namespace RTParser
{
    public class UserManager
    {
        static public bool BotDirective(RTPBot myBot, User myUser, string input, OutputDelegate console)
        {
            if (input == null) return false;
            input = input.Trim();
            if (input == "") return false;
            if (input.StartsWith("@"))
            {
                input = input.TrimStart(new[] { ' ', '@' });
            }
            myUser = myUser ?? myBot.LastUser ?? myBot.FindOrCreateUser(null);
            int firstWhite = input.IndexOf(' ');
            if (firstWhite == -1) firstWhite = input.Length - 1;
            string cmd = input.Substring(0, firstWhite + 1).Trim().ToLower();
            string args = input.Substring(firstWhite + 1).Trim();
            bool showHelp = false;
            if (cmd == "help")
            {
                showHelp = true;
            }

            if (showHelp)
                console("@rmuser <userid> -- removes a users from the user dictionary\n (best if used after a rename)");
            if (cmd == "rmuser")
            {
                string name = myBot.KeyFromUsername(args);
                if (args!=name)
                {
                    console("use @rmuser " + name);
                    return true;
                }
                myBot.RemoveUser(name);
                return true;
            }
            if (showHelp) console("@setuser <full name> -- Finds or creates and acct and changes the LastUser (current user)");
            if (cmd == "setuser")
            {
                myBot.LastUser = myBot.FindOrCreateUser(args);
                return true;
            }
            if (showHelp) console("@chuser <full name> [- <old user>] --  'old user' if not specified, uses LastUser. \n  Changes the LastUser (current user) and copies the user settings if the old acct was a 'role acct' and reloads the prevoius role settings.");
            if (cmd == "chuser")
            {
                string oldUser = null;// myUser ?? LastUser.ShortName ?? "";
                string newUser = args;
                int lastIndex = args.IndexOf("-");
                if (lastIndex > 0)
                {
                    oldUser = args.Substring(lastIndex).Trim();
                    newUser = args.Substring(0, lastIndex).Trim();
                }
                myBot.LastUser = myBot.ChangeUser(oldUser,newUser);
                return true;
            }
            if (showHelp) console("@rename <full name> [- <old user>] -- if 'old user' if not specified, uses LastUser.\n  if the old user is a role acct, then is the same as @chuser (without resetting current user).  otherwise creates a dictionary alias ");
            if (cmd == "rename")
            {
                string user, value;
                int found = RTPBot.DivideString(args, "-", out user, out value);
                if (found == 1)
                {
                    value = myUser.UserID;
                }
                else
                {
                    if (found == 0) console("use: @rename <full name> [- <old user>]");
                }
                myBot.RenameUser(value, user);
                console("Renamed: " + user + " is now known to be " + value);
                return true;
            }
            if (showHelp) console("@users  --- lists users");
            if (cmd == "users")
            {
                console("-----------------------------------------------------------------");
                console("------------BEGIN USERS----------------------------------");
                foreach (var kv in myBot.BotUsers)
                {
                    console("-----------------------------------------------------------------");
                    WriteUserInfo(console, "key=" + kv.Key, kv.Value);
                    console("-----------------------------------------------------------------");
                }
                console("------------ENDS USERS----------------------------------");
                console("-----------------------------------------------------------------");
                WriteUserInfo(console, "LastUser: ", myBot.LastUser);
                WriteUserInfo(console, "Command caller: ", myUser);
                console("-----------------------------------------------------------------");
                
                return true;
            }
            if (cmd.StartsWith("jmx"))
            {
                RTPBot.writeDebugLine("JMXTRACE: " + args);
                return true;
            }
            return false;
        }

        public static void WriteUserInfo(OutputDelegate console, string name, User user)
        {

            if (user == null)
            {
                console(name + " NOUSER");
                return;
            }
            console(name
                    + " UserID='" + user.UserID
                    + "' UserName='" + user.UserName
                    + "' name='" + user.Predicates.grabSettingNoDebug("name")
                    + "' roleacct='" + user.IsRoleAcct
                    + "' ListeningGraph=" + user.ListeningGraph
                    + "");
        }
    }
}
