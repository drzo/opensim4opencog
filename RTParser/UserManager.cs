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
                string name = RTPBot.KeyFromUsername(args);
                if (args!=name)
                {
                    console("use @rmuser " + name);
                    return true;
                }
                myBot.RemoveUser(name);
                return true;
            }
            if (showHelp) console("@rename <full name> [- <old user>]");
            if (cmd == "rename")
            {
                string user, value;
                int found = RTPBot.DivideString(args, "-", out user, out value);
                if (found==1)
                {
                    value = myUser.UserID;
                } else
                {
                    if (found == 0) console("@rename <full name> [- <old user>]");
                }
                myBot.RenameUser(value, user);
                console("Renamed: " + user + " is now known to be " + value);
                return true;
            }

            if (showHelp) console("@setuser <full name>");
            if (cmd == "setuser")
            {
                myBot.LastUser = myBot.FindOrCreateUser(args);
                return true;
            }
            if (showHelp) console("@chuser <full name> [- <old user>]");
            if (cmd == "chuser")
            {
                string oldUser = null;// myUser ?? LastUser.ShortName ?? "";
                string newUser = args;
                int lastIndex = args.IndexOf("-");
                if (lastIndex > 0)
                {
                    oldUser = args.Substring(lastIndex + 1).Trim();
                    newUser = args.Substring(0, lastIndex).Trim();
                }
                if (oldUser != null)
                {
                    myBot.RenameUser(oldUser, newUser);
                }
                myBot.LastUser = myBot.FindOrCreateUser(newUser);
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
                    User user = kv.Value;
                    if (user == null)
                    {
                        console("userid='" + kv.Key
                                + "' NULL");
                        continue;
                    }
                    console("userid='" + kv.Key
                            + "' UserID='" + user.UserID
                            + "' UserName='" + user.UserName + "'");
                    console(user.Predicates.ToDebugString());
                    console("-----------------------------------------------------------------");
                }
                console("------------ENDS USERS----------------------------------");
                return true;
            }
            if (cmd.StartsWith("jmx"))
            {
                RTPBot.writeDebugLine("JMXTRACE: " + args);
                return true;
            }
            return false;
        }
    }
}
