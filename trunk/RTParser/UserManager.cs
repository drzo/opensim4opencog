using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MushDLR223.ScriptEngines;
using MushDLR223.Virtualization;
using RTParser.Utils;

namespace RTParser
{
    public partial class RTPBot
    {
        public static string UNKNOWN_PARTNER = "UNKNOWN_PARTNER";

        public bool BotUserDirective(User myUser, string input, OutputDelegate console)
        {

            RTPBot myBot = this;
            if (input == null) return false;
            input = input.Trim();
            if (input == "") return false;
            if (input.StartsWith("@"))
            {
                input = input.TrimStart(new[] { ' ', '@' });
            }
            // myUser = myUser ?? myBot.LastUser ?? myBot.FindOrCreateUser(null);
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
                if (args != name)
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
                myBot.LastUser = myBot.ChangeUser(oldUser, newUser);
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
                lock (myBot.BotUsers) foreach (var kv in myBot.BotUsers)
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
            if (cmd.Contains("jmx"))
            {
                writeToLog("JMXTRACE: " + args);
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

        public void RemoveUser(string name)
        {
            string keyname = KeyFromUsername(name);
            User user;
            if (BotUsers.TryGetValue(name, out user))
            {
                user.Dispose();
                BotUsers.Remove(name);
                writeToLog("USERTRACE: REMOVED " + name);
            }
            else
                if (BotUsers.TryGetValue(keyname, out user))
                {
                    user.Dispose();
                    BotUsers.Remove(keyname);
                    writeToLog("USERTRACE: REMOVED " + keyname);
                }
                else
                {
                    writeToLog("USERTRACE: rmuser, No user by the name ='" + name + "'");
                }
        }

        public User FindOrCreateUser(string fromname)
        {
            lock (BotUsers)
            {
                bool b;
                User user = FindOrCreateUser(fromname, out b);
                user.UserName = fromname;
                return user;
            }
        }

        public User FindUser(string fromname)
        {
            if (IsLastKnownUser(fromname)) return LastUser;
            string key = fromname.ToLower().Trim();
            lock (BotUsers)
            {
                if (BotUsers.ContainsKey(key)) return BotUsers[key];
                if (UnknowableName(fromname))
                {
                    var unk = UNKNOWN_PARTNER.ToLower();
                    if (BotUsers.ContainsKey(unk)) return BotUsers[unk];                    
                }
                return null;
            }
        }

        public User FindOrCreateUser(string fullname, out bool newlyCreated)
        {
            newlyCreated = false;
            lock (BotUsers)
            {
                string key = KeyFromUsername(fullname);
                User myUser = FindUser(fullname);
                if (myUser != null) return myUser;
                newlyCreated = true;
                myUser = CreateNewUser(fullname, key);
                return myUser;
            }
        }

        private User CreateNewUser(string fullname, string key)
        {
            lock (BotUsers)
            {
                User myUser = new AIMLbot.User(key, this);
                myUser.UserName = fullname;
                writeToLog("USERTRACE: New User " + fullname);
                BotUsers[key] = myUser;
                if (!UnknowableName(fullname))
                {
                    myUser.Predicates.addSetting("name", fullname);
                    myUser.IsRoleAcct = false;
                    myUser.ListeningGraph = GetUserGraph(key, GraphMaster);
                }
                else
                {
                    myUser.IsRoleAcct = true;
                    myUser.ListeningGraph = GetUserGraph(key, GraphMaster);
                }
                myUser.Predicates.addSetting("name", fullname);
                string userdir = GetUserDir(key);
                myUser.SyncDirectory(userdir);
                return myUser;
            }
        }

        private string GetUserDir(string key)
        {
            string userDir = HostSystem.Combine(PathToUserDir, key);
            HostSystem.CreateDirectory(userDir);
            return HostSystem.ToRelativePath(userDir);
        }


        public User ChangeUser(string oldname, string newname)
        {
            lock (BotUsers)
            {
                oldname = oldname ?? LastUser.UserName;
                oldname = CleanupFromname(oldname);
                string oldkey = KeyFromUsername(oldname);

                newname = newname ?? LastUser.UserName;
                newname = CleanupFromname(newname);
                string newkey = KeyFromUsername(newname);


                User newuser = FindUser(newkey);
                User olduser = FindUser(oldname);

                writeToLog("USERTRACE: ChangeUser " + oldname + " -> " + newname);

                WriteUserInfo(writeToLog, " olduser='" + oldname + "' ", olduser);
                WriteUserInfo(writeToLog, " newuser='" + newname + "' ", newuser);

                if (olduser == null)
                {
                    if (newuser == null)
                    {
                        writeToLog("USERTRACE: Neigther acct found so creating clean: " + newname);
                        newuser = FindOrCreateUser(newname);
                        LastUser = newuser;
                        return newuser;
                    }
                    if (newuser.IsRoleAcct)
                    {
                        writeToLog("USERTRACE: User acct IsRole: " + newname);
                        newuser.UserName = newname;
                        return newuser;
                    }
                    writeToLog("USERTRACE: User acct found: " + newname);
                    newuser = FindOrCreateUser(newname);
                    LastUser = newuser;
                    return newuser;
                }

                if (newuser == olduser)
                {
                    writeToLog("USERTRACE: Same accts found: " + newname);
                    LastUser.UserName = newname;
                    LastUser = newuser;
                    return newuser;
                }

                // old user existed
                if (newuser != null)
                {
                    if (newuser.IsRoleAcct)
                    {
                        if (olduser.IsRoleAcct)
                        {
                            writeToLog("USERTRACE: both acct are RoleAcct .. normaly shouldnt happen but just qa boring switchusers ");
                            LastUser = newuser;
                            return newuser;
                        }
                        writeToLog("USERTRACE: New acct is RoleAcct .. so rebuilding: " + newkey);
                        // remove old "new" acct from dict
                        BotUsers.Remove(newkey);
                        // kill its timer!
                        newuser.Dispose();
                        newuser = FindOrCreateUser(newname);
                        LastUser = newuser;
                        return newuser;
                    }
                    else
                    {
                        writeToLog("USERTRACE: old acct is just some other user so just switching to: " + newname);
                        newuser = FindOrCreateUser(newname);
                        LastUser = newuser;
                        return newuser;
                    }
                }
                else
                {
                    if (olduser.IsRoleAcct)
                    {
                        writeToLog("USERTRACE: Copying old RoleAcct .. and making new: " + newuser);
                        // remove old acct from dict
                        BotUsers.Remove(oldkey);
                        // grab it into new user
                        LastUser = newuser = olduser;
                        BotUsers[newkey] = newuser;
                        newuser.IsRoleAcct = false;
                        newuser.ListeningGraph = GetUserGraph(newkey, GraphMaster);
                        newuser.UserID = newkey;
                        newuser.UserName = newname;
                        newuser.SyncDirectory(GetUserDir(newkey));
                        // rebuild an old one
                        CreateNewUser(oldname, oldkey);
                        return newuser;
                    }
                    else
                    {
                        writeToLog("USERTRACE: old acct is just some other user so just creating: " + newname);
                        newuser = FindOrCreateUser(newname);
                        LastUser = newuser;
                        return newuser;
                    }
                }

                writeToLog("USERTRACE: ERROR, Totally lost so using FindOrCreate and switching to: " + newname);
                newuser = FindOrCreateUser(newname);
                LastUser = newuser;
                return newuser;
            }
        }


        public void RenameUser(string oldname, string newname)
        {
            lock (BotUsers)
            {
                oldname = CleanupFromname(oldname);
                string oldkey = KeyFromUsername(oldname);
                newname = CleanupFromname(newname);
                string newkey = KeyFromUsername(newname);

                writeToLog("USERTRACE: Rename User " + oldname + " -> " + newname);

                User newuser = FindUser(newname);
                User olduser = FindUser(oldname);
                if (olduser == null)
                {
                    if (newuser == null)
                    {
                        newuser = FindOrCreateUser(newname);
                    }
                    return;
                }

                // old user existed
                if (newuser != null)
                {
                    // copy settings
                    newuser.LoadDirectory(olduser.UserDirectory);
                }

                if (UnknowableName(oldname))
                {
                    User nextuser = CreateNewUser(oldname, oldkey);
                    BotUsers[oldkey] = nextuser;
                }

                BotUsers[newkey] = olduser;
                olduser.Predicates.addSetting("name", newname);
            }
        }

        public string SetUser(string user, string lastKnownUser, OutputDelegate ConsoleWriteLine)
        {
            string setUser;
            ConsoleWriteLine("<- SetUser=" + user + " lastKnownUser=" + lastKnownUser);
            if (UnknowableName(user))
            {
                if (UnknowableName(lastKnownUser))
                {
                    ConsoleWriteLine("THEREFORE Same persom with still unknown name (make one up)");
                    setUser = lastKnownUser = UNKNOWN_PARTNER;
                }
                else
                {
                    ConsoleWriteLine("THEREFORE New Person with unknown name");
                    setUser = lastKnownUser = user;
                }
            }
            else
            {
                if (UnknowableName(lastKnownUser))
                {
                    ConsoleWriteLine("THEREFORE Same Person now known name");
                    RenameUser(lastKnownUser, user);
                    setUser = lastKnownUser = user;
                }
                else
                {
                    if (SameUser(lastKnownUser, user))
                    {
                        ConsoleWriteLine("THEREFORE Different Person with known name");
                        setUser = user;
                        lastKnownUser = user;
                    }
                    else
                    {
                        ConsoleWriteLine("THEREFORE New Person with known name");
                        setUser = user;
                        lastKnownUser = user;
                    }
                }
            }
            user = setUser;
            ConsoleWriteLine("-> SetUser=" + user + " lastKnownUser=" + lastKnownUser);
            return user;
        }

        public static bool UnknowableName(string user)
        {
            if (String.IsNullOrEmpty(user)) return true;
            string s = " " + user.ToUpper().Replace("-", " ").Replace("  ", " ").Replace(" ", "") + " ";
            bool b = s.Contains("UNKNOWN") || s.Contains("UNREC") || s.Contains("UNNAME")
                || s.Contains("UNSEEN") || s.Contains("DEFAULT") ||
                   s.Contains(" SOME") || s.Contains("*") || s.Contains(" _ ");
            return b;
        }

        public bool IsExistingUsername(string fullname)
        {
            lock (BotUsers)
            {
                if (null == fullname)
                {
                    return false;
                }
                String fromname = CleanupFromname(fullname);
                if (string.IsNullOrEmpty(fromname))
                {
                    return false;
                }
                String key = KeyFromUsername(fullname);
                User user;
                if (BotUsers.TryGetValue(key, out user))
                {
                    if (user.UserID == key || user.UserID == fromname) return true;
                    writeToLog("USERTRACE WARNING! {0} => {1} <= {2}", fromname, key, user.UserID);
                    return true;
                }
                return false;
            }
        }


        public string CleanupFromname(string fromname)
        {
            if (IsLastKnownUser(fromname))
            {
                if (LastUser != null)
                {
                    return LastUser.UserName;
                }
                else
                {
                    fromname = UNKNOWN_PARTNER;
                }
            }
            fromname = fromname.Trim();
            return fromname.Trim().Replace(" ", "_").Replace(".", "_").Replace("-", "_").Replace("__", "_");
        }

        public string KeyFromUsername(string fromname)
        {
            if (IsLastKnownUser(fromname))
            {
                if (LastUser != null)
                {
                    fromname = LastUser.UserID;
                }
            }
            if (UnknowableName(fromname))
            {
                fromname = RTPBot.UNKNOWN_PARTNER;
            }
            return CleanupFromname(fromname).ToLower();
        }

        public bool IsLastKnownUser(string fromname)
        {
            //if (LastUser == null) return false;
            return (string.IsNullOrEmpty(fromname) || fromname.Trim() == "null");
        }
    }
}
