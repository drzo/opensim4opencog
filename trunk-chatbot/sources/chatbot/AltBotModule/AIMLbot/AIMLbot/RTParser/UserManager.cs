using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using AIMLbot;
using AltAIMLbot;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser
{
    public partial class AltBot
    {
        public static string UNKNOWN_PARTNER = "UNKNOWN_PARTNER";
        public readonly object ListUserDirs = new object();
        public static bool UseUsersNameInOutput = true;
        public static int UseUserNameEveryNSeconds = 180;
        private readonly List<Action> OnBotCreatedHooks = new List<Action>();
        internal OutputDelegate userTraceRedir;

        public SettingsDictionary Settings
        {
            get { return BotAsUser.Predicates; }
            //set { BotAsUser.Predicates = value; }
        }

        private R UserOper<R>(string operationType, Func<R> action, OutputDelegate output)
        {
            OutputDelegate prev = userTraceRedir;
            //lock (OnBotCreatedHooks) 
            Action needsExit = LockInfo.MonitorTryEnter("UserOper " + operationType, BotUsers, MaxWaitTryEnter);
            try
            {
                userTraceRedir = output;
                try
                {
                    return action();
                }
                catch (ChatSignal ex)
                {
                    throw;
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    if (StaticAIMLUtils.NoRuntimeErrors) return default(R);
                    throw;
                }
            }
            finally
            {
                userTraceRedir = prev;
                needsExit();
            }
        }

        public void QuietLogger(string s, params object[] objects)
        {
            s = SafeFormat("USERTRACE: " + s, objects);
            if (s.ToUpper().Contains("ERROR"))
            {
                writeToLog(s, objects);
            }
        }

        public void writeToUserLog(string s, params object[] objects)
        {
            try
            {
                s = SafeFormat("USERTRACE: " + s, objects);
                if (s.ToUpper().Contains("ERROR"))
                {
                    writeToLog(s, objects);
                }
                if (userTraceRedir != null)
                {
                    userTraceRedir(s);
                    return;
                }
                writeToLog(s);
            }
            catch (Exception exception)
            {
                writeToLog(exception);
            }
        }

        public bool BotUserDirective(User myUser, string input, OutputDelegate console)
        {
            AltBot myBot = this;
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
            if (showHelp)
                console("@setuser <full name> -- Finds or creates and acct and changes the LastUser (current user)");
            if (cmd == "setuser")
            {
                myBot.LastUser = myBot.FindOrCreateUser(args);
                return true;
            }
            if (showHelp)
                console(
                    "@chuser <full name> [- <old user>] --  'old user' if not specified, uses LastUser. \n  Changes the LastUser (current user) and copies the user settings if the old acct was a 'role acct' and reloads the prevoius role settings.");
            if (cmd == "chuser")
            {
                string oldUser = null; // myUser ?? LastUser.ShortName ?? "";
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
            if (showHelp)
                console(
                    "@rename <full name> [- <old user>] -- if 'old user' if not specified, uses LastUser.\n  if the old user is a role acct, then is the same as @chuser (without resetting current user).  otherwise creates a dictionary alias ");
            if (cmd == "rename")
            {
                string user, value;
                int found = DivideString(args, "-", out user, out value);
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
                lock (myBot.BotUsers)
                    lock (microBotUsersLock) foreach (KeyValuePair<string, User> kv in myBot.BotUsers)
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
            string uname = user.Predicates.grabSetting("name");

            console(name
                    + " UserID='" + user.UserID
                    + "' UserName='" + user.UserName
                    + "' name='" + uname
                    + "' roleacct='" + user.IsRoleAcct
                    + "' ListeningGraph='" + user.HeardSelfSayGraph
                    + "' StartGraph='" + user.StartGraph
                    + "'");
        }

        public bool RemoveUser(string name)
        {
            return UserOper("RemoveUser " + name, () => RemoveUser0(name), QuietLogger);
        }

        internal bool RemoveUser0(string name)
        {
            string keyname = KeyFromUsername(name);
            User user = null;            
            {
                if (TryGetValueLocked(microBotUsersLock, BotUsers, name, out user))
                {
                    user.DisposeObject();
                    lock (microBotUsersLock) BotUsers.Remove(name);
                    writeToUserLog("REMOVED " + name);
                }
                if (TryGetValueLocked(microBotUsersLock, BotUsers, keyname, out user))
                {
                    user.DisposeObject();
                    lock (microBotUsersLock) BotUsers.Remove(keyname);
                    writeToUserLog("REMOVED " + keyname);
                }
                else
                {
                    writeToUserLog("rmuser, No user by the name ='" + name + "'");
                    return false;
                }
            }
            return true;
        }

        private static bool TryGetValueLocked<K, V>(object lockerObject, IDictionary<K, V> idict, K key, out V value)
        {
            lock (lockerObject)
            {
                return idict.TryGetValue(key,out value);
            }
        }

        public User FindOrCreateUser(string fromname)
        {
            bool newlyCreated = false;
            return FindOrCreateUser(fromname, out newlyCreated);
        }

        private User FindOrCreateUser0(string fromname)
        {
            //lock (BotUsers)
            {
                bool b;
                User user = FindOrCreateUser0(fromname, out b);
                //if (!IsLastKnownUser(fromname)) user.UserName = fromname;
                return user;
            }
        }

        public User FindUser(string fromname)
        {
            User user = FindUser0(fromname);
            if (user != null) return user;
            return UserOper("FindUser " + fromname, () => FindUser0(fromname), QuietLogger);
        }

        readonly object microBotUsersLock = new object();
        internal User FindUser0(string fromname)
        {
            if (fromname != null && !IsLegalUserName(fromname))
            {
                writeToLog("ERROR: BAd???? FindUser: " + fromname);
                if (!StaticAIMLUtils.NoRuntimeErrors)
                {
                    throw new NullReferenceException("FindUser: " + fromname);
                }
                return null;
            }
            if (IsLastKnownUser(fromname)) return LastUser;
            if (fromname == null)
            {
                if (!StaticAIMLUtils.NoRuntimeErrors)
                {
                    throw new NullReferenceException("FindUser: " + fromname);
                }
                else
                {
                    return LastUser;
                }
            }

            string key = fromname.ToLower().Trim();
            //lock (microBotUsersLock)
            {
                User found;
                if (TryGetValueLocked(microBotUsersLock, BotUsers, key, out found)) return found;
                key = KeyFromUsername(fromname);
                if (TryGetValueLocked(microBotUsersLock, BotUsers, key, out found)) return found;
                if (UnknowableName(fromname))
                {
                    string unk = UNKNOWN_PARTNER.ToLower();
                    if (TryGetValueLocked(microBotUsersLock, BotUsers, unk, out found)) return found;
                }
                return null;
            }
        }

        public bool IsLegalUserName(string fromname)
        {
            var b = IsLegalUserName0(fromname);
            if (b) return true;
            return false;
        }
        private static bool IsLegalUserName0(string fromname)
        {
            if (fromname == null) return false;
            fromname = NoSpaceLowerCaseName(fromname);
            if (fromname == "") return false;
            if (fromname == "to_to") return false;
            if (fromname.Contains("????")) return false;
            if (fromname.Contains("<") || fromname.Contains("\\") || fromname.Contains("sapi"))
            {
                return false;
            }
            if (fromname.Contains(",")) return false;
            if (fromname.Contains("}")) return false;
            if (fromname.Contains("{")) return false;
            if (fromname.Contains("-")) return false;
            return true;
        }

        public User FindOrCreateUser(string fullname, out bool newlyCreated)
        {
            User user = FindUser0(fullname);
            if (user != null)
            {
                newlyCreated = false;
                return user;
            }
            var res = UserOper("FindOrCreateUser " + fullname,
                               () =>
                                   {
                                       bool newlyCreated0;
                                       User user0 = FindOrCreateUser0(fullname, out newlyCreated0);
                                       return new KeyValuePair<User, bool>(user0, newlyCreated0);
                                   }, QuietLogger);
            newlyCreated = res.Value;
            return res.Key;
        }

        internal User FindOrCreateUser0(string fullname, out bool newlyCreated)
        {
            newlyCreated = false;
            //lock (BotUsers)
            {
                string key = KeyFromUsername(fullname);
                User myUser = FindUser(fullname);
                if (myUser != null) return myUser;
                newlyCreated = true;
                myUser = CreateNewUser0(fullname, key);
                return myUser;
            }
        }

        internal User CreateNewUser(string fullname, string key)
        {
            return UserOper("CreateNewUser " + fullname, () => CreateNewUser0(fullname, key), QuietLogger);
        }

        private User CreateNewUser0(string fullname, string key)
        {
            //lock (BotUsers)
            {
                string username = fullname;
                fullname = CleanupFromname(fullname);
                int spaces = TextPatternUtils.CountOf(fullname, " ");
                if (spaces > 1)
                {
                    throw new InvalidCastException("fullname=" + fullname);
                }
                key = key.ToLower();
                User myUser = new MasterUser(key, username, this, null);
                myUser.userTrace = writeToUserLog;
                myUser.UserName = username;
                writeToUserLog("New User " + fullname + " -DEBUG9");
                if (key != null) lock (microBotUsersLock) BotUsers[key] = myUser;
                bool roleAcct = IsRoleAcctName(fullname);
                myUser.IsRoleAcct = roleAcct;
                myUser.AddTodoItem(()=>SetupUserData(myUser, fullname, key));
                return myUser;
            }
        }

        private void SetupUserData(User myUser, string fullname, string key)
        {
            {
                SetupUserWithGraph(fullname, key, myUser);
                //GlobalSettings.AddChild("user." + key + ".", () => myUser.Predicates);

                //OnBotCreated(() => { myUser.Predicates.AddChild("bot.", () => BotAsUser.Predicates); });


                string userdir = GetUserDir(key);
                myUser.SyncDirectory(userdir);
                myUser.AddTodoItem(() =>
                                       {
                                           myUser.RaiseEvent("LOADED", this);
                                       });
                myUser.userTrace = null;
            }
        }

        private void OnBotCreated(Action action)
        {
            lock (OnBotCreatedHooks)
            {
                if (BotAsUser != null) action();
                else OnBotCreatedHooks.Add(action);
            }
        }

        internal void EnsureDefaultUsers()
        {
            UserOper("EnsureDefaultUsers",
                     () =>
                         {
                             EnsureDefaultUsers0();
                             return 0;
                         }, QuietLogger);
        }

        private void EnsureDefaultUsers0()
        {
            return;
            LastUser = FindOrCreateUser0(UNKNOWN_PARTNER);
            LastUser.IsRoleAcct = true;
            LoadUsers(".*");
        }

        public int LoadUsers(string key)
        {
            return UserOper("LoadUsers " + key, () => LoadUsers0(key, PathToUsersDir), QuietLogger);
        }

        internal int LoadUsers0(string key, string from)
        {
            Regex regex = new Regex(key, RegexOptions.IgnoreCase);
            int users = 0;
            string k1 = key.Replace("_", " ");
            foreach (string fsn in HostSystem.GetDirectories(from))
            {
                var files = HostSystem.GetFiles(fsn, "*.xml");
                if (files == null || files.Length == 0) continue;

                string s = fsn;
                if (fsn.StartsWith(from))
                {
                    s = s.Substring(from.Length);
                }
                if (s.StartsWith("/"))
                {
                    s = s.Substring(1);
                }
                if (s.StartsWith("\\"))
                {
                    s = s.Substring(1);
                }
                if (regex.IsMatch(s))
                {
                    string name = s.Replace("_", " ").Replace("~", " ").Replace("  ", " ");
                    MasterUser user = (MasterUser) FindOrCreateUser(name);
                    user.UserDirectory = user.UserDirectory ?? fsn;
                    users++;
                }
            }
            return users;
        }

        public string GetUserDir(string key)
        {
            string sk = "";
            foreach (char s in key)
            {
                if (IsOkForNameChar(s))
                    sk += s;
            }
            lock (ListUserDirs)
                return UserOper("GetUserDir " + key, () => HostSystem.Slashify(GetUserDir0(key)), QuietLogger);
        }

        private string GetUserDir0(string key)
        {
            string userDir = HostSystem.Combine(PathToUsersDir, key);
            string luserDir = HostSystem.ToRelativePath(userDir, RuntimeDirectory);
            if (HostSystem.DirExists(luserDir))
            {
                return luserDir;
            }
            string k1 = key.Replace("_", " ");
            foreach (string fsn in HostSystem.GetDirectories(PathToUsersDir))
            {
                string s = fsn;
                if (fsn.StartsWith(PathToUsersDir))
                {
                    s = s.Substring(PathToUsersDir.Length);
                }
                if (s.StartsWith("/"))
                {
                    s = s.Substring(1);
                }
                if (s.StartsWith("\\"))
                {
                    s = s.Substring(1);
                }
                string s1 = "^" + s.Replace(".", "\\.").Replace("~", ".*").
                                      Replace("~", ".*").Replace("_", "\\b").
                                      Replace("\\b\\b", "\\b") + "$";

                Regex regex;
                try
                {
                    regex = new Regex(s1);
                }
                catch (Exception e)
                {
                    writeToLog("new Regex '" + s1 + "' " + e);
                    continue;
                }
                if (regex.IsMatch(k1))
                {
                    luserDir = HostSystem.ToRelativePath(fsn, RuntimeDirectory);
                    if (HostSystem.DirExists(luserDir))
                    {
                        return luserDir;
                    }
                    return fsn;
                }
            }
            return luserDir;
        }


        public User ChangeUser(string oldname, string newname)
        {
            return UserOper("ChangeUser " + oldname + " " + newname,
                            () => ChangeUser0(oldname, newname), QuietLogger);
        }

        public User ChangeUser0(string oldname, string newname)
        {
            //lock (BotUsers)
            {
                oldname = oldname ?? LastUser.UserName;
                oldname = CleanupFromname(oldname);
                string oldkey = KeyFromUsername(oldname);

                newname = newname ?? LastUser.UserName;
                newname = CleanupFromname(newname);
                string newkey = KeyFromUsername(newname);


                User newuser = FindUser(newkey);
                User olduser = FindUser0(oldname);

                writeToUserLog("ChangeUser " + oldname + " -> " + newname);

                WriteUserInfo(writeToLog, " olduser='" + oldname + "' ", olduser);
                WriteUserInfo(writeToLog, " newuser='" + newname + "' ", newuser);

                if (olduser == null)
                {
                    if (newuser == null)
                    {
                        writeToUserLog("Neigther acct found so creating clean: " + newname);
                        newuser = FindOrCreateUser(newname);
                        LastUser = newuser;
                        return newuser;
                    }
                    if (newuser.IsRoleAcct)
                    {
                        writeToUserLog("User acct IsRole: " + newname);
                        newuser.UserName = newname;
                        return newuser;
                    }
                    writeToUserLog("User acct found: " + newname);
                    newuser = FindOrCreateUser(newname);
                    LastUser = newuser;
                    return newuser;
                }

                if (newuser == olduser)
                {
                    writeToUserLog("Same accts found: " + newname);
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
                            writeToUserLog(
                                "both acct are RoleAcct .. normaly shouldnt happen but just qa boring switchusers ");
                            LastUser = newuser;
                            return newuser;
                        }
                        writeToUserLog("New acct is RoleAcct .. so rebuilding: " + newkey);
                        // remove old "new" acct from dict
                        lock (microBotUsersLock) BotUsers.Remove(newkey);
                        // kill its timer!
                        newuser.DisposeObject();
                        newuser = FindOrCreateUser0(newname);
                        LastUser = newuser;
                        return newuser;
                    }
                    else
                    {
                        writeToUserLog("old acct is just some other user so just switching to: " + newname);
                        newuser = FindOrCreateUser0(newname);
                        // maybe                olduser.Predicates.AddMissingKeys(newuser.Predicates); 
                        LastUser = newuser;
                        return newuser;
                    }
                }
                else
                {
                    if (olduser.IsRoleAcct)
                    {
                        writeToUserLog("Copying old RoleAcct .. and making new: " + newuser);
                        // remove old acct from dict
                        lock (microBotUsersLock) BotUsers.Remove(oldkey);
                        // grab it into new user
                        LastUser = newuser = olduser;
                        lock (microBotUsersLock) BotUsers[newkey] = newuser;
                        newuser.IsRoleAcct = false;
                        SetupUserWithGraph(newname, newkey, newuser);
                        // rebuild an old one
                        CreateNewUser0(oldname, oldkey);
                        return newuser;
                    }
                    else
                    {
                        writeToUserLog("old acct is just some other user so just creating: " + newname);
                        newuser = FindOrCreateUser0(newname);
                        LastUser = newuser;
                        return newuser;
                    }
                }

                writeToUserLog("ERROR, Totally lost so using FindOrCreate and switching to: " + newname);
                newuser = FindOrCreateUser(newname);
                LastUser = newuser;
                return newuser;
            }
        }

        public User RenameUser(string oldname, string newname)
        {
            return UserOper("RenameUser " + oldname + " " + newname,
                            () => RenameUser0(oldname, newname), QuietLogger);
        }

        public User RenameUser0(string oldname, string newname)
        {
            //lock (BotUsers)
            {
                oldname = oldname ?? LastUser.UserName;
                oldname = CleanupFromname(oldname);
                string oldkey = KeyFromUsername(oldname);

                newname = newname ?? LastUser.UserName;
                newname = CleanupFromname(newname);
                string newkey = KeyFromUsername(newname);


                User newuser = FindUser(newkey);
                User olduser = FindUser0(oldname);
                if (olduser == null)
                {
                    writeToUserLog("Neigther acct found so creating clean: " + newname);
                    newuser = FindOrCreateUser(newname);
                    newuser.LoadDirectory(GetUserDir(oldkey));
                    return newuser;
                }

                if (newuser == olduser)
                {
                    writeToUserLog("Same accts found: " + newname);
                    LastUser.UserName = newname;
                    return newuser;
                }

                if (newuser != null)
                {
                    writeToUserLog("both users exists: " + newname);
                    // remove old acct from dict
                    lock (microBotUsersLock) BotUsers.Remove(oldkey);
                    // grab it into new user
                    SettingsDictionaryReal.AddMissingKeys(olduser.Predicates, newuser.Predicates, ObjectRequester);
                    newuser = olduser;
                    lock (microBotUsersLock) BotUsers[newkey] = newuser;
                    newuser.IsRoleAcct = false;
                    SetupUserWithGraph(newname, newkey, newuser);
                    // rebuild an old one
                    CreateNewUser(oldname, oldkey);
                    newuser = FindOrCreateUser(newname);
                    return newuser;
                }

                writeToUserLog("Copying old user .. and making new: " + newuser);
                // remove old acct from dict
                lock (microBotUsersLock) BotUsers.Remove(oldkey);
                // grab it into new user
                newuser = olduser;
                lock (microBotUsersLock) BotUsers[newkey] = newuser;
                newuser.IsRoleAcct = false;
                SetupUserWithGraph(newname, newkey, newuser);
                // rebuild an old one
                CreateNewUser(oldname, oldkey);
                return newuser;


                writeToUserLog("ChangeUser " + oldname + " -> " + newname);

                WriteUserInfo(writeToLog, " olduser='" + oldname + "' ", olduser);
                WriteUserInfo(writeToLog, " newuser='" + newname + "' ", newuser);

                if (olduser == null)
                {
                    if (newuser == null)
                    {
                        writeToUserLog("Neigther acct found so creating clean: " + newname);
                        newuser = FindOrCreateUser(newname);
                        return newuser;
                    }
                    if (newuser.IsRoleAcct)
                    {
                        writeToUserLog("User acct IsRole: " + newname);
                        newuser.UserName = newname;
                        return newuser;
                    }
                    writeToUserLog("User acct found: " + newname);
                    newuser = FindOrCreateUser(newname);
                    return newuser;
                }

                if (newuser == olduser)
                {
                    writeToUserLog("Same accts found: " + newname);
                    LastUser.UserName = newname;
                    return newuser;
                }

                // old user existed
                if (newuser != null)
                {
                    if (newuser.IsRoleAcct)
                    {
                        if (olduser.IsRoleAcct)
                        {
                            writeToUserLog(
                                "both acct are RoleAcct .. normaly shouldnt happen but just qa boring switchusers ");
                            return newuser;
                        }
                        writeToUserLog("New acct is RoleAcct .. so rebuilding: " + newkey);
                        // remove old "new" acct from dict
                        lock (microBotUsersLock) BotUsers.Remove(newkey);
                        // kill its timer!
                        newuser.DisposeObject();
                        newuser = FindOrCreateUser(newname);
                        return newuser;
                    }
                    else
                    {
                        writeToUserLog("old acct is just some other user so just switching to: " + newname);
                        newuser = FindOrCreateUser(newname);
                        return newuser;
                    }
                }
                else
                {
                    if (olduser.IsRoleAcct)
                    {
                        writeToUserLog("Copying old RoleAcct .. and making new: " + newuser);
                        // remove old acct from dict
                        lock (microBotUsersLock) BotUsers.Remove(oldkey);
                        // grab it into new user
                        newuser = olduser;
                        lock (microBotUsersLock) BotUsers[newkey] = newuser;
                        newuser.IsRoleAcct = false;
                        SetupUserWithGraph(newname, newkey, newuser);
                        // rebuild an old one
                        CreateNewUser(oldname, oldkey);
                        return newuser;
                    }
                    else
                    {
                        writeToUserLog("old acct is just some other user so just creating: " + newname);
                        newuser = FindOrCreateUser(newname);
                        return newuser;
                    }
                }

                writeToUserLog("ERROR, Totally lost so using FindOrCreate and switching to: " + newname);
                newuser = FindOrCreateUser(newname);
                return newuser;
            }
        }

        public static bool IsRoleAcctName(string fullname)
        {
            if (UnknowableName(fullname)) return true;
            if (fullname == null) return true;
            fullname = fullname.ToLower();
            return fullname.Contains("global") || fullname.Contains("heard");
        }

        public static bool UnknowableName(string user)
        {
            if (IsNullOrEmpty(user)) return true;
            return IsUnknown(user);
        }

        public bool IsExistingUsername(string fullname)
        {
            return UserOper("IsExistingUsername " + fullname, () => IsExistingUsername0(fullname), QuietLogger);
        }

        public bool IsExistingUsername0(string fullname)
        {
            //lock (BotUsers)
            {
                fullname = CleanupFromname(fullname);
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
                lock (microBotUsersLock) if (BotUsers.TryGetValue(key, out user))
                {
                    if (user.UserID == key || user.UserID == fromname) return true;
                    writeToLog("WARNING! {0} => {1} <= {2}", fromname, key, user.UserID);
                    return true;
                }
                return false;
            }
        }

        public bool DetectUserChange(User myUser, User wasUser, string wasNamed)
        {
            string uname = myUser.UserName;
            if (wasNamed.ToLower().Replace(" ", "_") != uname.ToLower().Replace(" ", "_"))
            {
                var LU = LastUser;
                LastUser = ChangeUser(wasNamed, uname);
                bool changed = wasUser != myUser;
                DLRConsole.SYSTEM_ERR_WRITELINE("ChangeUser: {0} {1}->{2}   {3}->{4}->{5}", changed, LU, LastUser,
                                                wasUser, wasNamed, uname);
                return changed;
                //BotDirective(request, "@chuser " + uname, DEVNULL);
            }
            return false;
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
            return ToScriptableName(fromname);
        }

        public string KeyFromUsername(string fromname)
        {
            if (IsLastKnownUser(fromname))
            {
                if (LastUser != null)
                {
                    fromname = LastUser.UserName;
                }
            }
            if (UnknowableName(fromname))
            {
                fromname = UNKNOWN_PARTNER;
            }
            return CleanupFromname(fromname).ToLower();
        }

        public bool IsLastKnownUser(string fromname)
        {
            if (LastUser != null && LastUser.IsNamed(fromname)) return false;
            return (string.IsNullOrEmpty(fromname) || fromname.Trim() == "null");
        }

        public static bool IsOkForNameChar(char s)
        {
            return s == '_' || s == ' ' || s == '-' || char.IsLetterOrDigit(s);
        }


        private string ToPath(string path, out string prefix)
        {
            if (path == null)
            {
                prefix = null;
                return null;
            }
            string realPath = HostSystem.FirstExisting(path, RuntimeDirectories, out prefix);
            if (realPath != null) return realPath;
            return HostSystem.Combine(prefix, path);
        }

        public string GetPathSetting(string namePath, string defaultVal)
        {
            string retP = SettingPath0(namePath, defaultVal);
            if (retP == null)
            {
                return defaultVal;
            }
            string ret = HostSystem.ToRelativePath(retP, RuntimeDirectory);
            string retA = HostSystem.GetAbsolutePath(retP);
            return ret;
        }
        private string SettingPath0(string namePath, string defaultVal)
        {
            string prefix;
            string res = ToPath(GlobalSettings.grabSetting(namePath), out prefix);
            if (res != null) return res;
            if (defaultVal != null)
            {
                res = ToPath(GlobalSettings.grabSetting(defaultVal), out prefix);
                if (res != null) return res;
            }
            return defaultVal;
        }

        public string OutputResult(InteractionResult res, OutputDelegate console, bool includeWeigth)
        {
            User CurrentUser = res.Requester.Value;
            string user = CurrentUser.UserName;
            string useOut = res.EnglishOutput;
            double vscored;

            double scored = res.Score;

            bool useNameInOutput = false;
            if (!string.IsNullOrEmpty(useOut))
            {
                string oTest = ToEnglish(useOut);
                if (oTest != null && oTest.Length > 2)
                {
                    useOut = oTest;
                }

                string tUser = CurrentUser.UserName;
                if (tUser.Length > 2)
                {
                    bool newlyCreated;
                    var orCreateUser = FindOrCreateUser(tUser, out newlyCreated);
                    if (orCreateUser != CurrentUser)
                    {
                        CurrentUser = orCreateUser;
                        user = orCreateUser.UserName;

                    }
                    if (newlyCreated) useNameInOutput = true;
                }
            }
            double dateTimeNowSubtractMinutes = DateTime.Now.Subtract(CurrentUser.NameUsedOrGivenTime).Seconds;
            if (UseUserNameEveryNSeconds > 0 && dateTimeNowSubtractMinutes >= UseUserNameEveryNSeconds)
            {
                useNameInOutput = true;
            }
            if (string.IsNullOrEmpty(useOut))
            {
                useOut = "Interesting.";
                res.TemplateRating = Math.Max(res.Score, 0.5d);
            }
            else useOut = useOut.Replace("_", " ").Replace("  ", " ").Trim();

            var stringPlit = useOut.Split(new[] { "(menevalue= " }, StringSplitOptions.RemoveEmptyEntries);
            string said = useOut;
            if (stringPlit.Length > 0)
            {
                said = stringPlit[0];
            }
            string vstring = stringPlit.Length < 2
                                 ? null
                                 : stringPlit[2].Split(new char[] { ' ', '\n' }, StringSplitOptions.RemoveEmptyEntries)[0];
            if (vstring == null || double.TryParse(vstring, out vscored))
            {
                if (!AltBot.IncludeMeNeValue)
                {
//                    useOut = string.Format("{0}", said);
                    useOut = string.Format("{0} menevalue= 1{1:1} ", said, res.Score * 1.4);

                }
                else
                {
                    useOut = string.Format("{0} menevalue= {1:1} ", said, res.Score * 1.4);
                }

            }
            user = user.Replace("_", " ").Replace("  ", " ");
            useOut = useOut.Replace("_", " ").Replace("  ", " ");
            if (AltBot.UseUsersNameInOutput && useNameInOutput && !user.Contains("UNKNOWN"))
            {
                if (!useOut.ToLower().Contains(user.ToLower()))
                {
                    useOut = user + ", " + useOut;
                    CurrentUser.NameUsedOrGivenTime = DateTime.Now;
                }
            }
            console(useOut);
            if (includeWeigth) return useOut;
            return said;
        }

        public bool IsInteractiveUser(IUser value)
        {
            return value != null && value != _botAsUser && value != ExemplarUser;
        }
    }
}