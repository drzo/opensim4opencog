using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.IO;
using cogbot.Actions.SimExport;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.StructuredData;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{
    public partial class ImportCommand 
    {
        public sealed class UserOrGroupMapping : UUIDChange
        {
            public override bool Equals(object obj)
            {
                var other = obj as UserOrGroupMapping;
                return (other != null) && (other.OldID == OldID);
            }
            public override int GetHashCode()
            {
                return OldID.GetHashCode();
            }
            public override string ToString()
            {
                return OldName + " " + OldID + " -> " + NewName + " " + base.NewID;
            }

            public bool IsGroup = false;
            public string OldName;
            public string _newName;
            public string NewName
            {
                get
                {
                    if (_newName != null) return _newName;
                    lock (RenamedWhosit)
                    {
                        string newName;
                        if (RenamedWhosit.TryGetValue(OldName, out newName))
                        {
                            return _newName = newName;
                        }
                    }
                    return OldName;
                }
                set
                {
                    _newName = value;
                }
            }
            public string ErrorMessage;
            private ManualResetEvent WaitOnCreate;

            public UserOrGroupMapping(UUID id, String name, bool isGroup)
            {
                OldID = id;
                UUID2OBJECT[id] = this;
                IsGroup = isGroup;
                OldName = name;
            }
            public override UUID NewID
            {
                get
                {
                    UUID newID = base.NewID;
                    if (CogbotHelpers.IsNullOrZero(newID))
                    {
                        newID = FindGroup(NewName);
                        if (CogbotHelpers.IsNullOrZero(newID))
                        {
                            newID = CreateGroup(NewName);
                        }
                        NewID = newID;
                    }
                    return base.NewID;
                }
                set
                {
                    base.NewID = value;
                    NewUUID2OBJECT[value] = this;
                }
            }

            public override void ReplaceAll()
            {
                var v = this.NewID;
            }

            private UUID FindGroup(string name)
            {
                if (!IsGroup) return FindUser(name);
                if (!CogbotHelpers.IsNullOrZero(base.NewID)) return base.NewID;
                if (IsLocalScene)
                {
                    if (OldName == name)
                    {
                        return base.NewID = OldID;
                    }
                    return base.NewID = UUID.Random();
                }
                WaitOnCreate = WaitOnCreate ?? new ManualResetEvent(false);
                WaitOnCreate.Reset();
                Running.Client.Directory.DirGroupsReply += GroupSearchReply;
                Running.Client.Directory.StartGroupSearch(name, 0, OldID);
                if (!WaitOnCreate.WaitOne(3000))
                {
                    return Running.Client.Self.ActiveGroup;
                } 
                return base.NewID;
            }

            private UUID FindUser(string name)
            {
                if (IsGroup) return FindGroup(name);
                if (!CogbotHelpers.IsNullOrZero(base.NewID)) return base.NewID;
                if (IsLocalScene)
                {
                    if (OldName == name)
                    {
                        return base.NewID = OldID;
                    }
                    return base.NewID = UUID.Random();
                }
                WaitOnCreate = WaitOnCreate ?? new ManualResetEvent(false);
                WaitOnCreate.Reset();
                // should we use this instead??
                //    Running.Client.Avatars.RequestAvatarNameSearch(name.Replace(" ", ", "), OldID);
                Running.Client.Directory.DirPeopleReply += PeopleSearchReply; 
                Running.Client.Directory.StartPeopleSearch(name, 0, OldID);
                WaitOnCreate.WaitOne(3000);
                return base.NewID;
            }

            private UUID CreateGroup(string name)
            {
                if (!CogbotHelpers.IsNullOrZero(base.NewID)) return base.NewID;
                if (!IsGroup) return CreatePerson(name);
                Group newGroup = GetNewGroup();
                WaitOnCreate.Reset();
                if (IsLocalScene)
                {
                    if (OldName == name)
                    {
                        return base.NewID = OldID;
                    }
                    return base.NewID = UUID.Random();
                }
                Running.Client.Groups.GroupCreatedReply += GroupCreateReply;
                Running.Client.Groups.RequestCreateGroup(newGroup);
                if (!WaitOnCreate.WaitOne(5000))
                {
                    return Running.Client.Self.ActiveGroup;
                }
                return base.NewID;
            }

            private Group GetNewGroup()
            {
                Group newGroup = new Group();
                newGroup.Name = NewName ?? OldName;
                newGroup.AcceptNotices = true;
                newGroup.AllowPublish = true;
                newGroup.Charter = "Imported from " + OldName + " " + OldID;
                newGroup.OpenEnrollment = true;
                newGroup.MembershipFee = 0;
                newGroup.ShowInList = true;
                newGroup.MaturePublish = false;
                return newGroup;
            }

            /// <summary>
            /// For now it just returns our name
            /// </summary>
            /// <param name="name"></param>
            /// <returns></returns>
            private UUID CreatePerson(string name)
            {
                if (!CogbotHelpers.IsNullOrZero(base.NewID)) return base.NewID;
                if (name == "MISSINGPERSON" && !CogbotHelpers.IsNullOrZero(Running.MISSINGPERSON))
                    return Running.MISSINGPERSON;
                if (name == "LINDENZERO" && !CogbotHelpers.IsNullOrZero(Running.LINDENZERO))
                    return Running.LINDENZERO;
                if (IsLocalScene)
                {
                    if (OldName == name)
                    {
                        return base.NewID = OldID;
                    }
                    return base.NewID = UUID.Random();
                }
                if (IsGroup) return Running.Client.Self.ActiveGroup;
                return base.NewID = Running.Client.Self.AgentID;
            }

            private void GroupCreateReply(object sender, GroupCreatedReplyEventArgs e)
            {
                Running.Client.Groups.GroupCreatedReply -= GroupCreateReply;
                ErrorMessage = e.Message;
                if (e.Success)
                {
                    NewID = e.GroupID;
                    Running.Client.Groups.UpdateGroup(NewID, GetNewGroup());
                }
                WaitOnCreate.Set();
            }

            private void PeopleSearchReply(object sender, DirPeopleReplyEventArgs e)
            {
                if (e.QueryID != OldID) return;
                Running.Client.Directory.DirPeopleReply -= PeopleSearchReply;
                foreach (var match in e.MatchedPeople)
                {
                    if (match.FirstName + " " + match.LastName == NewName)
                    {
                        NewID = match.AgentID;
                        return;
                    }
                }
                WaitOnCreate.Set();
            }

            private void GroupSearchReply(object sender, DirGroupsReplyEventArgs e)
            {
                if (e.QueryID != OldID) return;
                Running.Client.Directory.DirGroupsReply -= GroupSearchReply;
                foreach (var match in e.MatchedGroups)
                {
                    if (match.GroupName == NewName)
                    {
                        NewID = match.GroupID;
                        return;
                    }
                }
                WaitOnCreate.Set();
            }
        }

        public static readonly Dictionary<string, string> RenamedWhosit = new Dictionary<string, string>();
        private UUID LINDENZERO = UUID.Zero;
        private UUID MISSINGPERSON = UUID.Zero;
        public void LoadUsersAndGroups()
        {
            string nameChangesFile = ExportCommand.siminfoDir + "..\\nameChanges.txt";
            if (File.Exists(nameChangesFile))
            {
                string[] nameChanges = File.ReadAllLines(nameChangesFile);
                foreach (var entry in nameChanges)
                {
                    string[] entrySplit = entry.Split(',');
                    string n1 = entrySplit[0];
                    string n2 = n1;
                    if (entrySplit.Length > 1)
                    {
                        n2 = entrySplit[1];
                    }
                    RenamedWhosit[n1] = n2;                    
                }
            }
            string found;
            if (RenamedWhosit.TryGetValue("LINDENZERO", out found))
            {
                LINDENZERO = WorldSystem.FindUUIDForName(found);
            }
            if (RenamedWhosit.TryGetValue("MISSINGPERSON", out found))
            {
                MISSINGPERSON = WorldSystem.FindUUIDForName(found);
            }

            foreach (string file in Directory.GetFiles(ExportCommand.siminfoDir, "*.avatar"))
            {
                string[] content = File.ReadAllText(file).Split(',');
                UUID key = UUID.Parse(Path.GetFileNameWithoutExtension(file));
                UserOrGroupMapping mapping = new UserOrGroupMapping(key, content[0], false);
                UUID2OBJECT[key] = mapping;
                if (content.Length > 1)
                {
                    mapping.NewName = content[1];
                }
                else
                {
                    mapping.NewName = "MISSINGPERSON";
                }
                if (IsLocalScene)
                {
                    LocalScene.Users.Add(mapping);
                }
            }
            foreach (string file in Directory.GetFiles(ExportCommand.siminfoDir, "*.group"))
            {
                string[] content = File.ReadAllText(file).Split(',');
                UUID key = UUID.Parse(Path.GetFileNameWithoutExtension(file));
                UserOrGroupMapping mapping = new UserOrGroupMapping(key, content[0], true);
                UUID2OBJECT[key] = mapping;
                if (content.Length > 1)
                {
                    mapping.NewName = content[1];
                }
                else
                {
                    mapping.NewName = content[0];
                }
                if (IsLocalScene)
                {
                    LocalScene.Groups.Add(mapping);
                }
            }
        }
    }
}
