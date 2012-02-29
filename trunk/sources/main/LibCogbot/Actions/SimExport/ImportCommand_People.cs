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
            public bool IsGroup = false;
            public string OldName;
            public string NewName;
            public string ErrorMessage;
            private ManualResetEvent WaitOnCreate;

            public UserOrGroupMapping(UUID id, String name, bool isGroup)
            {
                OldID = id;
                UUID2OBJECT[id] = this;
                IsGroup = isGroup;
                OldName = name;
                NewName = name;
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

            private UUID FindGroup(string name)
            {
                if (!IsGroup) return FindUser(name);
                if (!CogbotHelpers.IsNullOrZero(base.NewID)) return base.NewID;
                WaitOnCreate = WaitOnCreate ?? new ManualResetEvent(false);
                WaitOnCreate.Reset();
                Running.Client.Directory.DirGroupsReply += GroupSearchReply;
                Running.Client.Directory.StartGroupSearch(name, 0, OldID);
                WaitOnCreate.WaitOne(3000);
                return base.NewID;
            }

            private UUID FindUser(string name)
            {
                if (IsGroup) return FindGroup(name);
                if (!CogbotHelpers.IsNullOrZero(base.NewID)) return base.NewID;
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
                Running.Client.Groups.GroupCreatedReply += GroupCreateReply;
                Running.Client.Groups.RequestCreateGroup(newGroup);
                WaitOnCreate.WaitOne(5000);
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

        public void LoadUsersAndGroups()
        {
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
            }
        }
    }
}
