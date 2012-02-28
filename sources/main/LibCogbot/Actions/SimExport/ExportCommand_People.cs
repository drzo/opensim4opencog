using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using OpenMetaverse.Assets;

using MushDLR223.ScriptEngines;
using PathSystem3D.Mesher;

namespace cogbot.Actions.SimExport
{
    public partial class ExportCommand : Command, RegionMasterCommand
    {

        private readonly HashSet<UUID> ExportUsers = new HashSet<UUID>();
        private readonly HashSet<UUID> ExportGroup = new HashSet<UUID>();

        private void RequestUsersAndGroups()
        {
            Client.Avatars.RequestAvatarNames(new List<UUID>(ExportUsers));
            Client.Groups.RequestGroupNames(new List<UUID>(ExportGroup));
        }

        private void GroupNames(object sender, GroupNamesEventArgs e)
        {
            if (!IsExporting) return;
            foreach (KeyValuePair<UUID, string> name in e.GroupNames)
            {
                lock (fileWriterLock) File.WriteAllText(siminfoDir + "" + name.Key + ".group", name.Value);
            }
        }
        private void UserNames(object sender, UUIDNameReplyEventArgs e)
        {
            if (!IsExporting) return;
            foreach (KeyValuePair<UUID, string> name in e.Names)
            {
                lock (fileWriterLock) File.WriteAllText(siminfoDir + "" + name.Key + ".avatar", name.Value);
            }
        }


        private void SnagUsers(SimObject o)
        {
            var prim = o.Prim;
            AddExportUser(prim.OwnerID);
            AddExportGroup(prim.GroupID);
            Primitive.ObjectProperties primProperties = prim.Properties ?? o.Properties;
            if (primProperties != null)
            {
                AddExportUser(primProperties.CreatorID);
                AddExportGroup(primProperties.GroupID);
            }
        }

        private void AddExportGroup(UUID groupID)
        {
            if (CogbotHelpers.IsNullOrZero(groupID)) return;
            ExportGroup.Add(groupID);
        }

        private void AddExportUser(UUID userID)
        {
            if (CogbotHelpers.IsNullOrZero(userID)) return;
            ExportUsers.Add(userID);
        }

        private OSDMap OSDSerializeMembers(object inv)
        {
            var osd = new OSDMap();
            OSD.AddObjectOSD(inv, osd, inv.GetType(), false);
            AddExportUser(osd["CreatorID"]);
            AddExportGroup(osd["GroupID"]);
            AddExportUser(osd["OwnerID"]);
            AddExportUser(osd["LastOwnerID"]);
            return osd;
        }
    }
}
