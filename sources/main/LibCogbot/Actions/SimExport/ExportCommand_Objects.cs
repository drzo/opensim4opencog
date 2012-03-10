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
    public class LinkSetBuffer
    {
        public string S;
        public string F;
        public SimObject O;
        public override string ToString()
        {
            return ExportCommand.named(O) + ":" + S;
        }
    }

    public partial class ExportCommand : Command, RegionMasterCommand
    {
        
        public readonly Dictionary<UUID, LinkSetBuffer> PrimWaitingLinkset = new Dictionary<UUID, LinkSetBuffer>();

        void ScanForLinksets(SimObject O)
        {
            if (O.Children == null) return;
            if (O.Children.Count == 0)
            {
                lock (fileWriterLock) File.WriteAllText(dumpDir + O.ID + ".link", "");
            }

            var lst = new SortedList<uint, UUID>();
            foreach (var o in LockInfo.CopyOf(O.Children))
            {
                lst.Add(o.LocalID, o.ID);
            }
            string contents = "" + O.ID;
            foreach (KeyValuePair<uint, UUID> uuid in lst)
            {
                contents += "," + uuid.Value;
            }
            lock (fileWriterLock) File.WriteAllText(dumpDir + O.ID + ".link", contents);
        }

        public static bool IsSkipped(SimObject P)
        {
            if (P is SimAvatar) return true;
            if (P == null) return true;
            if (P.IsKilled) return true;
            if (!P.HasPrim) return true;
            var pp = P.Prim;
            if (pp == null) return true;
            SimObject parent = P.Parent;
            if (parent is SimAvatar) return true;
            if (pp.ParentID == 0) return false;
            // yes SL really does have links two deep! (called attachment linksets)
            if (parent != null && parent.Parent is SimAvatar)
            {
                return true;
            }
            return false;
        }
        public void ExportPrim(BotClient Client, SimObject exportPrim, OutputDelegate Failure, ImportSettings arglist)
        {
            uint localID = exportPrim.LocalID;
            Client.Objects.SelectObject(arglist.CurSim, localID);
            ExportPrim0(Client, exportPrim, Failure, arglist);
            Client.Objects.DeselectObject(arglist.CurSim, localID);
        }
        public void ExportPrim0(BotClient Client, SimObject exportPrim, OutputDelegate Failure, ImportSettings arglist)
        {
            if (IsSkipped(exportPrim)) return;
            Simulator CurSim = exportPrim.GetSimulator();
            WorldObjects.EnsureSelected(exportPrim.LocalID, CurSim);
            string pathStem = Path.Combine(dumpDir, exportPrim.ID.ToString());
            if (arglist.Contains("task") || showsMissingOnly)
            {
                exportPrim.StartGetTaskInventory();
            }
            if (arglist.Contains("wait"))
            {
                var waitUntil = DateTime.Now.AddSeconds(10);
                bool needsLoop = true;
                while (needsLoop && waitUntil > DateTime.Now)
                {
                    needsLoop = false;
                    if (exportPrim.Properties == null || CogbotHelpers.IsNullOrZero(exportPrim.Properties.OwnerID))
                    {
                        needsLoop = true;
                    }
                    if (arglist.Contains("task"))
                    {
                        var ti = exportPrim.TaskInventory;
                        if (ti == null)
                        {
                            needsLoop = true;
                        }
                    }
                }
                if (needsLoop)
                {
                    Success("needs loop " + named(exportPrim));
                    string bissues = exportPrim.MissingData;
                    if (!string.IsNullOrEmpty(bissues))
                    {
                        Failure("Cant wait out the Issues " + bissues + ": " + named(exportPrim));
                        if (LocalFailures == 0) LocalFailures++;
                        return;
                    }
                }
            }
            string issues = exportPrim.MissingData;
            if (!string.IsNullOrEmpty(issues))
            {
                Failure("Issues " + issues + " " + named(exportPrim));
                if (LocalFailures == 0) LocalFailures++;
                return;
            }
            if (arglist.Contains("llsd")) SaveLLSD(Client, pathStem, exportPrim, Failure);
            if (exportPrim.IsRoot && (true || exportPrim.Children.Count > 0))
            {
                if (arglist.Contains("link")) SaveLinksetInfo(Client, pathStem, exportPrim, Failure);
                string exportFile = pathStem + ".link";
                //lock (fileWriterLock) if (File.Exists(exportFile))
                {
                    foreach (var c in exportPrim.Children)
                    {
                        ExportPrim(Client, c, Failure, arglist);
                    }
                }
            }
            if (arglist.Contains("task")) SaveTaskInv(arglist, Client, pathStem, exportPrim, Failure);
            if (!arglist.Contains("dep")) return;
            AddRelatedTextures(exportPrim);
            SaveRelatedAssets(pathStem, exportPrim, Failure);
        }

        void SaveLinksetInfo(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".link";
            if (Incremental || true) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            if (false && exportPrim.Children.Count == 0)
            {
                // so we dont do it again
                if (Incremental) lock (fileWriterLock) File.WriteAllText(exportFile, "");
                return;
            }
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED LINK for " + named(exportPrim));
                return;
            }
            RequestLinksetInfo(Client, pathStem, exportPrim, Failure);
        }

        void RequestLinksetInfo(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            bool canScript = checkPerms(Client, exportPrim, SilientFailure, true);
            InventoryItem found = GetInvItem(Client, "LinksetSpeaker");
            if (!canScript || found == null)
            {
                ScanForLinksets(exportPrim);
                return;
            }
            lock (PrimWaitingLinkset)
            {
                if (PrimWaitingLinkset.ContainsKey(exportPrim.ID)) return;
                PrimWaitingLinkset.Add(exportPrim.ID, new LinkSetBuffer {S = "", O = exportPrim, F = pathStem});
            }
            PutItemToTaskInv(Client, exportPrim, "LinksetSpeaker");
        }

        private void listen_forLinkset(string eMessage, UUID sourceId)
        {
            if (eMessage.StartsWith("INV-"))
            {
                return;
            }
            if (eMessage.StartsWith("MYKEY"))
            {
                return;
            }
            if (eMessage.Contains(":"))
            {
                return;
            }
            lock (PrimWaitingLinkset)
            {
                LinkSetBuffer linkSetBuffer;
                if (!PrimWaitingLinkset.TryGetValue(sourceId, out linkSetBuffer))
                {
                    linkSetBuffer = new LinkSetBuffer
                                        {S = "", O = GetSimObjectFromUUID(sourceId), F = Path.Combine(dumpDir, sourceId.ToString())};
                    PrimWaitingLinkset.Add(sourceId, linkSetBuffer);
                }
                if (linkSetBuffer.S == "")
                {
                    if (!eMessage.StartsWith("Y,") || eMessage.StartsWith("\u2127"))
                    {
                        lock (PrimWaitingLinkset)
                        {
                            PrimWaitingLinkset.Remove(sourceId);
                        }
                        return;
                        throw new InvalidOperationException("wrong message came first " + linkSetBuffer + " was " + eMessage);
                    }
                }
                else
                {
                    if (eMessage.StartsWith("Y,"))
                    {
                        lock (PrimWaitingLinkset)
                        {
                            PrimWaitingLinkset.Remove(sourceId);
                        }
                        return;
                        throw new InvalidOperationException("new message came to " + linkSetBuffer + " was " + eMessage);
                    }
                }
                string soS = linkSetBuffer.S;
                if (string.IsNullOrEmpty(soS))
                {
                    linkSetBuffer.S = eMessage;
                }
                else
                {
                    linkSetBuffer.S = soS + "," + eMessage;
                }
                if (eMessage.EndsWith(",Z"))
                {

                    var mustHave = linkSetBuffer.S.Substring(2);
                    if (mustHave.StartsWith("1,"))
                    {
                        lock (fileWriterLock) File.WriteAllText(linkSetBuffer.F + ".link", "");
                        return;
                    }
                    // get past count
                    int fc = mustHave.IndexOf(',');
                    mustHave = mustHave.Substring(fc + 1);
                    // remove off ,Z
                    mustHave = mustHave.Substring(0, mustHave.Length - 2);
                    var childs = GetUUIDs(mustHave);
                    foreach (UUID list in childs)
                    {
                        if (GetSimObjectFromUUID(list) == null)
                        {
                            throw new InvalidOperationException("new message came to " + linkSetBuffer + " was " + eMessage);
                        }
                    }
                    lock (fileWriterLock) File.WriteAllText(linkSetBuffer.F + ".link", mustHave);
                    lock (PrimWaitingLinkset)
                    {
                        PrimWaitingLinkset.Remove(sourceId);
                    }
                }
            }
        }
        static internal SimObject GetSimObjectFromUUID(UUID objid)
        {
            DateTime timeOut = DateTime.Now + TimeSpan.FromSeconds(5);
            while (DateTime.Now < timeOut)
            {
                var O = WorldObjects.GetSimObjectFromUUID(objid);
                if (O != null) return O;
                Thread.Sleep(500);
            }
            return null;
        }


        public static bool checkPerms(BotClient Client, SimObject exportPrim, OutputDelegate Failure, bool mustModify)
        {
            if (exportPrim != null)
            {

                var Properties = exportPrim.Properties;
                if (Properties == null)
                {
                    Client.Objects.RequestObjectPropertiesFamily(exportPrim.GetSimulator(), exportPrim.ID, true);
                    Failure("No props yet for " + named(exportPrim));
                    return false;
                }
                // Check for export permission first
                //GotPermissions = false;
                //
                //if (!GotPermissions)
                // {
                //   Properties = exportPrim.Properties ?? new Primitive.ObjectProperties();
                //}
                //   GotPermissionsEvent.WaitOne(1000 * 10, false);
                if (Properties.OwnerID != Client.Self.AgentID &&
                    Properties.OwnerID != Client.MasterKey &&
                    Properties.GroupID != Client.Self.ActiveGroup)
                {
                    Failure("That object is owned by " + Properties.OwnerID + ", we don't have permission " +
                            "to export " + named(exportPrim));
                }

                SimAvatarClient theAvatar = Client.TheSimAvatar;
                PermissionWho pw = theAvatar.EffectivePermissionWho(exportPrim);
                PermissionMask pm = theAvatar.EffectivePermissionsMask(exportPrim);

                bool modify = Permissions.HasPermissions(pm, PermissionMask.Modify);

                bool cmt = Permissions.HasPermissions(pm, PermissionMask.Copy) ||
                           Permissions.HasPermissions(pm, PermissionMask.Modify) ||
                           Permissions.HasPermissions(pm, PermissionMask.Transfer);

                if (mustModify)
                {
                    if (!modify)
                    {
                        Failure("ObjPerms NOMODIFY " + pm + " for " + pw + " on " + named(exportPrim));
                        return false;
                    }
                }
                if (!cmt)
                {
                    Failure("ObjPerms " + pm + " for " + pw + " on " + named(exportPrim));
                    return false;
                }

                //List<SimObject> family = new List<SimObject>();
                //family.Add(exportPrim);
                //family.AddRange(exportPrim.Children);

                /*bool complete = RequestObjectProperties(family, 250, exportPrim.GetSimulator());
                exportedPrims.AddRange(family);

                if (!complete)
                {
                    Logger.Log("Warning: Unable to retrieve full properties for:", Helpers.LogLevel.Warning, Client);
                    foreach (UUID uuid in PrimsWaiting.Keys)
                        Logger.Log(uuid.ToString(), Helpers.LogLevel.Warning, Client);
                }
                 * return true;*/
            }
            return true;
        }

        public void SaveLLSD(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            if (exportPrim != null)
            {
                string exportFile = pathStem + ".llsd";
                if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
                needFiles++;
                if (showsMissingOnly)
                {
                    Failure("NEED LLSD for " + named(exportPrim));
                    return;
                }

                try
                {
                    List<string> skipTag = new List<string>() { "Tag" };
                    Primitive prim = exportPrim.Prim;

                    Vector3 pp = prim.Position;
                    Quaternion pr = prim.Rotation;
                    //prim = prim.Clone(); 
                    OSDMap primOSD = prim.GetTotalOSD();
                    if (prim.ParentID != 0)
                    {
                        var parent = WorldSystem.GetLibOMVHostedPrim(prim.ParentID, CurSim, false);
                        if (parent == null)
                        {
                            pp += new Vector3(128, 128, Client.Self.SimPosition.Z + 20);
                            Failure("YET FAILED: Cant GET parent of " + prim);
                            return;
                        }
                        else
                        {
                            pp = prim.Position * Matrix4.CreateFromQuaternion(parent.Rotation) + parent.Position;
                            pr = parent.Rotation * pr;
                            primOSD["ParentUUID"] = parent.ID;
                        }
                    }
                    primOSD["RegionPosition"] = pp;
                    primOSD["RegionRotation"] = pr;
                    AddExportUser(primOSD["CreatorID"]);
                    AddExportGroup(primOSD["GroupID"]);
                    AddExportUser(primOSD["OwnerID"]);
                    AddExportUser(primOSD["LastOwnerID"]);
                    string output = OSDParser.SerializeLLSDXmlString(primOSD);
                    {
                        lock (fileWriterLock) File.WriteAllText(exportFile, output);
                    }
                    if (forced && !verbosely) return;
                    return;
                    Primitive prim2 = FromFile(exportFile, ExportCommand.UseBinarySerialization) as Primitive;
                    string memberwiseCompare = MemberwiseCompare(prim, prim2, skipTag);
                    if (!string.IsNullOrEmpty(memberwiseCompare))
                    {
                        string failre = "Error in LLSD: " + memberwiseCompare;
                        Failure(failre);
                        if (!forced)
                        {
                            File.Delete(exportFile);
                            return;
                            Error(failre);
                        }
                    }
                }
                catch (Exception e)
                {
                    File.Delete(exportFile);
                    Failure("Writing file " + exportFile + " caused " + e);
                }
            }
        }

        public static string named(SimObject prim)
        {
            string s = ("" + prim);
            int start = s.IndexOf("localID");
            int fp = s.IndexOf(")", start + 1);
            //if (fp < 64) fp = 0;
            if (fp > 0) return s.Substring(0, fp + 1);
            if (s.Length < 100) return s;
            return s.Substring(0, 100);

        }
    }
}
