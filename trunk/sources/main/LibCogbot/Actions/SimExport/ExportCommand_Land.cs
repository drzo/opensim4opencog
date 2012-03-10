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
        private bool DownloadingTerrain = false;
        /// <summary>
        /// Create a Synchronization event object
        /// </summary>
        private readonly AutoResetEvent terrainXferTimeout = new AutoResetEvent(false);
        static public string terrainFileName = terrainDir + "terrain.raw";
        private readonly AutoResetEvent ParcelsDownloaded = new AutoResetEvent(false);
        private void SaveParcelInfo()
        {
            Client.Parcels.RequestAllSimParcels(CurSim);

            if (CurSim.IsParcelMapFull())
                ParcelsDownloaded.Set();

            if (ParcelsDownloaded.WaitOne(30000, false) && Client.Network.Connected)
            {
                Success(string.Format("Downloaded {0} Parcels in {1} " + Environment.NewLine, CurSim.Parcels.Count, CurSim.Name));
                IDictionary<int, Parcel> curSimParcels = null;
                lock (CurSim.Parcels.Dictionary)
                {
                    curSimParcels = LockInfo.CopyOf(CurSim.Parcels.Dictionary);
                }

                List<UUID> ParcelPrimOwners = new List<UUID>();
                List<uint> ParcelObjectIDS = new List<uint>();
                foreach (KeyValuePair<int, Parcel> simParcel in curSimParcels)
                {
                    var parcel = simParcel.Value;
                    int parcelID = parcel.LocalID;

                    Success(string.Format(
                                "Parcel[{0}]: Name: \"{1}\", Description: \"{2}\" ACLBlacklist Count: {3}, ACLWhiteList Count: {5} Traffic: {4}" +
                                Environment.NewLine,
                                parcel.LocalID, parcel.Name, parcel.Desc,
                                parcel.AccessBlackList.Count, parcel.Dwell,
                                parcel.AccessWhiteList.Count));
                    AddExportUser(parcel.OwnerID);
                    AddExportGroup(parcel.GroupID);

                    Success(Helpers.StructToString(parcel));
                    foreach (
                        ParcelManager.ParcelAccessEntry white in parcel.AccessWhiteList)
                    {
                        if (white.AgentID != UUID.Zero)
                            Success(string.Format(
                                        "\tAllowed Avatar {0}" + Environment.NewLine,
                                        white.AgentID));
                    }
                    foreach (
                        ParcelManager.ParcelAccessEntry black in parcel.AccessBlackList)
                    {
                        //    if(black.AgentID != UUID.Zero)
                        Success(string.Format("\t Banned Avatar {0}" + Environment.NewLine,
                                              black.AgentID));
                    }
                    {
                        AutoResetEvent parcelOwnerWait = new AutoResetEvent(false);

                        EventHandler<ParcelObjectOwnersReplyEventArgs> callback =
                            delegate(object sender, ParcelObjectOwnersReplyEventArgs e)
                            {
                                for (int i = 0; i < e.PrimOwners.Count; i++)
                                {
                                    ParcelManager.ParcelPrimOwners ownerse = e.PrimOwners[i];
                                    if (ownerse.IsGroupOwned)
                                    {
                                        AddExportGroup(ownerse.OwnerID);
                                    }
                                    else
                                    {
                                        AddExportUser(ownerse.OwnerID);
                                    }
                                    Success(string.Format("Owner: {0} Count: {1}" + Environment.NewLine,
                                                          ownerse.OwnerID, ownerse.Count));
                                    if (!CogbotHelpers.IsNullOrZero(ownerse.OwnerID))
                                    {
                                        ParcelPrimOwners.Add(ownerse.OwnerID);
                                    }
                                }
                                parcelOwnerWait.Set();
                            };

                        Client.Parcels.ParcelObjectOwnersReply += callback;
                        try
                        {
                            Client.Parcels.RequestObjectOwners(CurSim, parcelID);
                            if (!parcelOwnerWait.WaitOne(10000, false))
                            {
                                Failure("Timed out waiting for packet.");
                            }

                        }
                        finally
                        {
                            Client.Parcels.ParcelObjectOwnersReply -= callback;
                        }
                    }
                    foreach (UUID ownerUUID in ParcelPrimOwners)
                    {
                        AutoResetEvent wait = new AutoResetEvent(false);
                        EventHandler<ForceSelectObjectsReplyEventArgs> callback = delegate(object sender, ForceSelectObjectsReplyEventArgs e)
                        {
                            ParcelObjectIDS.AddRange(e.ObjectIDs);

                            for (int i = 0; i < e.ObjectIDs.Count; i++)
                            {
                                // Success(string.Format(e.ObjectIDs[i].ToString() + " "));
                                // counter++;
                            }

                            if (e.ObjectIDs.Count < 251)
                                wait.Set();
                        };


                        Client.Parcels.ForceSelectObjectsReply += callback;
                        Client.Parcels.RequestSelectObjects(parcelID, (ObjectReturnType)16, ownerUUID);
                        wait.WaitOne(10000);
                    }
                }
                foreach (uint u in ParcelObjectIDS)
                {
                    RequiredForExportLocalIDs.Add(u);
                }
                Success("Parcel LocalIDs=" + ParcelObjectIDS.Count + " ParcelOwners=" + ParcelPrimOwners.Count);
            }
            else
                Failure("Failed to retrieve information on all the simulator parcels");
        }

        public void SaveTerrainHeight()
        {
            bool prefixFP = false;
            OSDMap simInfoMap = new OSDMap();
            // leave these out of serialization
            simInfoMap["ObjectsPrimitives"] = true;
            simInfoMap["ObjectsAvatars"] = true;
            simInfoMap["Client"] = true;
            simInfoMap["SharedData"] = true;
            simInfoMap["Caps"] = true;
            simInfoMap["DeadObjects"] = true;
            simInfoMap["KilledObjects"] = true;
            var exceptFor = new HashSet<object>() { typeof(IList), typeof(IDictionary), typeof(object) };
            OSD.AddObjectOSD0(CurSim.Stats, simInfoMap, typeof(Simulator.SimStats), exceptFor, true, prefixFP);
            OSD.AddObjectOSD0(CurSim.SharedData, simInfoMap, typeof(Simulator.SimPooledData), exceptFor, true, prefixFP);
            OSD.AddObjectOSD0(CurSim, simInfoMap, typeof(Simulator), exceptFor, true, prefixFP);
            string output = OSDParser.SerializeLLSDXmlString(simInfoMap);
            {
                lock (fileWriterLock) File.WriteAllText(terrainDir + "simInfoMap.llsd", output);
            }
            AddRelated(CurSim.TerrainBase0, AssetType.Texture);
            AddRelated(CurSim.TerrainBase1, AssetType.Texture);
            AddRelated(CurSim.TerrainBase2, AssetType.Texture);
            AddRelated(CurSim.TerrainBase3, AssetType.Texture);
            AddRelated(CurSim.TerrainDetail0, AssetType.Texture);
            AddRelated(CurSim.TerrainDetail1, AssetType.Texture);
            AddRelated(CurSim.TerrainDetail2, AssetType.Texture);
            AddRelated(CurSim.TerrainDetail3, AssetType.Texture);
            AddExportUser(CurSim.SimOwner);
            var Terrain = CurSim.Terrain;
            if (Terrain == null)
            {
                Failure("Terrain missing completely");
                return;
            }
            Client.Grid.RequestMapItems(CurSim.Handle, GridItemType.LandForSale, GridLayerType.Terrain);
            var number = CurSim.Terrain.Length;
            var missing = 0;
            foreach (TerrainPatch patch in Terrain)
            {
                if (patch != null && patch.Data != null && patch.Data.Length > 0)
                {
                    continue;
                }
                missing++;
            }
            var terrainReady = (missing == 0);
            if (!terrainReady)
            {
                Failure("Terrain missing chunks/total=" + missing + "/" + number);
            }
            if (terrainReady || forced)
            {
                SaveToDisk(terrainDir + "terrain.patches", Terrain);
                SaveTerrainRaw32(dumpDir + "../terrains/heightmap.r32");
                if (!terrainReady)
                {
                    Failure("SaveTerrainHeight Saved but not ready");
                }
                else
                {
                    Success("SaveTerrainHeight Success");
                }
                return;
            }
            Failure("Unable to SaveTerrainHeight (use --force)");
        }
        private void DownloadTerrain()
        {
            if (DownloadingTerrain) return;
            DownloadingTerrain = true;
            EventHandler<InitiateDownloadEventArgs> initiateDownloadDelegate =
                delegate(object sender, InitiateDownloadEventArgs e)
                {
                    Client.Assets.RequestAssetXfer(e.SimFileName, false, false, UUID.Zero, AssetType.Unknown, false);
                };

            var timeout = TimeSpan.FromMinutes(2);

            // Subscribe to the event that will tell us the status of the download
            Client.Assets.XferReceived += Terrain_XferReceived;
            // subscribe to the event which tells us when the simulator has received our request
            Client.Assets.InitiateDownload += initiateDownloadDelegate;

            // configure request to tell the simulator to send us the file
            List<string> parameters = new List<string>();
            parameters.Add("download filename");
            parameters.Add(terrainFileName);
            // send the request
            Client.Estate.EstateOwnerMessage("terrain", parameters);

            // wait for (timeout) seconds for the request to complete (defaults 2 minutes)
            if (!terrainXferTimeout.WaitOne(timeout, false))
            {
                Failure("Timeout while waiting for terrain data");
            }

            // unsubscribe from events
            Client.Assets.InitiateDownload -= initiateDownloadDelegate;
            Client.Assets.XferReceived -= Terrain_XferReceived;
        }

        private void Terrain_XferReceived(object sender, XferReceivedEventArgs e)
        {
            if (e.Xfer.Filename != terrainFileName) return;
            if (e.Xfer.Success)
            {
                // set the result message
                Success(string.Format("Terrain file {0} ({1} bytes) downloaded successfully, written to {2}",
                                      e.Xfer.Filename, e.Xfer.Size, terrainFileName));

                // write the file to disk
                FileStream stream = new FileStream(terrainFileName, FileMode.Create);
                BinaryWriter w = new BinaryWriter(stream);
                w.Write(e.Xfer.AssetData);
                w.Close();

                // tell the application we've gotten the file
                terrainXferTimeout.Set();
            }
        }
        public void SaveTerrainRaw32(string path)
        {
            var patches = Client.Network.CurrentSim.SharedData.Terrain;
            if (patches != null)
            {
                int count = 0;
                for (int i = 0; i < patches.Length; i++)
                {
                    if (patches[i] != null)
                        ++count;
                }

                Logger.Log(count + " terrain patches have been received for the current simulator", Helpers.LogLevel.Info);
            }
            else
            {
                Logger.Log("No terrain information received for the current simulator", Helpers.LogLevel.Info);
                return;
            }
            try
            {
                using (
                    FileStream stream = new FileStream(path, FileMode.Create,
                                                       FileAccess.Write))
                {
                    for (int y = 0; y < 256; y++)
                    {
                        for (int x = 0; x < 256; x++)
                        {
                            int xBlock = x / 16;
                            int yBlock = y / 16;
                            int xOff = x - (xBlock * 16);
                            int yOff = y - (yBlock * 16);

                            TerrainPatch patch = patches[yBlock * 16 + xBlock];
                            float t = 0f;

                            if (patch != null)
                                t = patch.Data[yOff * 16 + xOff];
                            else
                                Logger.Log(String.Format("Skipping missing patch at {0},{1}", xBlock, yBlock),
                                           Helpers.LogLevel.Warning);

                            stream.Write(BitConverter.GetBytes(t), 0, 4);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.Log("Failed saving terrain: " + ex.Message, Helpers.LogLevel.Error);
            }
        }

    }
}
