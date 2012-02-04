using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;
using OpenMetaverse.Packets;
using Radegast;
using Radegast.Netcom;
using System.Windows.Forms;

namespace cogbot.Actions.Land
{
    /// <summary>
    /// Display a list of all agent locations in a specified region
    /// </summary>
    public class GridHealthCommand : Command, GridMasterCommand
    {
        bool registeredPackHandler = false;
        bool needGridRequest = true;
        public GridHealthCommand(BotClient testClient)
        {
            Name = "gridhealth";
            Description = "Runs a TP check to make sure ALL sims are useable on the grid. Usage: gridhealth [regionhandle]";
            Category = CommandCategory.Simulator;
            Parameters = new [] { new NamedParam(typeof(SimRegion), typeof(ulong)) };
            RegisterGridHandler();
            TheBotClient.WorldSystem.OnConnectedQueue.Enqueue("gridhealth", MakeGridRequest);
        }

        private void MakeGridRequest()
        {
            RegisterGridHandler();
            if (!needGridRequest) return;
            needGridRequest = false;
            Client.Grid.RequestMapBlocks(GridLayerType.Objects, 0, 0, 65535, 65535, false);
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            MakeGridRequest();
            var regions = LockInfo.CopyOf(Regions);
            Success("Testing " + regions.Count);
            int passes = 0, failures = 0, tested = 0;
            var skipRegions = new[] { "sttc_0013" };
            foreach (var r in regions.Values)
            {
                Success("Region: " + r.Name + ": (" + r.X + "," + r.Y + ") " + r.Access);
                Client.Grid.RequestMapRegion(r.Name, GridLayerType.Terrain);
                DownloadRegionImage(r);
                continue;
                bool skipRegion = false;
                string rn = r.Name;
                foreach (var named in skipRegions)
                {
                    if (rn.Contains(named))
                    {
                        skipRegion = true;
                        break;
                    }
                }
                if (skipRegion)
                {
                    continue;
                }
                Simulator sim = Client.Network.CurrentSim;
                if (r.X == 0 || r.Y == 0 || (sim != null && sim.Name.ToLower() == r.Name.ToLower()))
                {
                    Success("Skipping: " + r.Name + ": (" + r.X + "," + r.Y + ") " + r.Access);
                    continue;
                }
                tested++;
                if (RegionTest(r))
                {
                    Success("PASS: " + r.Name + ": (" + r.X + "," + r.Y + ") " + r.Access);
                    passes++;
                }
                else
                {
                    Success("FAIL: " + r.Name + ": (" + r.X + "," + r.Y + ") " + r.Access);
                    failures++;
                   // break;
                }
            }
            return
                Success("Found passes=" + passes + " falures=" + failures + " on " + tested + "/" + regions.Count +
                        " total");
        }
        private bool RegionTest(GridRegion r)
        {
            Control.CheckForIllegalCrossThreadCalls = false;
            Success("Testing: " + r.Name + ": (" + r.X + "," + r.Y + ") " + r.Access);
            //return TestViaLogin(r);
            bool t1 =  MiniMapTest(r);
            Success("MINIMAP " + (t1 ? "PASS" : "FAIL") + ": " + r.Name + ": (" + r.X + "," + r.Y + ") " + r.Access);
            return TeleportTest(r);
        }

        private bool TestViaLogin(GridRegion r)
        {
            var TheRadegastInstance = Client.TheRadegastInstance;
            Success("Logging out to Test " + r.Name);
            TheRadegastInstance.Netcom.Logout();
            Client.logout();
            Client.Network.Logout();
            var loginParams = TheBotClient.TheRadegastInstance.Netcom.LoginOptions;
            string uri = r.Name + "/128/128/128";
            TheBotClient.BotLoginParams.loginParams.Start = uri;
            loginParams.StartLocationCustom = r.Name;
            loginParams.StartLocation = StartLocationType.Custom;
            TheBotClient.BotLoginParams.Start = uri;
            TheRadegastInstance.Netcom.loginOptions.StartLocationCustom = r.Name;
            TheRadegastInstance.Netcom.loginOptions.StartLocation = StartLocationType.Custom;
            TheBotClient.ExpectConnected = false;
            Thread.Sleep(6000);
            if (TheBotClient.IsLoggedInAndReady)
            {
                Success("Error  IsLoggedInAndReady");
            }
            Client.Network.CurrentSim = null;
            Success("NOW Logging into " + r.Name);
            if (!ClientManager.OneAtATimeQueue.NoExceptionsV(TheRadegastInstance.Netcom.Login))
            {
                TheBotClient.Login(true);
            }
            else
            {
                Thread.Sleep(10000);
            }
            Thread.Sleep(5000);
            //TheRadegastInstance.Reconnect(); 
                       
            Simulator clientNetworkCurrentSim = Client.Network.CurrentSim;
            return clientNetworkCurrentSim != null && clientNetworkCurrentSim.Name.ToLower() == r.Name.ToLower();
            Client.Settings.USE_LLSD_LOGIN = false;
            Client.Settings.LOGIN_TIMEOUT = 60000;
            return Client.Network.Login(loginParams.FirstName, loginParams.LastName, loginParams.Password, "Radegast", r.Name, "1.0");
        }

        private bool MiniMapTest(GridRegion r)
        {
            var itemList = Client.Grid.MapItems(r.RegionHandle, OpenMetaverse.GridItemType.AgentLocations, GridLayerType.Objects, 2000);
            if (itemList==null)
            {
                return false;
            }                 
            return itemList != null;
        }

        private bool TeleportTest(GridRegion r)
        {
            bool passfail = false;
            AutoResetEvent are = new AutoResetEvent(false);
            bool eventDead = false;
            EventHandler<TeleportEventArgs> tp = (o, e) =>
                                                                   {
                                                                       if (eventDead) return;
                                                                       switch(e.Status)
                                                                       {
                                                                           case TeleportStatus.None:
                                                                           case TeleportStatus.Start:
                                                                           case TeleportStatus.Progress:
                                                                               Success("status " + e.Status);
                                                                               break;
                                                                           case TeleportStatus.Failed:
                                                                               passfail = false;
                                                                               are.Set();
                                                                               break;
                                                                           case TeleportStatus.Finished:
                                                                               passfail = true;
                                                                               are.Set();
                                                                               break;
                                                                           case TeleportStatus.Cancelled:
                                                                               passfail = false;
                                                                               are.Set();
                                                                               break;
                                                                           default:
                                                                               throw new ArgumentOutOfRangeException();
                                                                       }
                                                                   };
            try
            {
                Client.Self.TeleportProgress += tp;

                bool pass = Client.Self.Teleport(r.RegionHandle, new Vector3(128, 128, 128));
                eventDead = true;
                int ms = Client.Settings.TELEPORT_TIMEOUT;
                if (!are.WaitOne(ms + 1000))
                {
                    Success("TimeOut: " + r.Name + ": (" + r.X + "," + r.Y + ") " + r.Access);
                    return false;
                }
            }
            finally
            {
                eventDead = true;
                Client.Self.TeleportProgress -= tp;
            }
            Thread.Sleep(2000);
            Simulator clientNetworkCurrentSim = Client.Network.CurrentSim;
            return clientNetworkCurrentSim != null && clientNetworkCurrentSim.Name.ToLower() == r.Name.ToLower();
            //return passfail;
        }

        private void RegisterGridHandler()
        {
            if (!registeredPackHandler)
            {
                registeredPackHandler = true;
                Client.Network.RegisterCallback(PacketType.MapBlockReply, MapBlockReplyHandler);
            }
        }

        /// <summary>A dictionary of all the regions, indexed by region name</summary>
        internal Dictionary<string, GridRegion> Regions = new Dictionary<string, GridRegion>();
        /// <summary>A dictionary of all the regions, indexed by region handle</summary>
        internal Dictionary<ulong, GridRegion> RegionsByHandle = new Dictionary<ulong, GridRegion>();

        protected void MapBlockReplyHandler(object sender, PacketReceivedEventArgs e)
        {
            MapBlockReplyPacket map = (MapBlockReplyPacket)e.Packet;

            foreach (MapBlockReplyPacket.DataBlock block in map.Data)
            {
                if (block.X != 0 || block.Y != 0)
                {
                    GridRegion region = new GridRegion();

                    region.X = block.X;
                    region.Y = block.Y;
                    region.Name = Utils.BytesToString(block.Name);
                    // RegionFlags seems to always be zero here?
                    region.RegionFlags = (RegionFlags)block.RegionFlags;
                    region.WaterHeight = block.WaterHeight;
                    region.Agents = block.Agents;
                    region.Access = (SimAccess)block.Access;
                    region.MapImageID = block.MapImageID;
                    region.RegionHandle = Utils.UIntsToLong((uint)(region.X * 256), (uint)(region.Y * 256));

                    lock (Regions)
                    {
                        Regions[region.Name] = region;
                        RegionsByHandle[region.RegionHandle] = region;
                    }
                }
            }
        }

        private void DownloadRegionImage(GridRegion region)
        {
            string filename = region.Name + "_" + region.X + "_" + region.Y + ".tga";
            Client.ExecuteCommand("download " + region.MapImageID + " " + AssetType.Texture + " " + filename + " jp2k", Client, WriteLine);
        }
    }
}
