using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class ParcelInfoCommand : Command, RegionMasterCommand
    {
        private AutoResetEvent ParcelsDownloaded = new AutoResetEvent(false);

        NetworkManager.DisconnectedCallback callback;

        public ParcelInfoCommand(BotClient testClient)
        {
            Name = "parcelinfo";
            Description = "Prints out info about all the parcels in this simulator";
            Category = CommandCategory.Parcel;

            callback = new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            StringBuilder sb = new StringBuilder();
            string result;
            Client.Network.OnDisconnected += callback;
            EventHandler<SimParcelsDownloadedEventArgs> del =
                (sender,e)=>
                {
                    ParcelsDownloaded.Set();
                };

            ParcelsDownloaded.Reset();
            Client.Parcels.SimParcelsDownloaded += del;
            try
            {
                int argsUsed;
                Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;

                Client.Parcels.RequestAllSimParcels(CurSim);

                if (CurSim.IsParcelMapFull())
                    ParcelsDownloaded.Set();

                if (ParcelsDownloaded.WaitOne(30000, false) && Client.Network.Connected)
                {
                    Success(string.Format("Downloaded {0} Parcels in {1} " + System.Environment.NewLine,CurSim.Parcels.Count, CurSim.Name));

                    CurSim.Parcels.ForEach(delegate(Parcel parcel)
                                                                  {
                                                                      Success(string.Format(
                                                                          "Parcel[{0}]: Name: \"{1}\", Description: \"{2}\" ACLBlacklist Count: {3}, ACLWhiteList Count: {5} Traffic: {4}" +
                                                                          System.Environment.NewLine,
                                                                          parcel.LocalID, parcel.Name, parcel.Desc,
                                                                          parcel.AccessBlackList.Count, parcel.Dwell,
                                                                          parcel.AccessWhiteList.Count));
                                                                      //foreach (ParcelManager.ParcelAccessEntry white in parcel.AccessWhiteList)
                                                                      //{
                                                                      //    if(white.AgentID != UUID.Zero)
                                                                      //        sb.AppendFormat("\tAllowed Avatar {0}" + System.Environment.NewLine, white.AgentID);
                                                                      //}
                                                                      //foreach (ParcelManager.ParcelAccessEntry black in parcel.AccessBlackList)
                                                                      //{
                                                                      //    if(black.AgentID != UUID.Zero)
                                                                      //        sb.AppendFormat("\t Banned Avatar {0}" + System.Environment.NewLine, black.AgentID);
                                                                      //}
                                                                  });

                }
                else
                    Failure("Failed to retrieve information on all the simulator parcels");

                return SuccessOrFailure();
            }
            finally
            {
                Client.Parcels.SimParcelsDownloaded -= del;
                Client.Network.OnDisconnected -= callback;
            }

        }

        void Network_OnDisconnected(NetworkManager.DisconnectType reason, string message)
        {
            ParcelsDownloaded.Set();
        }
    }
}
