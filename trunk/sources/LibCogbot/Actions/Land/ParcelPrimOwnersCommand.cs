using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Land
{
    public class ParcelPrimOwnersCommand : Command, RegionMasterCommand
    {
        public ParcelPrimOwnersCommand(BotClient testClient)
        {
            Name = "primowners";
            Description = "Displays a list of prim owners and prim counts on a parcel. Usage: primowners parcelID";
            Category = CommandCategory.Parcel;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " primowners parcelID (use parcelinfo to get ID)";

            int argsUsed;
            Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;
            int parcelID;
            Parcel parcel;
            StringBuilder result = new StringBuilder();
            // test argument that is is a valid integer, then verify we have that parcel data stored in the dictionary
            if (Int32.TryParse(args[argsUsed], out parcelID) && CurSim.Parcels.TryGetValue(parcelID, out parcel))
            {
                AutoResetEvent wait = new AutoResetEvent(false);

                EventHandler<ParcelObjectOwnersReplyEventArgs> callback = delegate(object sender, ParcelObjectOwnersReplyEventArgs e)
                {
                    for (int i = 0; i < e.PrimOwners.Count; i++)
                    {
                        result.AppendFormat("Owner: {0} Count: {1}" + Environment.NewLine, e.PrimOwners[i].OwnerID, e.PrimOwners[i].Count);
                        wait.Set();
                    }
                };
                
                Client.Parcels.ParcelObjectOwnersReply += callback;
                try
                {
                    Client.Parcels.RequestObjectOwners(CurSim, parcelID);
                    if (!wait.WaitOne(10000, false))
                    {
                        return Failure("Timed out waiting for packet.");
                    }

                }
                finally
                {
                    Client.Parcels.ParcelObjectOwnersReply -= callback;                    
                }
                
                return Success(result.ToString());;
            }
            else
            {
                return Failure(string.Format("Unable to find Parcel {0} in Parcels Dictionary, Did you run parcelinfo to populate the dictionary first?", args[0]));
            }
        }
    }
}
