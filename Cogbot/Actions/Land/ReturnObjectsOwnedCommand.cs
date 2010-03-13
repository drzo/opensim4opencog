using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

namespace cogbot.Actions.Land
{
    public class ReturnObjectsOwnedCommand : Command, RegionMasterCommand
    {
        public ReturnObjectsOwnedCommand(BotClient testClient)
        {
            Name = "ReturnObjectsOwned";
            Description = "Returns all prims with a specific owner. Usage: ReturnObjectsOwned [sim] parcelID Owner";
            Category = CommandCategory.Parcel;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2)
                return ShowUsage();// " selectobjects parcelID OwnerUUID (use parcelinfo to get ID, use parcelprimowners to get ownerUUID)";
            int argsUsed;
            Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;
            int parcelID;
            UUID ownerUUID;

            int counter = 0;
            StringBuilder result = new StringBuilder();
            // test argument that is is a valid integer, then verify we have that parcel data stored in the dictionary
            if (Int32.TryParse(args[argsUsed], out parcelID) && UUIDTryParse(args, argsUsed+1, out ownerUUID, out argsUsed))
            {
                Client.Parcels.ReturnObjects(CurSim,parcelID,ObjectReturnType.Owner,new List<UUID>{ownerUUID});
                return Success("returning parcel=" + parcelID + " sim=" + CurSim + " user=" + ownerUUID);
            }
            else
            {
                return Failure(string.Format("Unable to find Parcel {0} in Parcels Dictionary, Did you run parcelinfo to populate the dictionary first?", args[0]));
            }
        }
    }
}
