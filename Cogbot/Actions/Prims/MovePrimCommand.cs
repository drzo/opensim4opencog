using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Movement
{
    public class MovePrimCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public MovePrimCommand(BotClient client)
        {
            Name = "moveprim";
            Description = "move prim to the relative specified position. Usage: moveprim <prim> <position>";
            Category = cogbot.Actions.CommandCategory.Objects;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 2)
                return Failure(Usage);// " moveprim prim [x y [z]]";

            int used;
            Primitive P = WorldSystem.GetPrimitive(args, out used);
            if (P == null) return Failure("Cannot find prim: " + string.Join(" ", args));
            SimObject O = WorldSystem.GetSimObject(P);
            if (!O.IsRegionAttached) return Failure("!IsRegionAttached: " + O);

            string[] to = Parser.SplitOff(args, used);
            SimPosition localPos = WorldSystem.GetVector(to, out used, O);

            if (localPos == null) return Failure("Cannot find position: " + string.Join(" ", to));
            if (!localPos.IsRegionAttached) return Failure("!IsRegionAttached: " + localPos);

            Vector3d local = localPos.GlobalPosition;

            O.MoveTo(local,1f,10);
            return Success(WorldSystem.TheSimAvatar.DistanceVectorString(O));
        }
    }


    public class MvPrimCommand : cogbot.Actions.Command
    {
        public MvPrimCommand(BotClient client)
        {
            Name = "mvprim";
            Description = "mv prim to the relative specified position. Usage: mvprim [rel] prim [[+/-]x y [z]]";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 3)
                return Failure(Usage);// " mvprim [rel] prim [[+/-]x y [z]]";

            if (args[0] == "rel")
            {
            }
            int used;
            Primitive P = WorldSystem.GetPrimitive(args, out used);
            SimObject O = WorldSystem.GetSimObject(P);

            used = 1;
            Vector3 prev = O.SimPosition;
            Vector3 local = Vector3.Zero;
            if (float.TryParse(args[used++], out local.X) &&
                float.TryParse(args[used++], out local.Y))
            {

                if (args.Length > used)
                {
                    float.TryParse(args[used++], out local.Z);
                }
                else
                {
                    local.Z = 0f;// O.GetSimPosition().Z;
                }
            }
            local += prev;
            O.SetObjectPosition(local);
            return Success(WorldSystem.TheSimAvatar.DistanceVectorString(O));
        }
    }
}