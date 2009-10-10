using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions.Movement
{
    public class MovePrimCommand : cogbot.Actions.Command
    {
        public MovePrimCommand(BotClient client)
        {
            Name = "moveprim";
            Description = "move prim to the relative specified position. Usage: moveprim prim [x y [z]]";
            Category = cogbot.Actions.CommandCategory.Objects;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 3)
                return Failure(Usage);// " moveprim prim [x y [z]]";

            int used;
            Primitive P = WorldSystem.GetPrimitive(args, out used);
            SimObject O = WorldSystem.GetSimObject(P);

            used = 1;
            Vector3d prev = O.GlobalPosition;
            Vector3d local = Vector3d.Zero;
            if (double.TryParse(args[used++], out local.X) &&
                double.TryParse(args[used++], out local.Y))
            {

                if (args.Length > used)
                {
                    double.TryParse(args[used++], out local.Z);
                }
                else
                {
                    local.Z = 0f;// O.GetSimPosition().Z;
                }
            }
            local += prev;
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