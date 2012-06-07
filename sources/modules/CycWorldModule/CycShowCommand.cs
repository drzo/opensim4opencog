using System.Collections.Generic;
using Cogbot;
using Cogbot.Actions;
using Cogbot.World;
using MushDLR223.ScriptEngines;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace CycWorldModule
{
    public class CycShowCommand : Command, GridMasterCommand
    {
        public CycShowCommand(BotClient Client)
            : base(Client)
        {
            Name = "CycShow";
            Parameters = new []{ new NamedParam("uri",typeof(SimObject), typeof(UUID))};
        }
        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argsUsed;
            List<SimObject> target = WorldSystem.GetPrimitives(args, out argsUsed);
            foreach (var o in target)
            {
                AddSuccess("" + o);
                CycWorldModule.CycModule.Show(target);                
            }
            return SuccessOrFailure();
        }
    }
}