using System.Collections.Generic;
using cogbot;
using cogbot.Actions;
using cogbot.TheOpenSims;
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
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            List<SimObject> target = WorldSystem.GetPrimitives(args, out argsUsed);
            foreach (var o in target)
            {
                Success(""+o);
                CycWorldModule.CycModule.Show(target);                
            }
            return SuccessOrFailure();
        }
    }
}