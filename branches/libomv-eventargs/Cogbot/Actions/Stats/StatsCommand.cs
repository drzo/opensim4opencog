using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class StatsCommand : Command, RegionMasterCommand
    {
        public StatsCommand(BotClient testClient)
        {
            Name = "stats";
            Description = "Provide connection figures and statistics";
            Category = CommandCategory.Simulator;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            StringBuilder output = new StringBuilder();

            lock (Client.Network.Simulators)
            {
                for (int i = 0; i < Client.Network.Simulators.Count; i++)
                {
                    Simulator sim = Client.Network.Simulators[i];

                    output.AppendLine(String.Format(
                        "[{0}] Dilation: {1} InBPS: {2} OutBPS: {3} ResentOut: {4}  ResentIn: {5}",
                        sim.ToString(), sim.Stats.Dilation, sim.Stats.IncomingBPS, sim.Stats.OutgoingBPS, 
                        sim.Stats.ResentPackets, sim.Stats.ReceivedResends));
                    output.Append("Packets in the queue: " + Client.Network.InboxCount);
                    Simulator csim = sim;
                    output.AppendLine(String.Format("FPS : {0} PhysicsFPS : {1} AgentUpdates : {2} Objects : {3} Scripted Objects : {4}",
                        csim.Stats.FPS, csim.Stats.PhysicsFPS, csim.Stats.AgentUpdates, csim.Stats.Objects, csim.Stats.ScriptedObjects));
                    output.AppendLine(String.Format("Frame Time : {0} Net Time : {1} Image Time : {2} Physics Time : {3} Script Time : {4} Other Time : {5}",
                        csim.Stats.FrameTime, csim.Stats.NetTime, csim.Stats.ImageTime, csim.Stats.PhysicsTime, csim.Stats.ScriptTime, csim.Stats.OtherTime));
                    output.AppendLine(String.Format("Agents : {0} Child Agents : {1} Active Scripts : {2}",
                        csim.Stats.Agents, csim.Stats.ChildAgents, csim.Stats.ActiveScripts));
                }
            }



            return Success(output.ToString());
        }
    }
}
