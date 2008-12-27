using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class FollowCommand: Command
    {
        const float DISTANCE_BUFFER = 3.0f;
        uint targetLocalID = 0;

		public FollowCommand(cogbot.TextForm testClient)
		{
			Name = "follow";
			Description = "Follow another avatar. Usage: follow [FirstName LastName]/off.";
            Category = CommandCategory.Movement;

             testClient.client.Network.RegisterCallback(PacketType.AlertMessage, new NetworkManager.PacketCallback(AlertMessageHandler));
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
            // Construct the target name from the passed arguments
			string target = String.Empty;
			for (int ct = 0; ct < args.Length; ct++)
				target = target + args[ct] + " ";
			target = target.TrimEnd();

            if (target.Length == 0 || target == "off")
            {
                Active = false;
                targetLocalID = 0;
                client.Self.AutoPilotCancel();
                return "Following is off";
            }
            else
            {
                if (Follow(target))
                    return "Following " + target;
                else
                    return "Unable to follow " + target + ".  client may not be able to see that avatar.";
            }
		}

        bool Follow(string name)
        {
            lock (client.Network.Simulators)
            {
                for (int i = 0; i < client.Network.Simulators.Count; i++)
                {
                    Avatar target = client.Network.Simulators[i].ObjectsAvatars.Find(
                        delegate(Avatar avatar)
                        {
                            return avatar.Name == name;
                        }
                    );

                    if (target != null)
                    {
                        targetLocalID = target.LocalID;
                        Active = true;
                        return true;
                    }
                }
            }

            if (Active)
            {
                client.Self.AutoPilotCancel();
                Active = false;
            }

            return false;
        }

        bool Follow(UUID id)
        {
            lock (client.Network.Simulators)
            {
                for (int i = 0; i < client.Network.Simulators.Count; i++)
                {
                    Avatar target = client.Network.Simulators[i].ObjectsAvatars.Find(
                        delegate(Avatar avatar)
                        {
                            return avatar.ID == id;
                        }
                    );

                    if (target != null)
                    {
                        targetLocalID = target.LocalID;
                        Active = true;
                        return true;
                    }
                }
            }

            Active = false;
            return false;
        }

		public override void Think()
		{
            if (Active)
            {
                // Find the target position
                lock (client.Network.Simulators)
                {
                    for (int i = 0; i < client.Network.Simulators.Count; i++)
                    {
                        Avatar targetAv;

                        if (client.Network.Simulators[i].ObjectsAvatars.TryGetValue(targetLocalID, out targetAv))
                        {
                            float distance = 0.0f;

                            if (client.Network.Simulators[i] == client.Network.CurrentSim)
                            {
                                distance = Vector3.Distance(targetAv.Position, client.Self.SimPosition);
                            }
                            else
                            {
                                // FIXME: Calculate global distances
                            }

                            if (distance > DISTANCE_BUFFER)
                            {
                                uint regionX, regionY;
                                Utils.LongToUInts(client.Network.Simulators[i].Handle, out regionX, out regionY);

                                double xTarget = (double)targetAv.Position.X + (double)regionX;
                                double yTarget = (double)targetAv.Position.Y + (double)regionY;
                                double zTarget = targetAv.Position.Z - 2f;

                                Logger.DebugLog(String.Format("[Autopilot] {0} meters away from the target, starting autopilot to <{1},{2},{3}>",
                                    distance, xTarget, yTarget, zTarget), client);

                                client.Self.AutoPilot(xTarget, yTarget, zTarget);
                            }
                            else
                            {
                                // We are in range of the target and moving, stop moving
                                client.Self.AutoPilotCancel();
                            }
                        }
                    }
                }
            }

			base.Think();
		}

        private void AlertMessageHandler(Packet packet, Simulator simulator)
        {
            AlertMessagePacket alert = (AlertMessagePacket)packet;
            string message = Utils.BytesToString(alert.AlertData.Message);

            if (message.Contains("Autopilot cancel"))
            {
                Logger.Log("FollowCommand: " + message, Helpers.LogLevel.Info, client);
            }
        }
    }
}
