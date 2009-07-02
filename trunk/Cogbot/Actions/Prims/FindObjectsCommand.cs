using System;
using System.Collections.Generic;
using System.Threading;

using OpenMetaverse;


namespace cogbot.Actions
{
    public class FindObjectsCommand : Command
    {
        Dictionary<UUID, Primitive> PrimsWaiting = new Dictionary<UUID, Primitive>();
        AutoResetEvent AllPropertiesReceived = new AutoResetEvent(false);
        ObjectManager.ObjectPropertiesCallback callback;

        public FindObjectsCommand(BotClient testClient)
        {
            callback = new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);

            Name = "findobjects";
            Description = "Finds all objects, which name contains search-string. " +
                "Usage: findobjects [radius] <search-string>";
            Category = CommandCategory.Objects;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            // *** parse arguments ***
            if ((args.Length < 1) || (args.Length > 2))
                return "Usage: findobjects [radius] <search-string>";
            float radius = float.Parse(args[0]);
            string searchString = (args.Length > 1)? args[1] : "";

            // *** get current location ***
            Vector3 location = GetSimPosition();

            try
            {
                Client.Objects.OnObjectProperties += callback;
                // *** find all objects in radius ***
                List<Primitive> prims = Client.Network.CurrentSim.ObjectsPrimitives.FindAll(
                    delegate(Primitive prim)
                    {
                        Vector3 pos = prim.Position;
                        return ((prim.ParentID == 0) && (pos != Vector3.Zero) && (Vector3.Distance(pos, location) < radius));
                    }
                );

                // *** request properties of these objects ***
                bool complete = RequestObjectProperties(prims, 250);

                foreach (Primitive p in prims)
                {
                    string name = p.Properties.Name;
                    if ((name != null) && (name.Contains(searchString)))
                        WriteLine(String.Format("Object '{0}': {1}", name, p.ID.ToString()));
                }

                if (!complete)
                {
                    WriteLine("Warning: Unable to retrieve full properties for:");
                    foreach (UUID uuid in PrimsWaiting.Keys)
                        WriteLine(uuid.ToString());
                }

                return "Done searching";
            }
            finally
            {
                Client.Objects.OnObjectProperties -= callback;
            }
        }

        private bool RequestObjectProperties(List<Primitive> objects, int msPerRequest)
        {
            // Create an array of the local IDs of all the prims we are requesting properties for
            uint[] localids = new uint[objects.Count];

            lock (PrimsWaiting) {
                PrimsWaiting.Clear();

                for (int i = 0; i < objects.Count; ++i) {
                    localids[i] = objects[i].LocalID;
                    PrimsWaiting.Add(objects[i].ID, objects[i]);
                }
            }

            Client.Objects.SelectObjects(Client.Network.CurrentSim, localids);

            return AllPropertiesReceived.WaitOne(2000 + msPerRequest * objects.Count, false);
        }

        void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
        {
            lock (PrimsWaiting) {
                Primitive prim;
                if (PrimsWaiting.TryGetValue(properties.ObjectID, out prim)) {
                    prim.Properties = properties;
                }
                PrimsWaiting.Remove(properties.ObjectID);

                if (PrimsWaiting.Count == 0)
                    AllPropertiesReceived.Set();
            }
        }
    }
}
