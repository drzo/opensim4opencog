using Cogbot.World;
using OpenMetaverse;

namespace Cogbot
{
    abstract public class WorldObjectsModule : AListener
    {
        public string startupOptions;
        public WorldObjects WorldSystem
        {
            get
            {
                return client.WorldSystem;
            }
        }

        public WorldObjectsModule(BotClient _parent)
            : base(_parent)
        {
            // _parent.RegisterModule(this);
        }

        public SimObject GetSimObject(Primitive prim)
        {
            return WorldSystem.GetSimObject(prim);
        }
    }
}