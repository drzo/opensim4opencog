using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Listeners
{
    abstract public class WorldObjectsModule : AListener
    {
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