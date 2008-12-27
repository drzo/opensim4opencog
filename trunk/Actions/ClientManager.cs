using System;
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class LoginDetails
    {
        public string FirstName;
        public string LastName;
        public string Password;
        public string StartLocation;
        public bool GroupCommands;
        public string MasterName;
        public UUID MasterKey;
        public string URI;
    }

    public class StartPosition
    {
        public string sim;
        public int x;
        public int y;
        public int z;

        public StartPosition()
        {
            this.sim = null;
            this.x = 0;
            this.y = 0;
            this.z = 0;
        }
    }

    // WOW WHAT A HACK!
    public static class ClientManagerRef
    {
        public static cogbot.TextForm ClientManager;
    }
}
