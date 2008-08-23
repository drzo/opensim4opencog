using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    public class Listener
    {
        protected TextForm parent;
        protected GridClient client;

        public Listener(TextForm _parent)
        {
            parent = _parent;
            client = parent.client;
        }
    }
}
