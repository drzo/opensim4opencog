using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    public class Listener
    {
     //   protected TextForm botclient;
        protected BotClient client;

        public Listener(BotClient _parent)
        {
            //botclient = _parent;
            client = _parent;//.CurrentClient;
        }
    }
}
