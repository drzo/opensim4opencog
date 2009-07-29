using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
    abstract public class Listener
    {
     //   protected ClientManager botclient;
        public BotClient client;

        public Listener(BotClient _parent)
        {
            //botclient = _parent;
            client = _parent;//.CurrentClient;
            client.listeners[this.GetModuleName()] = this;
        }

        /// <summary>
        ///  Name registered in the BotClient.registrationTypes collection
        /// </summary>
        /// <returns></returns>
        public abstract string GetModuleName();

        public abstract void StartupListener();
        public abstract void ShutdownListener();
    }
}
