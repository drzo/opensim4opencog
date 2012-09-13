using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using Apache.Qpid.Messaging;
using Cogbot;
using Cogbot;
using Cogbot.World;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using RoboKindAvroQPID;

namespace WorldRoboKindAvroQPIDModule
{
    public class WorldRoboKindAvroQPIDModuleMain : WorldObjectsModule, YesAutoLoad, SimEventSubscriber
    {
        public BotClient botClient;
        private RoboKindEventModule qpid;


        public WorldRoboKindAvroQPIDModuleMain(BotClient _parent)
            : base(_parent)
        {
            botClient = _parent;
            qpid = new RoboKindEventModule();
        }

        #region Overrides of AListener

        public override void StartupListener()
        {
            throw new NotImplementedException();
        }

        public void OnEvent(CogbotEvent evt)
        {
            qpid.OnEvent(evt);
        }

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public override void Dispose()
        {
            qpid.Dispose();
        }

        public bool EventsEnabled
        {
            get { return qpid.EventsEnabled; }
            set { qpid.EventsEnabled = value; }
        }

        #endregion
    }
}