using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent
{
    public interface INotifyEnvironmentViews
    {
        /// <summary>
        /// A simple notification message, to be forewarded to an Environment's
        /// registered EnvironmentViews. 
        /// </summary>
        /// <param name="msg">the message to be forwarded to the EnvironmentViews.</param>
        void NotifyViews(string msg);
    }
}
