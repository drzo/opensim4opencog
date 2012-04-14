using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Agent.Impl;

    public class MoveToAction : DynamicAction 
    {
        public static readonly string AttributeMoveToLocation = "location";

        public MoveToAction(string location)
            : base("moveTo")
        {
            SetAttribute(AttributeMoveToLocation, location);
        }

        public string GetToLocation() {
            return (String) GetAttribute(AttributeMoveToLocation);
        }
    }
}
