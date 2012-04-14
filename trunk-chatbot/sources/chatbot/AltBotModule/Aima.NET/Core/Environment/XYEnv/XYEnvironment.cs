using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Environment.XYEnv
{
    using System.Diagnostics;

    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;
    using Aima.Core.Util.Datastructure;

    public class XYEnvironment : AbstractEnvironment 
    {
        private XYEnvironmentState envState = null;

        public XYEnvironment(int width, int height) 
        {
            Debug.Assert(width > 0);
            Debug.Assert(height > 0);

            envState = new XYEnvironmentState(width, height);
        }

        public override IEnvironmentState GetCurrentState() 
        {
            return envState;
        }

        public override IEnvironmentState ExecuteAction(IAgent a, IAction action) 
        {
            return envState;
        }

        public override IPercept GetPerceptSeenBy(IAgent anAgent) {
            return new DynamicPercept();
        }

        public void AddObjectToLocation(IEnvironmentObject eo, XYLocation loc) {
            this.MoveObjectToAbsoluteLocation(eo, loc);
        }

        public void MoveObjectToAbsoluteLocation(IEnvironmentObject eo,
                XYLocation loc) 
        {
            // Ensure the object is not already at a location
            envState.MoveObjectToAbsoluteLocation(eo, loc);

            // Ensure is added to the environment
            AddEnvironmentObject(eo);
        }

        public void MoveObject(IEnvironmentObject eo, XYLocation.Direction direction) 
        {
            var presentLocation = envState.GetCurrentLocationFor(eo);

            if (null != presentLocation) {
                XYLocation locationToMoveTo = presentLocation.LocationAt(direction);
                if (!(this.IsBlocked(locationToMoveTo))) {
                    this.MoveObjectToAbsoluteLocation(eo, locationToMoveTo);
                }
            }
        }

        public XYLocation GetCurrentLocationFor(IEnvironmentObject eo) {
            return envState.GetCurrentLocationFor(eo);
        }

        public ISet<IEnvironmentObject> GetObjectsAt(XYLocation loc) {
            return envState.GetObjectsAt(loc);
        }

        public ISet<IEnvironmentObject> GetObjectsNear(IAgent agent, int radius) {
            return envState.GetObjectsNear(agent, radius);
        }

        public bool IsBlocked(XYLocation loc)
        {
            return this.envState.GetObjectsAt(loc).OfType<Wall>().Any();
        }

        public void MakePerimeter() 
        {
            for (int i = 0; i < this.envState.Width; i++) 
            {
                XYLocation loc = new XYLocation(i, 0);
                XYLocation loc2 = new XYLocation(i, envState.Height - 1);
                envState.MoveObjectToAbsoluteLocation(new Wall(), loc);
                envState.MoveObjectToAbsoluteLocation(new Wall(), loc2);
            }

            for (int i = 0; i < envState.Height; i++) {
                XYLocation loc = new XYLocation(0, i);
                XYLocation loc2 = new XYLocation(envState.Width - 1, i);
                envState.MoveObjectToAbsoluteLocation(new Wall(), loc);
                envState.MoveObjectToAbsoluteLocation(new Wall(), loc2);
            }
        }
        class XYEnvironmentState : IEnvironmentState
        {
            public int Width { get; private set; }
            public int Height { get; private set; }

            private IDictionary<XYLocation, ISet<IEnvironmentObject>> objsAtLocation = new Dictionary<XYLocation, ISet<IEnvironmentObject>>();

            public XYEnvironmentState(int width, int height)
            {
                this.Width = width;
                this.Height = height;
                for (var h = 1; h <= height; h++)
                {
                    for (var w = 1; w <= width; w++)
                    {
                        objsAtLocation[new XYLocation(h, w)] = new HashedSet<IEnvironmentObject>();
                    }
                }
            }

            public void MoveObjectToAbsoluteLocation(IEnvironmentObject eo, XYLocation loc)
            {
                // Ensure is not already at another location
                foreach (var eos in this.objsAtLocation.Values.Where(eos => eos.Remove(eo)))
                {
                    break; // Should only every be at 1 location
                }
                // Add it to the location specified
                this.GetObjectsAt(loc).Add(eo);
            }

            public ISet<IEnvironmentObject> GetObjectsAt(XYLocation loc)
            {
                var objectsAt = objsAtLocation[loc];
                if (objectsAt == null)
                {
                    // Always ensure an empty Set is returned
                    objectsAt = new HashedSet<IEnvironmentObject>();
                    objsAtLocation[loc] = objectsAt;
                }
                return objectsAt;
            }

            public XYLocation GetCurrentLocationFor(IEnvironmentObject eo)
            {
                return this.objsAtLocation.Keys.FirstOrDefault(loc => this.objsAtLocation[loc].Contains(eo));
            }

            public ISet<IEnvironmentObject> GetObjectsNear(IAgent agent, int radius)
            {
                var objsNear = new HashedSet<IEnvironmentObject>();

                var agentLocation = this.GetCurrentLocationFor(agent);
                foreach (var loc in
                    this.objsAtLocation.Keys.Where(loc => this.WithinRadius(radius, agentLocation, loc)))
                {
                    objsNear.UnionWith(this.objsAtLocation[loc]);
                }
                // Ensure the 'agent' is not included in the Set of
                // objects near
                objsNear.Remove(agent);

                return objsNear;
            }

            public override string ToString()
            {
                return String.Format("XYEnvironmentState:{0}", objsAtLocation);
            }

            private bool WithinRadius(int radius, XYLocation agentLocation,
                    XYLocation objectLocation)
            {
                var xdifference = agentLocation.XCoordinate
                        - objectLocation.XCoordinate;
                var ydifference = agentLocation.YCoordinate
                        - objectLocation.YCoordinate;
                return Math.Sqrt((xdifference * xdifference)
                        + (ydifference * ydifference)) <= radius;
            }
        }
    }
}
