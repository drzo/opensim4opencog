using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

using Aima.Core.Agent.Impl;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;

    public class MapFunctionFactory {
        private static IResultFunction resultFunction = null;
        private static IPerceptToStateFunction perceptToStateFunction = null;

        public static IActionsFunction GetActionsFunction(IMap aMap) {
            return new MapActionsFunction(aMap);
        }

        public static IResultFunction GetResultFunction() {
            if (null == resultFunction) {
                resultFunction = new MapResultFunction();
            }
            return resultFunction;
        }

        // TODO: this used to be static in java. Figure out if thread safety needs to be implemented
        private class MapActionsFunction : IActionsFunction {
            private IMap map = null;

            public MapActionsFunction(IMap aMap) {
                map = aMap;
            }

            public ISet<IAction> Actions(object state) {
                ISet<IAction> actions = new HashedSet<IAction>();
                var location = state.ToString();

                var linkedLocations = map.GetLocationsLinkedTo(location);
                foreach (var linkLoc in linkedLocations) {
                    actions.Add(new MoveToAction(linkLoc));
                }

                return actions;
            }
        }

        public IPerceptToStateFunction GetPerceptToStateFunction() {
            if (null == perceptToStateFunction) {
                perceptToStateFunction = new MapPerceptToStateFunction();
            }
            return perceptToStateFunction;
        }

        private class MapResultFunction : IResultFunction {
            public object Result(object s, IAction a) {

                if (a is MoveToAction) {
                    var mta = (MoveToAction) a;

                    return mta.GetToLocation();
                }

                // The Action is not understood or is a NoOp
                // the result will be the current state.
                return s;
            }
        }

        private class MapPerceptToStateFunction : IPerceptToStateFunction 
        {
            public object GetState(IPercept p) {
                return ((DynamicPercept) p)
                        .GetAttribute(DynAttributeNames.PerceptIn);
            }
        }
    }

}
