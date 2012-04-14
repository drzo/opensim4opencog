using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Search.Framework;

    public class BidirectionalMapProblem : Problem, IBidirectionalProblem {

        IMap map;

        Problem reverseProblem;

        public BidirectionalMapProblem(IMap aMap, string initialState,
                string goalState):base(initialState, MapFunctionFactory.GetActionsFunction(aMap),
                    MapFunctionFactory.GetResultFunction(), new DefaultGoalTest(
                            goalState), new MapStepCostFunction(aMap)) {
            map = aMap;

            reverseProblem = new Problem(goalState, MapFunctionFactory
                    .GetActionsFunction(aMap), MapFunctionFactory
                    .GetResultFunction(), new DefaultGoalTest(initialState),
                    new MapStepCostFunction(aMap));
        }

        //
        // START Interface BidrectionalProblem
        public Problem GetOriginalProblem() {
            return this;
        }

        public Problem GetReverseProblem() {
            return reverseProblem;
        }
        // END Interface BirectionalProblem
        //
    }
}
