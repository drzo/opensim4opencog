using System;

namespace Aima.Core.Environment.Map
{
    public class StraightLineDistanceHeuristicFunction : AdaptableHeuristicFunction 
    {

        public StraightLineDistanceHeuristicFunction(object goal, IMap map) 
        {
            this.Goal = goal;
            this.Map = map;
        }

        public override double H(object state) 
        {
            var result = 0.0;
            var pt1 = Map.GetPosition((string)state);
            var pt2 = Map.GetPosition((string)Goal);
            if (pt1 != null && pt2 != null)
            {
                result = pt1.Distance(pt2);
            }
            return result;
        }
    }
}
