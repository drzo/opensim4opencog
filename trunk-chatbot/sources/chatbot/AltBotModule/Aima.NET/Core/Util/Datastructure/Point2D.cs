using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
    public class Point2D
    {
        public double X { get; private set; }

        public double Y { get; private set; }

        public Point2D(double x, double y)
        {
            this.X = x;
            this.Y = y;
        }

        /// <summary>
        /// Returns the Euclidean distance between a specified point and this point.
        /// </summary>
        /// <param name="pt"></param>
        /// <returns></returns>
        public double Distance(Point2D pt)
        {
            var result = (pt.X - this.X) * (pt.X - this.X);
            result += (pt.Y - this.Y) * (pt.Y - this.Y);
            return System.Math.Sqrt(result);
        }
    }
}
