using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
    /// <summary>
    /// Note: If looking at a rectangle - the coordinate (x=0, y=0) will be the top left hand corner.
    /// </summary>
    public class XYLocation 
    {
        public enum Direction 
        {
            North, South, East, West
        }

        public int XCoordinate {get; private set; }

        public int YCoordinate { get; private set; }

        public XYLocation(int x, int y) {
            XCoordinate = x;
            YCoordinate = y;
        }

        public XYLocation West() {
            return new XYLocation(XCoordinate - 1, YCoordinate);
        }

        public XYLocation East() {
            return new XYLocation(XCoordinate + 1, YCoordinate);
        }

        public XYLocation North() {
            return new XYLocation(XCoordinate, YCoordinate - 1);
        }

        public XYLocation South() {
            return new XYLocation(XCoordinate, YCoordinate + 1);
        }

        public XYLocation Right() {
            return this.East();
        }

        public XYLocation Left() {
            return this.West();
        }

        public XYLocation Up() {
            return this.North();
        }

        public XYLocation Down() {
            return this.South();
        }

        public XYLocation LocationAt(Direction direction) {
            if (direction.Equals(Direction.North)) {
                return this.North();
            }
            if (direction.Equals(Direction.South)) {
                return this.South();
            }
            if (direction.Equals(Direction.East)) {
                return this.East();
            }
            if (direction.Equals(Direction.West)) {
                return this.West();
            } 
            throw new ArgumentException("Unknown direction " + direction);
        }

        public override bool Equals(object o) {
            if (ReferenceEquals(null, o))
            {
                return false;
            }
            if (ReferenceEquals(this, o))
            {
                return true;
            }
            return o.GetType() == typeof(XYLocation) && this.Equals((XYLocation)o);
        }

        public override string ToString() 
        {
            return String.Format(" ( {0} , {1} ) ", XCoordinate, YCoordinate);
        }

        public bool Equals(XYLocation other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return other.XCoordinate == this.XCoordinate && other.YCoordinate == this.YCoordinate;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (this.XCoordinate * 397) ^ this.YCoordinate;
            }
        }
    }
}
