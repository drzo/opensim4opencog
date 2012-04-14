using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.EightPuzzle
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;
    using Aima.Core.Util.Datastructure;

    public class EightPuzzleBoard {

        public static IAction Left = new DynamicAction("Left");

        public static IAction Right = new DynamicAction("Right");

        public static IAction Up = new DynamicAction("Up");

        public static IAction Down = new DynamicAction("Down");

        public int[] State { get; private set; }
        
        //
        // PUBLIC METHODS
        //

        public EightPuzzleBoard() 
        {
            //TODO: figure out if this initial state could be made less static
            this.State = new int[] { 5, 4, 0, 6, 1, 8, 7, 3, 2 };
        }

        public EightPuzzleBoard(int[] s) 
        {
            this.State = new int[s.Length];
            s.CopyTo(this.State,0);
        }

        public EightPuzzleBoard(EightPuzzleBoard copyBoard): this(copyBoard.State) {}

        public int GetValueAt(XYLocation loc) {
            return this.GetValueAt(loc.XCoordinate, loc.YCoordinate);
        }

        public XYLocation GetLocationOf(int val) {
            var absPos = this.GetPositionOf(val);
            return new XYLocation(this.GetXCoord(absPos), this.GetYCoord(absPos));
        }

        public void MoveGapRight() {
            var gapPos = this.GetGapPosition();
            var x = this.GetXCoord(gapPos);
            var ypos = this.GetYCoord(gapPos);
            if (ypos != 2) {
                var valueOnRight = this.GetValueAt(x, ypos + 1);
                this.SetValue(x, ypos, valueOnRight);
                this.SetValue(x, ypos + 1, 0);
            }

        }

        public void MoveGapLeft() {
            var gapPos = this.GetGapPosition();
            var x = this.GetXCoord(gapPos);
            var ypos = this.GetYCoord(gapPos);
            if ((ypos == 0))
            {
                return;
            }
            var valueOnLeft = this.GetValueAt(x, ypos - 1);
            this.SetValue(x, ypos, valueOnLeft);
            this.SetValue(x, ypos - 1, 0);
        }

        public void MoveGapDown() {
            var gapPos = this.GetGapPosition();
            var x = this.GetXCoord(gapPos);
            var y = this.GetYCoord(gapPos);
            if ((x == 2))
            {
                return;
            }
            var valueOnBottom = this.GetValueAt(x + 1, y);
            this.SetValue(x, y, valueOnBottom);
            this.SetValue(x + 1, y, 0);
        }

        public void MoveGapUp() {
            var gapPos = this.GetGapPosition();
            var x = this.GetXCoord(gapPos);
            var y = this.GetYCoord(gapPos);
            if ((x == 0))
            {
                return;
            }
            var valueOnTop = this.GetValueAt(x - 1, y);
            this.SetValue(x, y, valueOnTop);
            this.SetValue(x - 1, y, 0);
        }

        public IList<XYLocation> GetPositions() {
            var retVal = new List<XYLocation>();
            for (var i = 0; i < 9; i++) {
                var absPos = this.GetPositionOf(i);
                var loc = new XYLocation(this.GetXCoord(absPos),
                        this.GetYCoord(absPos));
                retVal.Add(loc);

            }
            return retVal;
        }

        public void SetBoard(IList<XYLocation> locs) {
            var count = 0;
            for (var i = 0; i < locs.Count(); i++) {
                XYLocation loc = locs[i];
                this.SetValue(loc.XCoordinate, loc.YCoordinate, count);
                count = count + 1;
            }
        }

        public bool CanMoveGap(IAction where) {
            var retVal = true;
            var absPos = this.GetPositionOf(0);
            if (where.Equals(Left))
                retVal = (this.GetYCoord(absPos) != 0);
            else if (where.Equals(Right))
                retVal = (this.GetYCoord(absPos) != 2);
            else if (where.Equals(Up))
                retVal = (this.GetXCoord(absPos) != 0);
            else if (where.Equals(Down))
                retVal = (this.GetXCoord(absPos) != 2);
            return retVal;
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
            return o.GetType() == typeof(EightPuzzleBoard) && this.Equals((EightPuzzleBoard)o);
        }

        public override string ToString()
        {
            return String.Format(
                "{0} {1} {2}\n{3} {4} {5}\n{6} {7} {8}",
                State[0],
                State[1],
                State[2],
                State[3],
                State[4],
                State[5],
                State[6],
                State[7],
                State[8]);
        }

        /// <summary>
        ///  Note: The graphic representation maps x values on row numbers (x-axis in vertical direction).
        /// </summary>
        /// <param name="absPos"></param>
        /// <returns></returns>
        private int GetXCoord(int absPos) {
            return absPos / 3;
        }

        /// <summary>
        ///  Note: The graphic representation maps y values on column numbers (y-axis in horizontal direction).
        /// </summary>
        /// <param name="absPos"></param>
        /// <returns></returns>
        private int GetYCoord(int absPos) {
            return absPos % 3;
        }

        private int GetAbsPosition(int x, int y) 
        {
            return x * 3 + y;
        }

        private int GetValueAt(int x, int y) {
            // refactor this use either case or a div/mod soln
            return State[this.GetAbsPosition(x, y)];
        }

        private int GetGapPosition() {
            return this.GetPositionOf(0);
        }

        private int GetPositionOf(int val) {
            var retVal = -1;
            for (int i = 0; i < 9; i++) {
                if (State[i] == val) {
                    retVal = i;
                }
            }
            return retVal;
        }

        private void SetValue(int x, int y, int val) {
            var absPos = this.GetAbsPosition(x, y);
            State[absPos] = val;

        }

        public bool Equals(EightPuzzleBoard other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }

            for (var i = 0; i < 8; i++)
            {
                if (this.GetPositionOf(i) != other.GetPositionOf(i))
                {
                    return false;
                }
            }

            return true;

        }

        public override int GetHashCode()
        {
            return (this.State != null ? this.State.GetHashCode() : 0);
        }
    }
}
