using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.NQueens
{
    using System.Diagnostics;

    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Represents a quadratic board with a matrix of squares on which queens can be
    /// placed (only one per square) and moved.
    /// </summary>
    public class NQueensBoard 
    {
        /// <summary>
        /// X---> increases left to right with zero based index Y increases top to
        /// bottom with zero based index | | V
        /// </summary>
        int[,] squares;

        public int Size { get; private set;}

        public NQueensBoard(int n) 
        {
            Size = n;
            squares = new int[Size,Size];
            for (var i = 0; i < Size; i++) {
                for (var j = 0; j < Size; j++) {
                    squares[i,j] = 0;
                }
            }
        }

        public void Clear() {
            for (var i = 0; i < Size; i++) {
                for (var j = 0; j < Size; j++) {
                    squares[i,j] = 0;
                }
            }
        }

        public void SetBoard(IList<XYLocation> al)
        {
            this.Clear();
            foreach (var t in al)
            {
                this.AddQueenAt(t);
            }
        }

        public void AddQueenAt(XYLocation l) {
            if (!(this.QueenExistsAt(l)))
                squares[l.XCoordinate, l.YCoordinate] = 1;
        }

        public void RemoveQueenFrom(XYLocation l) {
            if (squares[l.XCoordinate, l.YCoordinate] == 1) 
            {
                squares[l.XCoordinate, l.YCoordinate] = 0;
            }
        }

        /// <summary>
        /// Moves the queen in the specified column (x-value of <paramref name="l"/>) to
        /// the specified row (y-value of <paramref name="l"/>). The action assumes a
        /// complete-state formulation of the n-queens problem.
        /// </summary>
        /// <param name="l"></param>
        public void MoveQueenTo(XYLocation l) 
        {
            for (var i = 0; i < Size; i++)
                squares[l.XCoordinate, i] = 0;
            squares[l.XCoordinate, l.YCoordinate] = 1;
        }

        public void MoveQueen(XYLocation from, XYLocation to) 
        {
            if ((this.QueenExistsAt(from)) && (!(this.QueenExistsAt(to)))) {
                this.RemoveQueenFrom(from);
                this.AddQueenAt(to);
            }
        }

        public bool QueenExistsAt(XYLocation l) 
        {
            return (this.QueenExistsAt(l.XCoordinate, l.YCoordinate));
        }

        private bool QueenExistsAt(int x, int y)
        {
            return (squares[x,y] == 1);
        }

        public int GetNumberOfQueensOnBoard() 
        {
            var count = 0;
            for (var i = 0; i < Size; i++) {
                for (var j = 0; j < Size; j++) {
                    if (squares[i,j] == 1)
                        count++;
                }
            }
            return count;
        }

        public List<XYLocation> GetQueenPositions() 
        {
            var result = new List<XYLocation>();
            for (var i = 0; i < Size; i++) {
                for (var j = 0; j < Size; j++) {
                    if (this.QueenExistsAt(i, j))
                        result.Add(new XYLocation(i, j));
                }
            }
            return result;

        }

        public int GetNumberOfAttackingPairs() {
            var result = 0;
            foreach (XYLocation location in this.GetQueenPositions()) 
            {
                result += this.GetNumberOfAttacksOn(location);
            }
            return result / 2;
        }

        public int GetNumberOfAttacksOn(XYLocation l) {
            var x = l.XCoordinate;
            var y = l.YCoordinate;
            return this.NumberOfHorizontalAttacksOn(x, y)
                    + this.NumberOfVerticalAttacksOn(x, y)
                    + this.NumberOfDiagonalAttacksOn(x, y);
        }

        public bool IsSquareUnderAttack(XYLocation l) 
        {
            int x = l.XCoordinate;
            int y = l.YCoordinate;
            return (this.IsSquareHorizontallyAttacked(x, y)
                    || this.IsSquareVerticallyAttacked(x, y) || this.IsSquareDiagonallyAttacked(
                    x, y));
        }

        private bool IsSquareHorizontallyAttacked(int x, int y) 
        {
            return this.NumberOfHorizontalAttacksOn(x, y) > 0;
        }

        private bool IsSquareVerticallyAttacked(int x, int y) 
        {
            return this.NumberOfVerticalAttacksOn(x, y) > 0;
        }

        private bool IsSquareDiagonallyAttacked(int x, int y)
        {
            return this.NumberOfDiagonalAttacksOn(x, y) > 0;
        }

        private int NumberOfHorizontalAttacksOn(int x, int y) 
        {
            var retVal = 0;
            for (var i = 0; i < Size; i++) {
                if ((this.QueenExistsAt(i, y)))
                    if (i != x)
                        retVal++;
            }
            return retVal;
        }

        private int NumberOfVerticalAttacksOn(int x, int y) 
        {
            int retVal = 0;
            for (int j = 0; j < Size; j++) 
            {
                if ((this.QueenExistsAt(x, j)))
                    if (j != y)
                        retVal++;
            }
            return retVal;
        }

        private int NumberOfDiagonalAttacksOn(int x, int y) 
        {
            var retVal = 0;
            int i;
            int j;
            // forward up diagonal
            for (i = (x + 1), j = (y - 1); (i < Size && (j > -1)); i++, j--) {
                if (this.QueenExistsAt(i, j))
                    retVal++;
            }
            // forward down diagonal
            for (i = (x + 1), j = (y + 1); ((i < Size) && (j < Size)); i++, j++) {
                if (this.QueenExistsAt(i, j))
                    retVal++;
            }
            // backward up diagonal
            for (i = (x - 1), j = (y - 1); ((i > -1) && (j > -1)); i--, j--) {
                if (this.QueenExistsAt(i, j))
                    retVal++;
            }

            // backward down diagonal
            for (i = (x - 1), j = (y + 1); ((i > -1) && (j < Size)); i--, j++) {
                if (this.QueenExistsAt(i, j))
                    retVal++;
            }

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
            if (o.GetType() != typeof(NQueensBoard))
            {
                return false;
            }
            return Equals((NQueensBoard)o);
        }

        public void Print() {
            Debug.Print(this.GetBoardPic());
        }

        public string GetBoardPic()
        {
            var buffer = new StringBuilder();
            for (var row = 0; row < this.Size; row++)
            {
                // row
                for (var col = 0; col < this.Size; col++)
                {
                    // col
                    buffer.Append(this.QueenExistsAt(col, row) ? " Q " : " - ");
                }
                buffer.Append("\n");
            }
            return buffer.ToString();
        }

        public override string ToString()
        {
            var buf = new StringBuilder();
            for (var row = 0; row < this.Size; row++)
            {
                // rows
                for (var col = 0; col < this.Size; col++)
                {
                    // columns
                    buf.Append(this.QueenExistsAt(col, row) ? 'Q' : '-');
                }
                buf.Append("\n");
            }
            return buf.ToString();
        }

        public bool Equals(NQueensBoard other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.squares, this.squares) && other.Size == this.Size;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.squares != null ? this.squares.GetHashCode() : 0) * 397) ^ this.Size;
            }
        }
    }
}
