using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.TicTacToe
{
    using System.Collections;
    using System.Diagnostics;

    using Aima.Core.Util.Datastructure;

    public class TicTacToeBoard :ICloneable
    {
        public static readonly String O = "O";
        public static readonly String X = "X";
        public static readonly String Empty = "-";

        private String[] state = new[] { Empty, Empty, Empty, Empty, Empty, Empty,
                Empty, Empty, Empty };

        public string[] State
        {
            get
            {
                return this.state;
            }
            set
            {
                this.state = value;
            }
        }

        public bool IsEmpty(int row, int col) {
            return this.State[GetAbsPosition(row, col)] == Empty;
        }

        public bool IsMarked(string str, int i, int j) 
    {
            return this.GetValue(i, j).Equals(str);
        }

        public void MarkX(int row, int col) {
            this.Mark(row, col, X);
        }

        public void MarkO(int row, int col) {
            this.Mark(row, col, O);
        }

        private void Mark(int row, int col, String symbol) {
            this.State[GetAbsPosition(row, col)] = symbol;
        }

        public bool IsAnyRowComplete() {
            for (var i = 0; i < 3; i++) {
                var val = this.GetValue(i, 0);
                if (val != Empty && val == this.GetValue(i, 1) && val == this.GetValue(i, 2))
                    return true;
            }
            return false;
        }

        public bool IsAnyColumnComplete() 
        {
            for (var j = 0; j < 3; j++) {
                var val = this.GetValue(0, j);
                if (val != Empty && val == this.GetValue(1, j) && val == this.GetValue(2, j))
                    return true;
            }
            return false;
        }

        public bool IsAnyDiagonalComplete() 
        {
            var retVal = false;
            var val = this.GetValue(0, 0);
            if (val != Empty && val == this.GetValue(1, 1) && val == this.GetValue(2, 2))
                return true;
            val = this.GetValue(0, 2);
            if (val != Empty && val == this.GetValue(1, 1) && val == this.GetValue(2, 0))
                return true;
            return retVal;
        }

        public bool LineThroughBoard() {
            return (this.IsAnyRowComplete() || this.IsAnyColumnComplete() || this.IsAnyDiagonalComplete());
        }

        public String GetValue(int row, int col) {
            return this.State[GetAbsPosition(row, col)];
        }

        private void SetValue(int row, int col, String val) {
            this.State[GetAbsPosition(row, col)] = val;
        }

        public TicTacToeBoard CloneBoard() {
            return (TicTacToeBoard) Clone();
        }

        public object Clone() 
        {
            var newBoard = new TicTacToeBoard();
            for (var i = 0; i < 3; i++) {
                for (var j = 0; j < 3; j++) {
                    var s = this.GetValue(i, j);
                    newBoard.SetValue(i, j, s);
                }
            }
            return newBoard;
        }

        public int GetNumberOfMarkedPositions() {
            int retVal = 0;
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    if (!(this.IsEmpty(i, j))) {
                        retVal++;
                    }
                }
            }
            return retVal;
        }

        public IList<XYLocation> GetUnMarkedPositions() {
            var retVal = new List<XYLocation>();
            for (var i = 0; i < 3; i++) {
                for (var j = 0; j < 3; j++) {
                    if (this.IsEmpty(i, j)) {
                        retVal.Add(new XYLocation(i, j));
                    }
                }

            }
            return retVal;
        }

        
        public override bool Equals(object anObj) 
        {
            if (ReferenceEquals(null, anObj) || !(anObj is TicTacToeBoard))
            {
                return false;
            }
            return Equals((TicTacToeBoard)anObj);
        }

        public override string ToString() {
            var buf = new StringBuilder();
            for (var i = 0; i < 3; i++) {
                for (var j = 0; j < 3; j++)
                    buf.Append(this.GetValue(i, j) + " ");
                buf.Append("\n");
            }
            return buf.ToString();
        }

        public void Print() {
            for (var i = 0; i < 3; i++) {
                for (var j = 0; j < 3; j++)
                    Debug.Write(this.GetValue(i, j) + " ");
                Debug.WriteLine("");
            }
        }

        private int GetAbsPosition(int row, int col) 
        {
            return row * 3 + col;
        }

        public bool Equals(TicTacToeBoard other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.state, this.state);
        }

        public override int GetHashCode()
        {
            return (this.state != null ? this.state.GetHashCode() : 0);
        }
    }
}
