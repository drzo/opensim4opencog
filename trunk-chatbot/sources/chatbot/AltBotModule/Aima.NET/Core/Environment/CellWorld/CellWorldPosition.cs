// --------------------------------------------------------------------------------------------------------------------
// <copyright file="CellWorldPosition.cs" company="">
//   
// </copyright>
// <summary>
//   Defines the CellWorldPosition type.
// </summary>
// --------------------------------------------------------------------------------------------------------------------


namespace Aima.Core.Environment.CellWorld
{
    using System;

    public class CellWorldPosition 
    {
        /// <summary>
        /// Gets X.
        /// </summary>
        public int X { get; private set; }

        /// <summary>
        /// Gets Y.
        /// </summary>
        public int Y { get; private set; }

        public CellWorldPosition(int x, int y) 
        {
            this.X = x;
            this.Y = y;
        }

        public override bool Equals(object o)
        {
            if (o == this)
            {
                return true;
            }
            if (!(o is CellWorldPosition))
            {
                return false;
            }
            var cwp = (CellWorldPosition)o;
            return (this.X == cwp.X) && (this.Y == cwp.Y);
        }

        public override int GetHashCode() 
        {
            //TODO: implement logic that will be able to treat more than 31 x positions
            return this.X + (31 * this.Y);
        }

        public override String ToString()
        {
            return String.Format("< {0} , {1} >", this.X, this.Y);
        }
    }
}
