using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.NQueens
{
    using Aima.Core.Agent.Impl;
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Queens can be placed, removed, and moved. For movements, a vertical direction
    /// is assumed. Therefore, only the end point needs to be specified.
    /// </summary>
    public class QueenAction : DynamicAction 
    {
        public static readonly String PlaceQueen = "placeQueenAt";
        public static readonly String RemoveQueen = "removeQueenAt";
        public static readonly String MoveQueen = "moveQueenTo";

        public static readonly String AttributeQueenLoc = "location";

        //TODO: create an enumeration instead of these string constants
        /// <summary>
        /// Creates a queen action. Supported values of type are <see cref="PlaceQueen"/>
        /// , <see cref="RemoveQueen"/>, or <see cref="MoveQueen"/>.
        /// </summary>
        /// <param name="type"></param>
        /// <param name="loc"></param>
        public QueenAction(String type, XYLocation loc):base(type) 
        {
            SetAttribute(AttributeQueenLoc, loc);
        }

        public XYLocation GetLocation() {
            return (XYLocation) GetAttribute(AttributeQueenLoc);
        }

        public int GetX() {
            return this.GetLocation().XCoordinate;
        }

        public int GetY() {
            return this.GetLocation().YCoordinate;
        }
    }

}
