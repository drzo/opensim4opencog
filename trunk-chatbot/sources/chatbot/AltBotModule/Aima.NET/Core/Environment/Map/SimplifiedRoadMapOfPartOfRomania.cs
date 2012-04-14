using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    /// <summary>
    /// Represents a simplified road map of Romania. The initialization method is
    /// declared static. So it can also be used to initialize other specialized
    /// subclasses of <see cref="ExtendableMap" /> with road map data from Romania. Location
    /// names, road distances and directions have been extracted from Artificial
    /// Intelligence A Modern Approach (2nd Edition), Figure 3.2, page 63. The
    /// straight-line distances to Bucharest have been taken from Artificial
    /// Intelligence A Modern Approach (2nd Edition), Figure 4.1, page 95.
    /// </summary>
    public class SimplifiedRoadMapOfPartOfRomania : ExtendableMap 
    {

        public SimplifiedRoadMapOfPartOfRomania() 
        {
            initMap(this);
        }

        // The different locations in the simplified map of part of Romania
        public static readonly string Oradea = "Oradea";
        public static readonly string Zerind = "Zerind";
        public static readonly string Arad = "Arad";
        public static readonly string Timisoara = "Timisoara";
        public static readonly string Lugoj = "Lugoj";
        public static readonly string Mehadia = "Mehadia";
        public static readonly string Dobreta = "Dobreta";
        public static readonly string Sibiu = "Sibiu";
        public static readonly string RimnicuVilcea = "RimnicuVilcea";
        public static readonly string Craiova = "Craiova";
        public static readonly string Fagaras = "Fagaras";
        public static readonly string Pitesti = "Pitesti";
        public static readonly string Giurgiu = "Giurgiu";
        public static readonly string Bucharest = "Bucharest";
        public static readonly string Neamt = "Neamt";
        public static readonly string Urziceni = "Urziceni";
        public static readonly string Iasi = "Iasi";
        public static readonly string Vaslui = "Vaslui";
        public static readonly string Hirsova = "Hirsova";
        public static readonly string Eforie = "Eforie";

        /**
         * Initializes a map with a simplified road map of Romania.
         */
        public static void initMap(ExtendableMap map) {
            // mapOfRomania
            map.Clear();
            map.AddBidirectionalLink(Oradea, Zerind, 71.0);
            map.AddBidirectionalLink(Oradea, Sibiu, 151.0);
            map.AddBidirectionalLink(Zerind, Arad, 75.0);
            map.AddBidirectionalLink(Arad, Timisoara, 118.0);
            map.AddBidirectionalLink(Arad, Sibiu, 140.0);
            map.AddBidirectionalLink(Timisoara, Lugoj, 111.0);
            map.AddBidirectionalLink(Lugoj, Mehadia, 70.0);
            map.AddBidirectionalLink(Mehadia, Dobreta, 75.0);
            map.AddBidirectionalLink(Dobreta, Craiova, 120.0);
            map.AddBidirectionalLink(Sibiu, Fagaras, 99.0);
            map.AddBidirectionalLink(Sibiu, RimnicuVilcea, 80.0);
            map.AddBidirectionalLink(RimnicuVilcea, Pitesti, 97.0);
            map.AddBidirectionalLink(RimnicuVilcea, Craiova, 146.0);
            map.AddBidirectionalLink(Craiova, Pitesti, 138.0);
            map.AddBidirectionalLink(Fagaras, Bucharest, 211.0);
            map.AddBidirectionalLink(Pitesti, Bucharest, 101.0);
            map.AddBidirectionalLink(Giurgiu, Bucharest, 90.0);
            map.AddBidirectionalLink(Bucharest, Urziceni, 85.0);
            map.AddBidirectionalLink(Neamt, Iasi, 87.0);
            map.AddBidirectionalLink(Urziceni, Vaslui, 142.0);
            map.AddBidirectionalLink(Urziceni, Hirsova, 98.0);
            map.AddBidirectionalLink(Iasi, Vaslui, 92.0);
            // AddBidirectionalLink(VASLUI - already all linked
            map.AddBidirectionalLink(Hirsova, Eforie, 86.0);
            // AddBidirectionalLink(EFORIE - already all linked

            // distances and directions
            // reference location: Bucharest
            map.SetDistAndDirToRefLocation(Arad, 366, 117);
            map.SetDistAndDirToRefLocation(Bucharest, 0, 360);
            map.SetDistAndDirToRefLocation(Craiova, 160, 74);
            map.SetDistAndDirToRefLocation(Dobreta, 242, 82);
            map.SetDistAndDirToRefLocation(Eforie, 161, 282);
            map.SetDistAndDirToRefLocation(Fagaras, 176, 142);
            map.SetDistAndDirToRefLocation(Giurgiu, 77, 25);
            map.SetDistAndDirToRefLocation(Hirsova, 151, 260);
            map.SetDistAndDirToRefLocation(Iasi, 226, 202);
            map.SetDistAndDirToRefLocation(Lugoj, 244, 102);
            map.SetDistAndDirToRefLocation(Mehadia, 241, 92);
            map.SetDistAndDirToRefLocation(Neamt, 234, 181);
            map.SetDistAndDirToRefLocation(Oradea, 380, 131);
            map.SetDistAndDirToRefLocation(Pitesti, 100, 116);
            map.SetDistAndDirToRefLocation(RimnicuVilcea, 193, 115);
            map.SetDistAndDirToRefLocation(Sibiu, 253, 123);
            map.SetDistAndDirToRefLocation(Timisoara, 329, 105);
            map.SetDistAndDirToRefLocation(Urziceni, 80, 247);
            map.SetDistAndDirToRefLocation(Vaslui, 199, 222);
            map.SetDistAndDirToRefLocation(Zerind, 374, 125);
        }
    }

}
