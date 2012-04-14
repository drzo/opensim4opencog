// --------------------------------------------------------------------------------------------------------------------
// <copyright file="SimplifiedRoadMapOfAustralia.cs" company="">
//   
// </copyright>
// <summary>
//   Represents a simplified road map of Australia. The initialization method is
//   declared static. So it can also be used to initialize other specialized
//   subclasses of {@link ExtendableMap} with road map data from Australia. The
//   data was extracted from a class developed by Felix Knittel.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace Aima.Core.Environment.Map
{
    /// <summary>
    /// Represents a simplified road map of Australia. The initialization method is
    /// declared static. So it can also be used to initialize other specialized
    /// subclasses of {@link ExtendableMap} with road map data from Australia. The
    /// data was extracted from a class developed by Felix Knittel.
    /// </summary>
    public class SimplifiedRoadMapOfAustralia : ExtendableMap
    {
        // Locations
        #region Constants and Fields

        /// <summary>
        /// The adelaide.
        /// </summary>
        public static readonly string Adelaide = "Adelaide";

        /// <summary>
        /// The albany.
        /// </summary>
        public static readonly string Albany = "Albany";

        /// <summary>
        /// The alic e_ springs.
        /// </summary>
        public static readonly string AliceSprings = "AliceSprings";

        /// <summary>
        /// The brisbane.
        /// </summary>
        public static readonly string Brisbane = "Brisbane";

        /// <summary>
        /// The broke n_ hill.
        /// </summary>
        public static readonly string BrokenHill = "BrokenHill";

        /// <summary>
        /// The broome.
        /// </summary>
        public static readonly string Broome = "Broome";

        /// <summary>
        /// The cairns.
        /// </summary>
        public static readonly string Cairns = "Cairns";

        /// <summary>
        /// The camarvon.
        /// </summary>
        public static readonly string Camarvon = "Camarvon";

        /// <summary>
        /// The canberra.
        /// </summary>
        public static readonly string Canberra = "Canberra";

        /// <summary>
        /// The charleville.
        /// </summary>
        public static readonly string Charleville = "Charleville";

        /// <summary>
        /// The coobe r_ pedy.
        /// </summary>
        public static readonly string CooberPedy = "CooberPedy";

        /// <summary>
        /// The darwin.
        /// </summary>
        public static readonly string Darwin = "Darwin";

        /// <summary>
        /// The dubbo.
        /// </summary>
        public static readonly string Dubbo = "Dubbo";

        /// <summary>
        /// The esperance.
        /// </summary>
        public static readonly string Esperance = "Esperance";

        /// <summary>
        /// The geraldton.
        /// </summary>
        public static readonly string Geraldton = "Geraldton";

        /// <summary>
        /// The hall s_ creek.
        /// </summary>
        public static readonly string HallsCreek = "HallsCreek";

        /// <summary>
        /// The hay.
        /// </summary>
        public static readonly string Hay = "Hay";

        /// <summary>
        /// The kalgoorlie.
        /// </summary>
        public static readonly string Kalgoorlie = "Kalgoorlie";

        /// <summary>
        /// The katherine.
        /// </summary>
        public static readonly string Katherine = "Katherine";

        /// <summary>
        /// The lake s_ entrance.
        /// </summary>
        public static readonly string LakesEntrance = "LakesEntrance";

        /// <summary>
        /// The longreach.
        /// </summary>
        public static readonly string Longreach = "Longreach";

        /// <summary>
        /// The mackay.
        /// </summary>
        public static readonly string Mackay = "Mackay";

        /// <summary>
        /// The melbourne.
        /// </summary>
        public static readonly string Melbourne = "Melbourne";

        /// <summary>
        /// The moun t_ gambier.
        /// </summary>
        public static readonly string MountGambier = "MountGambier";

        /// <summary>
        /// The m t_ isa.
        /// </summary>
        public static readonly string MtIsa = "MtIsa";

        /// <summary>
        /// The newcastle.
        /// </summary>
        public static readonly string Newcastle = "Newcastle";

        /// <summary>
        /// The norseman.
        /// </summary>
        public static readonly string Norseman = "Norseman";

        /// <summary>
        /// The nyngan.
        /// </summary>
        public static readonly string Nyngan = "Nyngan";

        /// <summary>
        /// The perth.
        /// </summary>
        public static readonly string Perth = "Perth";

        /// <summary>
        /// The por t_ augusta.
        /// </summary>
        public static readonly string PortAugusta = "PortAugusta";

        /// <summary>
        /// The por t_ hedland.
        /// </summary>
        public static readonly string PortHedland = "PortHedland";

        /// <summary>
        /// The por t_ lincoln.
        /// </summary>
        public static readonly string PortLincoln = "PortLincoln";

        /// <summary>
        /// The por t_ macquarie.
        /// </summary>
        public static readonly string PortMacquarie = "PortMacquarie";

        /// <summary>
        /// The rockhampton.
        /// </summary>
        public static readonly string Rockhampton = "Rockhampton";

        /// <summary>
        /// The sydney.
        /// </summary>
        public static readonly string Sydney = "Sydney";

        /// <summary>
        /// The tamworth.
        /// </summary>
        public static readonly string Tamworth = "Tamworth";

        /// <summary>
        /// The tennan t_ creek.
        /// </summary>
        public static readonly string TennantCreek = "TennantCreek";

        /// <summary>
        /// The townsville.
        /// </summary>
        public static readonly string Townsville = "Townsville";

        /// <summary>
        /// The wagg a_ wagga.
        /// </summary>
        public static readonly string WaggaWagga = "WaggaWagga";

        /// <summary>
        /// The warnambool.
        /// </summary>
        public static readonly string Warnambool = "Warnambool";

        /// <summary>
        /// The wyndham.
        /// </summary>
        public static readonly string Wyndham = "Wyndham";

        #endregion

        #region Constructors and Destructors

        /// <summary>
        /// Initializes a new instance of the <see cref="SimplifiedRoadMapOfAustralia"/> class.
        /// </summary>
        public SimplifiedRoadMapOfAustralia()
        {
            InitMap(this);
        }

        #endregion

        #region Public Methods

        /// <summary>
        /// Initializes a map with a simplified road map of Australia.
        /// </summary>
        /// <param name="map">
        /// </param>
        public static void InitMap(ExtendableMap map)
        {
            map.Clear();

            // Add links
            // Distances from http://maps.google.com
            map.AddBidirectionalLink(Perth, Albany, 417.0);
            map.AddBidirectionalLink(Perth, Kalgoorlie, 593.0);
            map.AddBidirectionalLink(Perth, Geraldton, 424.0);
            map.AddBidirectionalLink(Perth, PortHedland, 1637.0);
            map.AddBidirectionalLink(Albany, Esperance, 478.0);
            map.AddBidirectionalLink(Kalgoorlie, Norseman, 187.0);
            map.AddBidirectionalLink(Esperance, Norseman, 204.0);
            map.AddBidirectionalLink(Norseman, PortAugusta, 1668.0);
            map.AddBidirectionalLink(Geraldton, Camarvon, 479.0);
            map.AddBidirectionalLink(Camarvon, PortHedland, 872.0);
            map.AddBidirectionalLink(PortHedland, Broome, 589.0);
            map.AddBidirectionalLink(Broome, HallsCreek, 685.0);
            map.AddBidirectionalLink(HallsCreek, Wyndham, 370.0);
            map.AddBidirectionalLink(HallsCreek, Katherine, 874.0);
            map.AddBidirectionalLink(Wyndham, Katherine, 613.0);
            map.AddBidirectionalLink(Katherine, Darwin, 317.0);
            map.AddBidirectionalLink(Katherine, TennantCreek, 673.0);
            map.AddBidirectionalLink(TennantCreek, MtIsa, 663.0);
            map.AddBidirectionalLink(TennantCreek, AliceSprings, 508.0);
            map.AddBidirectionalLink(AliceSprings, CooberPedy, 688.0);
            map.AddBidirectionalLink(CooberPedy, PortAugusta, 539.0);
            map.AddBidirectionalLink(MtIsa, Townsville, 918.0);
            map.AddBidirectionalLink(Townsville, Cairns, 346.0);
            map.AddBidirectionalLink(MtIsa, Longreach, 647.0);
            map.AddBidirectionalLink(Townsville, Mackay, 388.0);
            map.AddBidirectionalLink(Mackay, Rockhampton, 336.0);
            map.AddBidirectionalLink(Longreach, Rockhampton, 687.0);
            map.AddBidirectionalLink(Rockhampton, Brisbane, 616.0);
            map.AddBidirectionalLink(Longreach, Charleville, 515.0);
            map.AddBidirectionalLink(Charleville, Brisbane, 744.0);
            map.AddBidirectionalLink(Charleville, Nyngan, 657.0);
            map.AddBidirectionalLink(Nyngan, BrokenHill, 588.0);
            map.AddBidirectionalLink(BrokenHill, PortAugusta, 415.0);
            map.AddBidirectionalLink(Nyngan, Dubbo, 166.0);
            map.AddBidirectionalLink(Dubbo, Brisbane, 860.0);
            map.AddBidirectionalLink(Dubbo, Sydney, 466.0);
            map.AddBidirectionalLink(Brisbane, Tamworth, 576.0);
            map.AddBidirectionalLink(Brisbane, PortMacquarie, 555.0);
            map.AddBidirectionalLink(PortMacquarie, Newcastle, 245.0);
            map.AddBidirectionalLink(Tamworth, Newcastle, 284.0);
            map.AddBidirectionalLink(Newcastle, Sydney, 159.0);
            map.AddBidirectionalLink(Sydney, Canberra, 287.0);
            map.AddBidirectionalLink(Canberra, WaggaWagga, 243.0);
            map.AddBidirectionalLink(Dubbo, WaggaWagga, 400.0);
            map.AddBidirectionalLink(Sydney, LakesEntrance, 706.0);
            map.AddBidirectionalLink(LakesEntrance, Melbourne, 317.0);
            map.AddBidirectionalLink(WaggaWagga, Melbourne, 476.0);
            map.AddBidirectionalLink(WaggaWagga, Hay, 269.0);
            map.AddBidirectionalLink(Melbourne, Warnambool, 269.0);
            map.AddBidirectionalLink(Warnambool, MountGambier, 185.0);
            map.AddBidirectionalLink(MountGambier, Adelaide, 449.0);
            map.AddBidirectionalLink(Hay, Adelaide, 655.0);
            map.AddBidirectionalLink(PortAugusta, Adelaide, 306.0);
            map.AddBidirectionalLink(Melbourne, Adelaide, 728.0);
            map.AddBidirectionalLink(PortAugusta, PortLincoln, 341.0);

            // Locations coordinates
            // Alice Springs is taken as central point with coordinates (0|0)
            // Therefore x and y coordinates refer to Alice Springs. Note that
            // the coordinates are not very precise and partly modified to
            // get a more real shape of Australia.
            map.SetPosition(Adelaide, 417, -1289);
            map.SetPosition(Albany, -1559, -1231);
            map.SetPosition(AliceSprings, 0, 0);
            map.SetPosition(Brisbane, 1882, -415);
            map.SetPosition(BrokenHill, 709, -873);
            map.SetPosition(Broome, -1189, 645);
            map.SetPosition(Cairns, 1211, 791);
            map.SetPosition(Camarvon, -2004, 34);
            map.SetPosition(Canberra, 1524, -1189);
            map.SetPosition(Charleville, 1256, -268);
            map.SetPosition(CooberPedy, 86, -593);
            map.SetPosition(Darwin, -328, 1237);
            map.SetPosition(Dubbo, 1474, -881);
            map.SetPosition(Esperance, -1182, -1132);
            map.SetPosition(Geraldton, -1958, -405);
            map.SetPosition(HallsCreek, -630, 624);
            map.SetPosition(Hay, 985, -1143);
            map.SetPosition(Kalgoorlie, -1187, -729);
            map.SetPosition(Katherine, -183, 1025);
            map.SetPosition(LakesEntrance, 1412, -1609);
            map.SetPosition(Longreach, 1057, 49);
            map.SetPosition(Mackay, 1553, 316);
            map.SetPosition(Melbourne, 1118, -1570);
            map.SetPosition(MountGambier, 602, -1531);
            map.SetPosition(MtIsa, 563, 344);
            map.SetPosition(Newcastle, 1841, -979);
            map.SetPosition(Norseman, -1162, -881);
            map.SetPosition(Nyngan, 1312, -781);
            map.SetPosition(Perth, -1827, -814);
            map.SetPosition(PortAugusta, 358, -996);
            map.SetPosition(PortHedland, -1558, 438);
            map.SetPosition(PortLincoln, 169, -1205);
            map.SetPosition(PortMacquarie, 1884, -849);
            map.SetPosition(Rockhampton, 1693, 59);
            map.SetPosition(Sydney, 1778, -1079);
            map.SetPosition(Tamworth, 1752, -722);
            map.SetPosition(TennantCreek, 30, 445);
            map.SetPosition(Townsville, 1318, 520);
            map.SetPosition(WaggaWagga, 1322, -1125);
            map.SetPosition(Warnambool, 761, -1665);
            map.SetPosition(Wyndham, -572, 932);
        }

        #endregion
    }
}