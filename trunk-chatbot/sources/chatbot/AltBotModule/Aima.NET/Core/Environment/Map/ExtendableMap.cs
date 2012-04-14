using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aima.Core.Util;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Implements a map with locations, distance labeled links between the
    /// locations, straight line distances, and 2d-placement positions of locations.
    /// Locations are represented by strings and travel distances by double values.
    /// Locations and links can be added dynamically and removed after creation. This
    /// enables to read maps from file or to modify them with respect to newly
    /// obtained knowledge.
    /// </summary>
    public class ExtendableMap : IMap {

        /// <summary>
        /// Stores map data. Locations are represented as vertices and connections
        /// (links) as directed edges labeled with corresponding travel distances.
        /// </summary>
        private readonly LabeledGraph<string, double> links;

        /// <summary>
        /// Stores xy-coordinates for each location.
        /// </summary>
        private readonly Dictionary<string, Point2D> locationPositions;

        /// <summary>
        /// Creates an empty map.
        /// </summary>
        public ExtendableMap() {
            links = new LabeledGraph<string, double>();
            locationPositions = new Dictionary<string, Point2D>();
        }

        /// <summary>
        /// Removes everything.
        /// </summary>
        public void Clear() {
            links.Clear();
            locationPositions.Clear();
        }

        /// <summary>
        /// Clears all connections but keeps location position informations.
        /// </summary>
        public void ClearLinks() {
            links.Clear();
        }

        /// <summary>
        /// Returns a list of all locations.
        /// </summary>
        /// <returns></returns>
        public IList<String> GetLocations() {
            return links.GetVertexLabels();
        }

        /// <summary>
        /// Checks whether the given string is the name of a location.
        /// </summary>
        /// <param name="str"></param>
        /// <returns></returns>
        public bool IsLocation(string str) {
            return links.IsVertexLabel(str);
        }

        /// <summary>
        /// Answers to the question: Where can I get, following one of the
        /// connections starting at the specified location?
        /// </summary>
        /// <param name="fromLocation"></param>
        /// <returns></returns>
        public IList<string> GetLocationsLinkedTo(string fromLocation) {
            IList<string> result = links.GetSuccessors(fromLocation);
            return result.OrderBy(s => s).ToList();
        }

        /// <summary>
        /// Returns the travel distance between the two specified locations if they
        /// are linked by a connection and null otherwise.
        /// </summary>
        /// <param name="fromLocation"></param>
        /// <param name="toLocation"></param>
        /// <returns></returns>
        public double GetDistance(string fromLocation, string toLocation) {
            return links.Get(fromLocation, toLocation);
        }

        /// <summary>
        /// Adds a one-way connection to the map.
        /// </summary>
        /// <param name="fromLocation"></param>
        /// <param name="toLocation"></param>
        /// <param name="distance"></param>
        public void AddUnidirectionalLink(string fromLocation, string toLocation,
                double distance) 
        {
            this.links.Set(fromLocation, toLocation, distance);
        }

        /// <summary>
        /// Adds a connection which can be traveled in both direction. Internally,
        /// such a connection is represented as two one-way connections.
        /// </summary>
        /// <param name="fromLocation"></param>
        /// <param name="toLocation"></param>
        /// <param name="distance"></param>
        public void AddBidirectionalLink(string fromLocation, string toLocation,
                double distance)
        {
            links.Set(fromLocation, toLocation, distance);
            links.Set(toLocation, fromLocation, distance);
        }

        /// <summary>
        /// Returns a location which is selected by random.
        /// </summary>
        /// <returns></returns>
        public string RandomlyGenerateDestination() 
        {
            return Util.Util.SelectRandomlyFromList(this.GetLocations());
        }

        /// <summary>
        /// Removes a one-way connection.
        /// </summary>
        /// <param name="fromLocation"></param>
        /// <param name="toLocation"></param>
        public void RemoveUnidirectionalLink(string fromLocation, string toLocation) 
        {
            links.Remove(fromLocation, toLocation);
        }

        /// <summary>
        /// Removes the two corresponding one-way connections.
        /// </summary>
        /// <param name="fromLocation"></param>
        /// <param name="toLocation"></param>
        public void RemoveBidirectionalLink(string fromLocation, string toLocation) 
        {
            links.Remove(fromLocation, toLocation);
            links.Remove(toLocation, fromLocation);
        }

        /// <summary>
        /// Defines the position of a location as with respect to an orthogonal
        /// coordinate system.
        /// </summary>
        /// <param name="loc"></param>
        /// <param name="x"></param>
        /// <param name="y"></param>
        public void SetPosition(string loc, double x, double y) {
            locationPositions[loc] = new Point2D(x, y);
        }

        /// <summary>
        /// Defines the position of a location within the map. Using this method, one
        /// location should be selected as reference position (<code>dist=0</code>
        /// and <code>dir=0</code>) and all the other location should be placed
        /// relative to it.
        /// </summary>
        /// <param name="loc">location name</param>
        /// <param name="dist">distance to a reference position</param>
        /// <param name="dir">bearing (compass direction) in which the location is seen from the reference position</param>
        public void SetDistAndDirToRefLocation(string loc, double dist, int dir)
        {
            Point2D coords = new Point2D(
                -Math.Sin(dir * Math.PI / 180.0) * dist, Math.Cos(dir * Math.PI / 180.0) * dist);
            links.AddVertex(loc);
            locationPositions[loc] = coords;
        }

        /// <summary>
        /// Returns the position of the specified location as with respect to an
        /// orthogonal coordinate system.
        /// </summary>
        /// <param name="loc"></param>
        /// <returns></returns>
        public Point2D GetPosition(string loc) {
            return locationPositions[loc];
        }
    }

}
