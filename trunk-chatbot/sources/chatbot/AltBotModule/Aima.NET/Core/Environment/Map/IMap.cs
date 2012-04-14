using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Provides a general interface for maps.
    /// </summary>
    public interface IMap
    {

        /// <summary>
        /// Returns a list of all locations.
        /// </summary>
        /// <returns> Returns a list of all locations.</returns>
        IList<string> GetLocations();

        /// <summary>
        /// Answers to the question: Where can I get, following one of the
        /// connections starting at the specified location?
        /// </summary>
        /// <param name="fromLocation"></param>
        /// <returns></returns>
        IList<string> GetLocationsLinkedTo(string fromLocation);

        /// <summary>
        /// Returns the travel distance between the two specified locations if they
        /// are linked by a connection and null otherwise.
        /// </summary>
        /// <param name="fromLocation"></param>
        /// <param name="toLocation"></param>
        /// <returns></returns>
        double GetDistance(string fromLocation, string toLocation);

        /// <summary>
        /// Returns the position of the specified location. The position is
        /// represented by two coordinates, e.g. latitude and longitude values.
        /// </summary>
        /// <param name="loc"></param>
        /// <returns></returns>
        Point2D GetPosition(string loc);

        /// <summary>
        /// Returns a location which is selected by random.
        /// </summary>
        /// <returns></returns>
        string RandomlyGenerateDestination();
    }
}
