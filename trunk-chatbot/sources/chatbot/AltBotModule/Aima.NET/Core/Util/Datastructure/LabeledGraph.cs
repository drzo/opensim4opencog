using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
    /// <summary>
    /// Represents a directed labeled graph. Vertices are represented by their unique
    /// labels and labeled edges by means of nested hashtables. Variant of class
    /// <see cref="Table{TRowHeader,TColumnHeader,TValue}"/> . This version is more dynamic, it requires no
    /// initialization and can add new items whenever needed.
    /// </summary>
    /// <typeparam name="TVertexLabel"></typeparam>
    /// <typeparam name="TEdgeLabel"></typeparam>
    public class LabeledGraph<TVertexLabel, TEdgeLabel>
    {

        /// <summary>
        /// Lookup for edge label information. Contains an entry for every vertex
        /// label.
        /// </summary>
        private readonly Dictionary<TVertexLabel, Dictionary<TVertexLabel, TEdgeLabel>> globalEdgeLookup;
        
        /// <summary>
        /// List of the labels of all vertices within the graph.
        /// </summary>
        private readonly IList<TVertexLabel> vertexLabels;

        /// <summary>
        /// Creates a new empty graph.
        /// </summary>
        public LabeledGraph() {
            globalEdgeLookup = new Dictionary<TVertexLabel, Dictionary<TVertexLabel, TEdgeLabel>>();
            vertexLabels = new List<TVertexLabel>();
        }

        /// <summary>
        /// Adds a new vertex to the graph if it is not already present.
        /// </summary>
        /// <param name="v"></param>
        public void AddVertex(TVertexLabel v) {
            this.CheckForNewVertex(v);
        }

        /// <summary>
        /// Adds a directed labeled edge to the graph. The end points of the edge are
        /// specified by vertex labels. New vertices are automatically identified and
        /// added to the graph.
        /// </summary>
        /// <param name="from"></param>
        /// <param name="to"></param>
        /// <param name="el"></param>
        public void Set(TVertexLabel from, TVertexLabel to, TEdgeLabel el) {
            Dictionary<TVertexLabel, TEdgeLabel> localEdgeLookup = this.CheckForNewVertex(from);
            localEdgeLookup[to] = el;
            this.CheckForNewVertex(to);
        }

        /// <summary>
        /// Handles new vertices.
        /// </summary>
        /// <param name="v"></param>
        /// <returns></returns>
        private Dictionary<TVertexLabel, TEdgeLabel> CheckForNewVertex(
                TVertexLabel v)
        {
            var result = this.globalEdgeLookup[v];
            if (result == null)
            {
                result = new Dictionary<TVertexLabel, TEdgeLabel>();
                this.globalEdgeLookup[v] = result;
                this.vertexLabels.Add(v);
            }
            return result;
        }

        /// <summary>
        /// Removes an edge from the graph.
        /// </summary>
        /// <param name="from"></param>
        /// <param name="to"></param>
        public void Remove(TVertexLabel from, TVertexLabel to) {
            Dictionary<TVertexLabel, TEdgeLabel> localEdgeLookup = globalEdgeLookup[from];
            if (localEdgeLookup != null)
                localEdgeLookup.Remove(to);
        }

        /// <summary>
        /// Returns the label of the edge between the specified vertices and null if
        /// there is no edge between them.
        /// </summary>
        /// <param name="from"></param>
        /// <param name="to"></param>
        /// <returns></returns>
        public TEdgeLabel Get(TVertexLabel from, TVertexLabel to) {
            Dictionary<TVertexLabel, TEdgeLabel> localEdgeLookup = globalEdgeLookup[from];

            // TODO: java version used to have check for null below. Is that needed?
            // return localEdgeLookup == null ? null : localEdgeLookup[to];
            return localEdgeLookup[to];
        }

        /// <summary>
        /// Returns the labels of those vertices which can be obtained by following
        /// the edges starting at the specified vertex.
        /// </summary>
        /// <param name="v"></param>
        /// <returns></returns>
        public IList<TVertexLabel> GetSuccessors(TVertexLabel v) 
        {
            IList<TVertexLabel> result = new List<TVertexLabel>();
            Dictionary<TVertexLabel, TEdgeLabel> localEdgeLookup = globalEdgeLookup[v];
            if (localEdgeLookup != null)
                result = localEdgeLookup.Keys.ToList();
            return result;
        }

        /// <summary>
        /// Returns the labels of all vertices within the graph.
        /// </summary>
        /// <returns></returns>
        public IList<TVertexLabel> GetVertexLabels() {
            return vertexLabels;
        }

        /// <summary>
        /// Checks whether the given label is the label of one of the vertices.
        /// </summary>
        /// <param name="v"></param>
        /// <returns></returns>
        public bool IsVertexLabel(TVertexLabel v) {
            return globalEdgeLookup[v] != null;
        }

        /// <summary>
        /// Removes all vertices and all edges from the graph.
        /// </summary>
        public void Clear() {
            vertexLabels.Clear();
            globalEdgeLookup.Clear();
        }
    }

}
