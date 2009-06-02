using System;
using System.Collections.Generic;

namespace RTParser.Utils
{
    /// <summary>
    /// A container class for holding wildcard matches encountered during an individual path's 
    /// interrogation of the graphmaster.
    /// </summary>
    public class SubQuery
    {
        #region Attributes
        /// <summary>
        /// The path that this query relates to
        /// </summary>
        public Unifiable FullPath;

        /// <summary>
        /// The template found from searching the graphmaster brain with the path 
        /// </summary>
        public List<Template> Template = new List<Template>();

        /// <summary>
        /// If the raw input matches a wildcard then this attribute will contain the block of 
        /// text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<Unifiable> InputStar = new List<Unifiable>();

        /// <summary>
        /// If the "that" part of the normalized path contains a wildcard then this attribute 
        /// will contain the block of text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<Unifiable> ThatStar = new List<Unifiable>();

        /// <summary>
        /// If the "topic" part of the normalized path contains a wildcard then this attribute 
        /// will contain the block of text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<Unifiable> TopicStar = new List<Unifiable>();
             
        public List<Unifiable> GuardStar = new List<Unifiable>();

        #endregion

        public Result Result;
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="fullPath">The path that this query relates to</param>
        public SubQuery(Unifiable fullPath, Result req)
        {
            Result = req;
            this.FullPath = fullPath;
        }
    }
}
