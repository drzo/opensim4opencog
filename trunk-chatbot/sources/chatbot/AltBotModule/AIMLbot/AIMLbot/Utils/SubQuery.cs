#if false
using System;
using System.Collections.Generic;
using AltAIMLParser;
using RTParser;
using RTParser.Utils;
using RTParser.Variables;

namespace AltAIMLbot.Utils
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
        public string FullPath;

        /// <summary>
        /// The template found from searching the graphmaster brain with the path 
        /// </summary>
        public string Template = string.Empty;

        /// <summary>
        /// The path in the graphmaster brain to find the Template 
        /// </summary>
        public string TemplatePath = string.Empty;

        /// <summary>
        /// If the raw input matches a wildcard then this attribute will contain the block of 
        /// text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<string> InputStar
        {
            get { return Stars["pattern"]; }
        }

        /// <summary>
        /// If the "that" part of the normalized path contains a wildcard then this attribute 
        /// will contain the block of text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<string> ThatStar
        {
            get { return Stars["that"]; }
        }

        public Dictionary<string, List<string>> Stars = new Dictionary<string, List<string>>();

        /// <summary>
        /// If the "topic" part of the normalized path contains a wildcard then this attribute 
        /// will contain the block of text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<string> TopicStar
        {
            get { return Stars["topic"]; }
        }
        public List<string> StateStar
        {
            get { return Stars["state"]; }
        }
        //public List<string> StateStar2 = new List<string>();

        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="fullPath">The path that this query relates to</param>
        public SubQuery(string fullPath, Request request)
        {
            this.FullPath = fullPath;
            this.Request = request;
            foreach (string s in GraphMaster.StarTypes)
            {
                Stars[s] = new List<string>();
            }
        }

        public AltBot TargetBot
        {
            get { return Request.TargetBot;  }
        }

        public SettingsDictionary TargetSettings
        {
            get { return Request.TargetSettings; }
        }

        public Request Request;

        public List<string> GetStars(string s)
        {
            s = s.ToLower();
            if (s == "input") s = "pattern";
            return Stars[s.ToLower()];
        }

        public string ReduceStarAttribute<T>(IConvertible arg)
        {
            throw new NotImplementedException();
        }

        public bool UseDictionaryForSet(ISettingsDictionary dictionary)
        {
            throw new NotImplementedException();
        }
    }
}
#endif