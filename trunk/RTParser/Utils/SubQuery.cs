using System;
using System.Collections;
using System.Collections.Generic;
using AIMLbot;
using UPath = RTParser.Unifiable;


namespace RTParser.Utils
{
    /// <summary>
    /// A container class for holding wildcard matches encountered during an individual path's 
    /// interrogation of the graphmaster.
    /// </summary>
    [Serializable]
    public class SubQuery : ISettingsDictionary
    {
        #region Attributes

        public GraphMaster Graph = null;
        /// <summary>
        /// The path that this query relates to
        /// </summary>
        public string FullPath;

        /// <summary>
        /// The template found from searching the graphmaster brain with the path 
        /// </summary>
        public UList Templates = new UList();

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

        public List<Unifiable> Flags = new List<Unifiable>();

        #endregion

        public override string ToString()
        {
            string s = string.Format("SubQuery '{0}' I={1} TH={2} TP={3} G={4} TC={5}",
                                     FullPath, InputStar.Count, ThatStar.Count, TopicStar.Count,
                                     GuardStar.Count, Templates == null ? 0 : Templates.Count);
            foreach (var path in Templates)
            {
                s += "\r\n t: " + path;
            }
            return s + " \r\n   Result: " + Result ?? "-no-result-";
        }

        public Result Result;
        public Request Request;
        public TemplateInfo CurrentTemplate;
        public Node Pattern;
        public QueryList TopLevel;

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="fullPath">The path that this query relates to</param>
        public SubQuery(UPath fullPath, Result res, Request request)
        {
            Result = res;
            Request = request;
            Graph = request.Graph;
            this.FullPath = fullPath;
        }


        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
            return Request.Predicates.removeSetting(name);
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, Unifiable value)
        {
            return Request.Predicates.updateSetting(name, value);
        }

        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        public bool containsSettingCalled(string name)
        {
            var value = grabSetting(name);
            return !Unifiable.IsNullOrEmpty(value);
        }

        public bool containsLocalCalled(string name)
        {
            return Request.Predicates.containsLocalCalled(name);
        }

        public string NameSpace
        {
            get { return Request.Predicates.NameSpace; }
        }

        public Unifiable grabSetting(string name)
        {
            var v = Request.grabSetting(name);
            name = name.ToLower();
            if (name.Equals("dog") || name.Equals("father"))
            {
                RTPBot.writeDebugLine("dog=" + v);
            }
            return v;
        }


        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        public bool addSetting(string name, Unifiable value)
        {
            return Request.addSetting(name, value);
        }

        public SubQuery CopyOf()
        {
            SubQuery sq = new SubQuery(FullPath, Result, Request);
            sq.InputStar.AddRange(InputStar);
            sq.ThatStar.AddRange(ThatStar);
            sq.TopicStar.AddRange(TopicStar);
            sq.GuardStar.AddRange(GuardStar);
            sq.Flags.AddRange(Flags);
            sq.Graph = Graph;
            return sq;
        }

        public List<Unifiable> GetMatchList(MatchState matchstate)
        {
            switch (matchstate)
            {
                case MatchState.UserInput:
                    return InputStar;
                    break;
                case MatchState.That:
                    return ThatStar;
                    break;
                case MatchState.Topic:
                    return TopicStar;
                    break;
                case MatchState.Flag:
                    return Flags;
                    break;
                default:
                    throw new ArgumentOutOfRangeException("matchstate");
            }
        }
    }

    public class UList : IEnumerable<TemplateInfo>
    {
        public List<TemplateInfo> root = new List<TemplateInfo>();
        public int Count
        {
            get { lock (root) return root.Count; }
        }

        public void Insert(int i, TemplateInfo info)
        {
            lock (root) root.Insert(i, info);

        }

        public void ForEach(Action<TemplateInfo> action)
        {
            lock (root) root.ForEach(action);
        }

        public void Remove(TemplateInfo info)
        {
            lock (root) root.Remove(info);
        }

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        IEnumerator<TemplateInfo> IEnumerable<TemplateInfo>.GetEnumerator()
        {
            return GetRootEnumerator();
        }

        public IEnumerator GetEnumerator()
        {
            return GetRootEnumerator();
        }

        private IEnumerator<TemplateInfo> GetRootEnumerator()
        {
            var next = new List<TemplateInfo>();
            lock (root)
            {
                next.AddRange(root);
            }
            return next.GetEnumerator();
        }

        public void AddRange(UList infos)
        {
            lock (root)
            {
                lock (infos.root)
                {
                    root.AddRange(infos.root);                    
                }
            }
        }
    }
}
