using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using AIMLbot;
using RTParser.Variables;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;


namespace RTParser.Utils
{
    /// <summary>
    /// A container class for holding wildcard matches encountered during an individual path's 
    /// interrogation of the graphmaster.
    /// </summary>
    [Serializable]
    public class SubQuery : ISettingsDictionary
    {
        public bool IsTraced { get; set; }
        public IEnumerable<string> SettingNames(int depth)
        {
            //get 
            { return Request.TargetSettings.SettingNames(depth); }
        }

        #region Attributes

        #region Overrides of QuerySettings

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        public GraphMaster Graph { get; set; }
        
        #endregion

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
            string s = string.Format("\nPATTERN='{0}' I={1} TH={2} TP={3} G={4} TC={5}\nINPUT = '{6}'",
                                     Pattern, InputStar.Count, ThatStar.Count, TopicStar.Count,
                                     GuardStar.Count, Templates == null ? 0 : Templates.Count,
                                     FullPath);
            if (Templates != null)
                foreach (var path in Templates)
                {
                    s += "\r\n t: " + path;
                }
            s += " \r\n";
            Result r = Result;
            if (r == null) s += " Result: -no-result- ";
            else s += " result.Count=" + r.OutputSentenceCount;
            Request rq = Request;
            int depth = 0;
            {
                while (rq != null)
                {
                    depth++;
                    rq = rq.ParentRequest;
                }
            }
            s += " depth: " + depth;
            s += " " + Graph;
            return s;
        }

        public Result Result;
        public Request Request;
        public TemplateInfo CurrentTemplate;
        public Node Pattern;
        public QueryList TopLevel;
        private Dictionary<string, AIMLTagHandler> TagHandlers;

        public double GetSucceedReward(string type)
        {
            return 1.0;
        }
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

        public RTPBot RProcessor;

        public Unifiable grabSetting(string name)
        {
            string realName;
            var v = SettingsDictionary.grabSettingDefualt(Request.TargetSettings, name, out realName); 
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

        public AIMLTagHandler GetTagHandler(System.Xml.XmlNode node)
        {
            lock (this)
            {

                string str = node.OuterXml;
                str = AIMLLoader.CleanWhitepaces(str).ToLower();
                AIMLTagHandler handler;
                if (TagHandlers == null)
                {
                    TagHandlers = new Dictionary<string, AIMLTagHandler>();
                }
                else if (TagHandlers.TryGetValue(str, out handler))
                {
                    return handler;
                }
                SubQuery subquery = this;
                RTPBot bot = null;
                User user = null;
                Request request = null;
                Result result = null;
                // if (node.ChildNodes.Count == 0) ;         
                {
                    result = subquery.Result;
                    request = subquery.Request ?? result.request;
                    result = result ?? request.CurrentResult;
                    user = result.user;
                    bot = request.TargetBot;
                }
                handler = bot.GetTagHandler(user, subquery, request, result, node, null);
                TagHandlers[str] = handler;
                return handler;
            }
        }

        public void UndoAll()
        {
             UndoStack.FindUndoAll(this);
        }

        public void AddUndo(ThreadStart undo)
        {
            lock (this)
            {
                UndoStack.GetStackFor(this).AddUndo(undo);
            }
        }

        public UndoStack GetFreshUndoStack()
        {
            lock (this)
            {
                UndoStack.FindUndoAll(this);
                return UndoStack.GetStackFor(this);
            }
        }

        public bool CanUseNode(Node node)
        {
            return TopLevel.CanUseNode(node);
        }
    }
#if _FALSE_
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
#endif
}
