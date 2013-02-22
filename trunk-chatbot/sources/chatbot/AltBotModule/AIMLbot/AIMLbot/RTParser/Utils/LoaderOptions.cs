using System;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.Xml;
using AIMLbot;
using AltAIMLParser;
using AltAIMLbot.Variables;
using LogicalParticleFilter1;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using MasterRequest = AltAIMLbot.Utils.Request;


//using CategoryInfo = RTParser.Utils.TemplateInfo;

namespace AltAIMLbot.Utils
{
    public class LoaderOptions: QuerySettings
    {
        public string graphName = "*";
        public string topicName = "*";
        public string stateNamePre = "*";
        public string stateNamePost = "*";
        public string currentThat = "*";
        public string currentFlags = "*";
        public string currentInput = "*";

       
        public List<ConversationCondition> AdditionalPreconditions;

        // below is more "loader"ish
        //public LoaderOptions prevoious;
        public static readonly string MISSING_FILE = "*";
        public string CurrentFilename = MISSING_FILE;
        public string CurrentlyLoadingFrom = null;

        public List<CategoryInfo> CategoryInfos;
        public bool DebugFiles;
        public bool Recurse;
        public bool NeedsLoaderLock;
        public bool SearchForGuard = false;

        // simply a cache
        protected GraphMaster _sGraph;
        public void WithAttributes(XmlNode node, ref string defaultElement, Action action)
        {
            if (node.Attributes == null)
            {
                action();
                return;
            }
            var ts = WithAttributesForUnwind(node, ref defaultElement, this, null);
            try
            {
                action();
            }
            finally
            {
                ts();
            }
        }
        public Action WithAttributesForUnwind(XmlNode node, ref string defaultElement, List<XmlAttribute> consumed)
        {
            return WithAttributesForUnwind(node, ref defaultElement, this, consumed);
        }
        public Action WithAttributesForUnwind(XmlNode node, ref string defaultElement, object saveOn, List<XmlAttribute> consumed)
        {
            if (node.Attributes == null)
            {
                return StaticAIMLUtils.DoNothing;
            }
            bool defaultElementResore = false;
            string prevDefaultEle = null;
            var restore = CopyOptions();
            var current = this;
            bool isNamed = false;
            foreach (XmlAttribute Attrib in node.Attributes)
            {
                Type so = saveOn.GetType();
                bool wasName = false;
                string name = Attrib.Name;
                if (name == "name")
                {
                    wasName = true;
                    isNamed = true;
                    name = node.Name;
                    if (string.IsNullOrEmpty(defaultElement))
                    {
                        AltBot.writeDebugLine("WARN Cant on default attribute " + Attrib);
                    }
                    else
                    {
                        if (consumed != null) consumed.Add(Attrib);
                    }
                    prevDefaultEle = defaultElement;
                    defaultElementResore = true;
                    defaultElement = node.Attributes["name"].Value;
                }
                var ic = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.IgnoreCase;
                FieldInfo fi = so.GetField(name, ic) ??
                               so.GetField(name + "Name", ic) ??
                               so.GetField("current" + name, ic) ??
                               so.GetField(name + "NamePre", ic) ??
                               so.GetField(name + "NamePost", ic);
                if (name == "version" || name=="id") continue;

                if (fi != null)
                {
                    string val = Attrib.Value;
                    fi.SetValue(saveOn, val);
                    if (consumed != null)
                    {
                        consumed.Add(Attrib);
                    }
                    if (wasName)
                    {
                        defaultElementResore = false;
                    }
                    continue;
                }
                if (wasName)
                {
                    AltBot.writeDebugLine("WARN Cant find default element " + Attrib);
                    continue;
                }
                ///AltBot.writeDebugLine("WARN `ibute " + Attrib);
            }
            return () =>
            {
                if (defaultElementResore && !isNamed)
                {
                    AltBot.ConsoleRobot.RaiseError("Cant reset default element! ");
                    //defaultElement = prevDefaultEle;
                }
                CopyFromTo(restore, this);
            };
        }
        public LoaderOptions(User dict)
            : base(null)
        {
            this.graphName = StarNulls(dict.StartGraphName);
            this.topicName = StarNulls(dict.Topic);
            this.currentThat = StarNulls(dict.That);
            this.currentInput = StarNulls(dict.JustSaid);
            this.CurrentlyLoadingFrom = StarNulls(dict.Predicates["cwd"]);
            this.stateNamePre = StarNulls(dict.Predicates["state"]);
          this.currentFlags = StarNulls(dict.Predicates["flags"]);
            //CategoryInfos = new List<CategoryInfo>();
        }

        static private string StarNulls(string startGraphName)
        {
            if (IsNullOrEmpty(startGraphName))
            {
                return "*";
            }
            return startGraphName;
        }

        protected LoaderOptions()
            : base(null)
        {
            //CategoryInfos = new List<CategoryInfo>();
        }

        public void SetLoaderOptions(LoaderOptions copyFrom)
        {
            if (copyFrom != default(LoaderOptions))
            {
                CopyFromTo(copyFrom, this);
            }
        }

        static public void CopyFromTo(LoaderOptions from, LoaderOptions to)
        {
            foreach (FieldInfo fi in typeof(LoaderOptions).GetFields())
            {
                if (!typeof(IComparable<string>).IsAssignableFrom(fi.FieldType)) continue;
                fi.SetValue(to, StarNulls("" + (fi.GetValue(from) ?? "*")));
            }
        }

        public GraphMaster Graph
        {
            get
            {
                if (_sGraph == null)
                {
                    _sGraph = AltBot.FindGlobalGraph(graphName);
                }
                return _sGraph;
            }
            set
            {
                _sGraph = value;
                if (value != null)
                {
                    graphName = value.ScriptingName;
                }
            }
        }

        //public Request TheRequest = this;

        public string generateCPath(AltBot bot)
        {
            return AIMLLoader.generateCPath(graphName, currentInput, currentThat, currentFlags, topicName, stateNamePre,
                                            stateNamePost,
                                            true, null, bot);
        }

        override public string ToString()
        {
            return GlobalSharedSettings.StructToString(this);
        }


        protected LoaderOptions CopyOptions()
        {
            var loaderOpts = new LoaderOptions();
            CopyFromTo(this, loaderOpts);
            return loaderOpts;
        }

        public void SuggestPath(string path)
        {
            if (NullOrStar(path)) return;
            if (NullOrStar(CurrentlyLoadingFrom))
            {
                CurrentlyLoadingFrom = path;
            }
            if (NullOrStar(CurrentFilename))
            {
                CurrentFilename = path;
            }
            else
            {
                if (HostSystem.FileExists(path))
                {
                    CurrentFilename = path;
                }
            }
        }

        internal string BestOf(string befor, string after)
        {
            if (NullOrStar(befor))
            {
                if (NullOrStar(after))
                {
                    return "*";
                }
                return after;
            }

            if (!NullOrStar(after)) return after;
            return befor;
        }


        private bool NullOrStar(string path)
        {
            return string.IsNullOrEmpty(path) || path == "*";
        }

        public override string StartGraphName
        {
            get { return graphName; }
            set { graphName = value; }
        }
    }
}