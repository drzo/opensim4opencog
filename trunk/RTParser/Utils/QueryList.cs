using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;

namespace RTParser.Utils
{
    public class QueryList: RequestSettings
    {

        public override string ToString()
        {
            lock (this)
            {
                String s = TheRequest.ToString() + " " + Environment.NewLine;
                var noddes = new List<Node>();
                s += ToString("  Q: ", Bindings);
                s += ToString("  P: ", PatternsUsed);
                s += ToString("  T: ", Templates);
                return s + Environment.NewLine;
            }
        }


        public QueryList(RequestSettings request)
        {
            TheRequest = request;
        }
        public int TemplateCount
        {
            get { return Templates == null ? 0 : Templates.Count; }
        }

        public int PatternCount
        {
            get { return PatternsUsed == null ? 0 : PatternsUsed.Count; }
        }

        private List<TemplateInfo> Templates;
        private List<Node> PatternsUsed;
        private List<SubQuery> Bindings;
        public RequestSettings TheRequest;
        public bool Bubble;
        public bool IsNewType = true;
        public bool IsMaxedOut
        {
            get
            {
                if (TemplateCount >= MaxTemplates)
                {
                    return true;
                }
                if (PatternCount >= MaxPatterns)
                {
                    return true;
                }
                return false;
            }
        }


        private void AddPattern(Node node)
        {
            if (PatternsUsed == null)
            {
                PatternsUsed = new List<Node>();
            }
            PatternsUsed.Add(node);
            CheckConsistent(); 
        }

        private void CheckConsistent()
        {
            if (PatternCount!=0)
            {
                if (PatternCount != Bindings.Count)
                {
                    throw new NotImplementedException();
                }                
            }
        }

        public void AddTemplate(TemplateInfo info)
        {
            if (Templates == null) Templates = new List<TemplateInfo>();
            Templates.Add(info);
        }
        
        public bool ContainsPattern(Node node)
        {
            return PatternsUsed != null && PatternsUsed.Contains(node);
        }

        public void AddBindingSet(SubQuery query)
        {
            if (Bindings == null) Bindings = new List<SubQuery>();
            Bindings.Add(query);
            AddPattern(query.Pattern);
        }

        public IEnumerable<SubQuery> GetBindings()
        {
            return Bindings;
        }

        public static void PrintTemplates(Result templates, RTPBot.OutputDelegate console)
        {
            console("!REQUEST: {0}", templates.TopLevel);
            PrintTemplates(templates.UsedTemplates, console);
            console(" !Result: {0}", templates);
        }

        public static void PrintTemplates(IEnumerable<TemplateInfo> templates, RTPBot.OutputDelegate console)
        {
            if (templates == null) return;
            foreach (var info in templates)
            {
                console(" {0}", CleanWhitepaces(info.CategoryInfo));
            }
        }


        private string ToString(string pre, ICollection c)
        {
            if (c == null || c.Count == 0) return Environment.NewLine;
            string s = "";
            foreach (object node in c)
            {
                s += pre + CleanWhitepaces(node) + Environment.NewLine;
            }
            s += Environment.NewLine;
            return s;
        }
        
        public static string CleanWhitepaces(object info)
        {
            if (info is XmlNode)
            {
                XmlNode n = (XmlNode) info;
                if (n.Name == "template") info = n.ParentNode;
            }
            if (info is TemplateInfo)
            {
                info = ((TemplateInfo)info).CategoryInfo;
            }
            return AIMLLoader.CleanWhitepaces("" + info);
        }
    }
}