using System.Collections.Generic;

namespace RTParser.Utils
{
    public class QueryList: RequestSettings
    {

        public QueryList(RequestSettings request)
        {
            TheRequest = request;
        }
        public int TemplateCount
        {
            get { return Templates == null ? 0 : Templates.Count; }
        }

        public decimal BindingCount
        {
            get { return Bindings == null ? 0 : Bindings.Count; }
        }

        private List<TemplateInfo> Templates;
        private List<Node> Patterns;
        private List<SubQuery> Bindings;
        public RequestSettings TheRequest;
        public bool Bubble;
        public int PatternCount;
        public bool IsNewType = true;

        public void AddPattern(Node node)
        {
            if (Patterns == null)
            {
                Patterns = new List<Node>();
            }
            PatternCount++;
            Patterns.Add(node);
        }

        public void AddTemplate(TemplateInfo info)
        {
            if (Templates == null) Templates = new List<TemplateInfo>();
            Templates.Add(info);
        }

        public bool ContainsPattern(Node node)
        {
            return Patterns != null && Patterns.Contains(node);
        }

        public void AddBindingSet(SubQuery query)
        {
            if (Bindings == null) Bindings = new List<SubQuery>();
            Bindings.Add(query);
        }

        public IEnumerable<SubQuery> GetBindings()
        {
            return Bindings;
        }
    }
}