using System;
using VDS.RDF;

namespace LogicalParticleFilter1
{
    public class GraphWithDef
    {
        public IGraph rdfGraph;
        public IGraph definations;

        public GraphWithDef(IGraph data, IGraph defs)
        {
            rdfGraph = data;
            definations = defs;
        }

        public void AddDefs(SIProlog.Rule rule)
        {
            AddLiteral(rule.head);
            throw new NotImplementedException();
        }

        private void AddLiteral(SIProlog.Term term)
        {
            string predName = term.name;
            int arity = term.partlist.list.Count;
        }

        public void AddRule(SIProlog.Rule rule)
        {
            AddDefs(rule);
            AddData(rule);
        }

        public void AddData(SIProlog.Rule rule)
        {
            throw new NotImplementedException();
        }
    }
}