#define MERGED_RDFSTORE
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Web;
using LogicalParticleFilter1;
using MushDLR223.Utilities;
using VDS.RDF;
using VDS.RDF.Nodes;
using VDS.RDF.Parsing;
using VDS.RDF.Parsing.Contexts;
using VDS.RDF.Parsing.Handlers;
using VDS.RDF.Parsing.Tokens;
using VDS.RDF.Query;
using VDS.RDF.Query.Expressions;
using VDS.RDF.Writing;
using ListOfBindings = System.Collections.Generic.List<System.Collections.Generic.Dictionary<string, LogicalParticleFilter1.SIProlog.Part>>;
using StringWriter=System.IO.StringWriter;
using VDS.RDF.Writing.Formatting;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;

namespace LogicalParticleFilter1
{

    public partial class SIProlog
    {

        public class RdfRules
        {
            ///<summary>
            ///</summary>
            public string _aInfo
            {
                get
                {
                    try
                    {
                        return ToString();
                    }
                    catch (Exception)
                    {
                        return ToString();
                    }
                }
            }

            private bool _requirementsMet = true;
            private HashSet<INode> RequiredAnteceedantVars = new HashSet<INode>();
            public IGraph ContainingGraph;
            public IGraph def
            {
                get
                {
                    return ContainingGraph ?? _graph;
                }
            }
            private INode _ruleNode;
            public List<INode> Subjects = new List<INode>();
            public List<Triple> Requirements = new List<Triple>();
            public List<Triple> Producing = new List<Triple>();
            public List<Triple> Consequences = new List<Triple>();

            IGraph _graph;
            public override string ToString()
            {
                INamespaceMapper graphNamespace = _graph.NamespaceMap;
                StringWriter sw = new StringWriter();
                Graph ig = new Graph();
                ig.NamespaceMap.Import(graphNamespace);
                AssertTriples(ig, false, true);
                sw.WriteLine("# subjs= {0} metreq={1} ", Subjects.Count, RequirementsMet);
                sw.WriteLine("BEGIN: ");
                DumpTriplesPlain(ig.Triples, sw, " {0}", ig);
                sw.WriteLine("EOT.");
                sw.WriteLine("TOTRIPLES: ");
                DumpTriplesPlain(ToTriples, sw, " {0}", ig);
                sw.WriteLine("EOT.");
                //WriteGraph(sw, ig, "rdfs triples", true, false);
                return sw.ToString();
            }

            public RdfRules(IGraph graph)
            {
                _graph = graph;
                EnsureReaderNamespaces(graph);
                //Producing = Requirements;
            }

            public void AddRequirement(Triple triple)
            {
                if (Contains(triple))
                {
                    return;
                }
                // CheckTriple(triple); ;
                _requirementsMet = false;
                Requirements.Add(triple);
            }
            public void AddProducing(Triple triple)
            {
                if (Contains(triple))
                {
                    return;
                }
                if (triple.Subject.NodeType == NodeType.Variable)
                {
                    AddRequirement(triple);
                    return;
                }
                if (triple.Object.NodeType == NodeType.Variable)
                {
                    AddRequirement(triple);
                    return;
                }
                CheckTriple(triple);
                Producing.Add(triple);
            }
            public void AddConsequent(Triple triple)
            {
                if (Contains(triple))
                {
                    return;
                }
                if (triple.Object.NodeType == NodeType.Variable)
                {
                    _requirementsMet = false;
                    RequiredAnteceedantVars.Add(triple.Object);
                }
                if (triple.Subject.NodeType == NodeType.Variable)
                {
                    _requirementsMet = false;
                    RequiredAnteceedantVars.Add(triple.Subject);
                }

                _requirementsMet = false;
                Consequences.Add(triple);
            }

            private bool Contains(Triple triple)
            {
                if (Consequences.Contains(triple)) return true;
                if (Producing.Contains(triple)) return true;
                if (Requirements.Contains(triple)) return true;
                return false;
            }

            public bool RequirementsMet
            {
                get
                {
                    if (_requirementsMet) return true;
                    if (_ruleNode == null)
                    {
                        return ToTriples.Count() == Producing.Count;
                    }
                    if (Requirements.Count == 0) return true;
                    return false;
                }
                set
                {
                    _requirementsMet = value;
                }
            }
            public INode RuleNode
            {
                get
                {
                    if (_ruleNode == null)
                    {
                        ///_ruleNode = def.CreateBlankNode();
                    }
                    return _ruleNode;
                }
                set
                {
                    if (value.NodeType != NodeType.Blank)
                    {
                        Warn("Rule node should be a Bnode.. not " + value);
                    }
                    _ruleNode = value;
                }
            }
            public ICollection<Triple> ToTriples
            {
                get
                {
                    ICollection<Triple> temp = new HashSet<Triple>();
                    if (Requirements.Count > 0)
                    {
                        if (Consequences.Count > 0)
                        {
                            temp.Add(GraphWithDef.CreateImplication(_graph, Requirements, Consequences));
                        }
                        else
                        {
                            if (Producing.Count > 0)
                            {
                                temp.Add(GraphWithDef.CreateImplication(_graph, Requirements, Producing));
                                return temp;
                            }
                            else
                            {
                                Warn("No consequences for requirements");
                                foreach (Triple consequence in Requirements)
                                {
                                    temp.Add(consequence);
                                }
                                return temp;
                            }
                        }
                    }
                    else
                    {
                        bool generateAnteceedant = RequiredAnteceedantVars.Count > 0;
                        if (Consequences.Count > 0)
                        {
                            if (generateAnteceedant)
                            {
                                var p = new List<Triple>(Producing);
                                var c = new List<Triple>(Consequences);
                                if (p.Count == 0)
                                {
                                    foreach (Triple consequence in c)
                                    {
                                        if (ContainsVariable(consequence))
                                        {
                                            p.Add(consequence);
                                        }
                                    }
                                    foreach (Triple consequence in p)
                                    {
                                        c.Remove(consequence);
                                    }
                                }
                                if (p.Count > 0)
                                {
                                    if (c.Count == 0)
                                    {
                                        Warn("cant have Consequent");
                                    }
                                    else
                                    {
                                        temp.Add(GraphWithDef.CreateImplication(_graph, p, c));
                                        foreach (var t in Producing)
                                        {
                                            temp.Add(t);
                                        }
                                        return temp;
                                    }
                                }
                                else
                                {
                                    Warn("cant precondition Consequent");
                                }
                            }
                            else
                            {
                                foreach (Triple consequence in Consequences)
                                {
                                    temp.Add(consequence);
                                }
                            }
                        }
                        else
                        {
                            if (Producing.Count == 0)
                            {
                                return temp;
                            }
                        }
                    }
                    foreach (var t in Producing)
                    {
                        temp.Add(t);
                    }
                    return temp;
                }
            }

            private static bool ContainsVariable(Triple triple)
            {
                if (triple.Object.NodeType == NodeType.Variable)
                {
                    return true;
                }
                if (triple.Subject.NodeType == NodeType.Variable)
                {
                    return true;
                }
                if (triple.Predicate.NodeType == NodeType.Variable)
                {
                    return true;
                }
                return false;
            }

            public string AssertTriples(IGraph kb, bool checkWff, bool saveToKB)
            {
                var ToTriples = this.ToTriples;

                string bad = "";
                if (saveToKB && !RequirementsMet && checkWff)
                {
                    bad = "Meet requirements please! ";
                    saveToKB = false;
                }
                if (checkWff)
                {
                    bad += Check(kb);
                }
                bool wasGood = (string.IsNullOrEmpty(bad));
                if (wasGood && saveToKB)
                {
                    foreach (Triple triple in ToTriples)
                    {
                        bad += rdfGraphAssert(kb, triple, checkWff, true);
                    }
                }
                wasGood = string.IsNullOrEmpty(bad);
                if (wasGood)
                {
                    if (saveToKB)
                    {
                        ContainingGraph = kb;
                    }
                    return "";
                }
                // bad += " " + ToString();
                if (saveToKB)
                {
                    throw ErrorBadOp(bad);
                }
                Warn(bad);
                return bad;
            }
            public string Check(IGraph kb)
            {
                string bad = "";
                foreach (Triple triple in ToTriples)
                {
                    bad += rdfGraphAssert(kb, triple, true, false);
                }
                if (string.IsNullOrEmpty(bad)) return null;
                return bad;
            }

            internal void AddSubject(INode rdf)
            {
                if (rdf == null) return;
                if (!Subjects.Contains(rdf)) Subjects.Add(rdf);
            }

            public void Clear()
            {
                Producing.Clear();
                Consequences.Clear();
                Requirements.Clear();
            }

            public void AddRequirement(INode s, string sp, Part o)
            {
                AddRequirement(MakeTriple(s,
                           GraphWithDef.PredicateToProperty(sp),
                           GraphWithDef.PartToRdf(o, this), false));
            }

            public void IncludeRules(RdfRules sourceRules)
            {
                AddNew(sourceRules.Producing, Producing);
                AddNew(sourceRules.Requirements, Requirements);
                AddNew(sourceRules.Consequences, Consequences);
                AddNew(sourceRules.Subjects, Subjects);
            }

            static private void AddNew<T>(IEnumerable<T> source, ICollection<T> target)
            {
                foreach (T t in source)
                {
                    if (!target.Contains(t)) target.Add(t);
                }
            }
        }

    }
}