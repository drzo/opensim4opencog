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
        public void WriteEnumeration<T>(TextWriter writer, IEnumerable<T> triple, Func<T, object> toString)
        {
            writer.WriteLine("<pre>");
            foreach (T t in triple)
            {
                object os = toString(t);
                if (os == null) continue;
                var ts = os.ToString();
                writer.WriteLine(ts);
            }
            writer.WriteLine("</pre>");
        }

        internal static void WriteGraph(TextWriter writer, IGraph graph, IGraph defs, string named)
        {
            WriteGraph(writer, graph, "Data for " + named, false, true);
            if (graph != defs) WriteGraph(writer, defs, "Defs for " + named, true, true);
        }
        internal static void WriteGraph(TextWriter writer, IGraph graph, string named, bool plain, bool inHtml)
        {
            string printerName = "plain";
            if (plain)
            {
                DumpTriples(graph, writer, named);
                return;
            }
            else
            {
                IRdfWriter n3w;
                StringWriter dtt = new StringWriter();
                string dttToString = "error";
                bool prettyWillWorked = false;

                if (prettyWillWorked)
                {
                    try
                    {
                        n3w = new CompressingTurtleWriter() { DefaultNamespaces = graph.NamespaceMap, PrettyPrintMode = true };
                        n3w.Save(graph, dtt);
                        dttToString = dtt.ToString();
                        prettyWillWorked = true;
                        printerName = n3w.GetType().Name;
                    }
                    catch (Exception e)
                    {
                        dttToString = dtt.ToString();
                        prettyWillWorked = false;
                    }
                }
                if (!prettyWillWorked)
                {
                    n3w = new Notation3Writer() { DefaultNamespaces = graph.NamespaceMap, PrettyPrintMode = true };
                    dtt = new StringWriter();
                    try
                    {
                        lock (graph) n3w.Save(graph, dtt);
                        dttToString = dtt.ToString();
                        printerName = n3w.GetType().Name;
                    }
                    catch (Exception e)
                    {
                        if (inHtml)
                        {
                            writer.WriteLine("<pre><font color='red'>{0} {1} {2}</font></pre>", e.GetType(), e.Message,
                                             e.StackTrace);
                            printerName = "triples";
                            DumpTriples(graph, writer, named);
                        }
                        else
                        {
                            printerName = "plain";
                            DumpTriplesPlain(graph.Triples, writer, "{0}", graph);
                        }
                        return;
                    }
                }
                if (inHtml)
                {
                    writer.WriteLine("<h3>{0} KB {1}</h3>", named, printerName);
                    writer.WriteLine("<pre>");
                }
                dttToString = dttToString.Replace("\r\n", "\n").Replace("\r", "\n");
                foreach (string line in dttToString.Split('\n'))
                {
                    if (String.IsNullOrEmpty(line))
                    {
                        writer.WriteLine();
                        continue;
                    }
                    if (line.StartsWith("@prefix ")) continue;
                    var hline = line;
                    hline = (" " + hline).Replace(" robokind:", " rk:");
                    hline = hline.Replace("<robokind:", "<rk:");
                    if (inHtml) hline = hline.Replace("<", "&lt;").Replace(">", "&gt;");
                    writer.WriteLine(hline);
                }
                if (inHtml)
                {
                    writer.WriteLine("</pre>");
                }
            }
        }

        private static void DumpTriples(IGraph graph, TextWriter writer, string named)
        {
            var trips = graph.Triples;
            writer.WriteLine("<h3>{0} KB Triples {1}</h3>", named, trips.Count);
            writer.WriteLine("<pre>");
            try
            {
                DumpTriplesPlain(trips, writer, "<font color='green'>{0}</font>", graph);
            }
            catch (Exception e)
            {
                Warn(e);
                writer.WriteLine("<font color='red'>{0} {1} {2}</font>", e.GetType(), e.Message, e.StackTrace);
            }
            writer.WriteLine("</pre>");
        }
        public static void DumpTriplesPlain(IEnumerable<Triple> trips, TextWriter writer, string fmt, IGraph from)
        {
            var formatter = new SparqlFormatter/* TurtleW3CFormatter, TurtleFormatter*/(from.NamespaceMap);
            try
            {
                foreach (Triple trip0 in trips)
                {
                    DumpOneTriple(from, trip0, formatter, writer, fmt);
                }
            }
            catch (Exception e)
            {
                Warn(e);
                throw e;
            }
        }

        private static void DumpOneTriple(IGraph from, Triple trip0, ITripleFormatter formatter, TextWriter writer, string fmt)
        {
            Triple trip = trip0;
            if (trip.Graph == null)
            {
                trip = new Triple(trip.Subject, trip.Predicate, trip.Object, from);
                trip.Context = trip0.Context;
            }
            string ts;
            try
            {
                ts = trip.ToString(formatter);
            }
            catch (RdfOutputException)
            {
                ts = trip.ToString();
            }
            var hline = (" " + ts).Replace(" robokind:", " rk:");
            hline = hline.Replace("<robokind:", "<rk:");
            writer.WriteLine(fmt, hline);
        }
    }

}
