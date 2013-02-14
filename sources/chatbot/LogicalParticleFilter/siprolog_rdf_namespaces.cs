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
        public static string ProtocolSep = "://";
        const string rdfDefMT = "rdfGlobalDefsMt";
        internal static NamespaceMapper _sharedNS = new NamespaceMapper();
        public static NamespaceMapper rdfDefNS
        {
            get
            {
                lock (rdfDefMT)
                {
                    if (_sharedNS != null) return _sharedNS;
                    var rdfd = rdfDefinations;
                    if (rdfd != null)
                    {
                        return _sharedNS = (NamespaceMapper)rdfd.NamespaceMap;
                    }
                    return _sharedNS;
                }
            }
        }

        internal static readonly FirstUse<LiveCallGraph> _rdfDefinations = new Func<LiveCallGraph>(() => CurrentProlog.NewGraph(rdfDefMT, RoboKindURI, true, true));
        internal static LiveCallGraph rdfDefinations
        {
            get
            {
                return _rdfDefinations.Value;
            }
        }
        public const string TripleName = "triple";
        internal static PNode rdfDefSync;
        public CIDictionary<string, PNode> GraphForMT = new CIDictionary<string, PNode>(new KeyCase(NormalizeKBName));
        public CIDictionary<string, Graph> RdfGraphForURI = new CIDictionary<string, Graph>(new KeyCase(NormalizeURIKBName));

        public static string NormalizeKBName(object arg)
        {
            string retVal = KeyCase.NormalizeKeyLowerCaseNoFileExt(arg);
            int retValLength = retVal.Length;
            if (retValLength > 3)
            {
                if (retVal.EndsWith("mt") || retVal.EndsWith("kb")) retVal = retVal.Substring(0, retValLength - 2);
            }
            return retVal;
        }
        public static string NormalizeURIKBName(object arg)
        {
            string retVal = NormalizeKBName(arg);
            retVal = retVal.TrimEnd('/', '#');
            retVal = NormalizeKBName(retVal);
            retVal = retVal.TrimEnd('/', '#');
            foreach (char c in KeyCase.RegexMarkers)
            {
                retVal = retVal.Replace("" + c, "_");
            }
            return retVal;
        }

        public static bool SameKBName(string kb1, string kb2)
        {
            return NormalizeKBName(kb1) == NormalizeKBName(kb2);
        }

        public static string RoboKindURI = "http://" + CogbotServerWithPort + "/onto/robokind#";
        public static string RoboKindMtURI = "http://" + CogbotServerWithPort + "/onto/rkmt/";
        public static string RoboKindPrefix = "robokind";
        public static string RoboKindPrefixPrepend = RoboKindPrefix + ":";
        private static readonly CIDictionary<string, PredicateProperty> SharedGlobalPredDefs = new CIDictionary<string, PredicateProperty>(KeyCase.Default);
        public static bool SharedGlobalPredDefsDirty = false;
        private static CIDictionary<string, RDFArgSpec> SharedGlobalArgTypeDefs = new CIDictionary<string, RDFArgSpec>(KeyCase.Default);

        private void defineRDFExtensions()
        {
            Options.InternUris = true;
            Options.LiteralValueNormalization = true;
            Options.LiteralEqualityMode = LiteralEqualityMode.Loose;
            if (DLRConsole.IsOnMonoUnix)
            {
                RdfDeveloperSanityChecks = 0;
                DontRDFSync = true;
            }
            if (GlobalSharedSettings.IsRdfServer)
            {
                RdfDeveloperSanityChecks = 0;
                DontRDFSync = false;
               
            }
            RdfDeveloperSanityChecks = 0; //KHC test
            var uriAgainIs = UriFactory.Create(RoboKindURI);
            var rdfDefinations = SIProlog.rdfDefinations;
            rdfDefinations.BaseUri = uriAgainIs;
            lock (rdfDefNS) LoadGraphPrefixes(rdfDefNS);
            rdfDefinations.BaseUri = uriAgainIs;
            rdfDefSync =
                rdfDefSync ??
                //FindKB(rdfDefMT) ??
                new PNode(rdfDefMT, this, rdfDefinations);
            rdfDefinations.PrologKB = rdfDefSync;
            forReaderTripleStoreGraph.BaseUri = uriAgainIs;
            bool newlyCreated;
            var rdfKB = FindOrCreateKB_unlocked_Actual(rdfDefMT, out newlyCreated);
           // rdfKB.SourceKind = ContentBackingStore.RdfMemory;
            rdfDefinations.PrologKB = rdfDefSync;
            rdfDefSync.IncludeRDFUri(new FileInfo("aiml/shared_ke/PrologToRDFConversion.owl").FullName);
            loadKEText(rdfDefMT, FromStream("aiml/shared_ke/argdefs.txt"), false);
        }

        private static void EnsureReaderNamespaces(IGraph graph)
        {
            lock (graph)
            {
                INamespaceMapper nm = graph.NamespaceMap;
                lock (nm) if (!nm.HasNamespace(RoboKindPrefix)) 
                    lock (rdfDefNS) nm.Import(rdfDefNS);
                lock (nm) EnsureBaseURIMapped(graph, nm);
            }
        }

        /// <summary>
        /// Must lock (nm) and probably graph or it may throw a InvalidOperationException
        /// </summary>
        /// <param name="graph"></param>
        /// <param name="nm"></param>
        private static void EnsureBaseURIMapped(IGraph graph, INamespaceMapper nm)
        {      
            bool hsBlank = nm.HasNamespace("");
            var BaseUri = graph.BaseUri;
            if (!hsBlank)
            {
                if (BaseUri != null)
                {
                    //nm.AddNamespace("", BaseUri);
                    return;
                }
                var newIdea = UriFactory.Create(RoboKindURI);
                graph.BaseUri = newIdea;
                //nm.AddNamespace("", newIdea);
                return;

            }
            Uri previosuBlankURI = nm.GetNamespaceUri("");
            var previousBlank = previosuBlankURI.AbsoluteUri;
            if (BaseUri != null)
            {
                if (previousBlank != BaseUri.AbsoluteUri) {} ;//nm.AddNamespace("", BaseUri);
                return;
            }
            var newIdea2 = UriFactory.Create(RoboKindURI);
            //nm.AddNamespace("", newIdea2);
        }
        public static void EnsureGraphPrefixes(IGraph graph)
        {
            var nm = graph.NamespaceMap;
            if (nm.HasNamespace(RoboKindPrefix))
            {
                return;
            }
            EnsureReaderNamespaces(graph);
        }

        public static void LoadGraphPrefixes(INamespaceMapper nm)
        {
            if (nm.HasNamespace(RoboKindPrefix))
            {
                return;
            }
            string s =
                @"
@prefix : <#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix daml: <http://www.daml.org/2001/03/daml+oil#> .
@prefix log: <http://www.w3.org/2000/10/swap/log.n3#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix virtrdf: <http://www.openlinksw.com/schemas/virtrdf#> .
@prefix sswap: <http://sswapmeet.sswap.info/sswap/#> .
@prefix sioc: <http://rdfs.org/sioc/ns#> .
@prefix sioct: <http://rdfs.org/sioc/types#> .
@prefix atom: <http://atomowl.org/ontologies/atomrdf#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix geo: <http://www.w3.org/2003/01/geo/wgs84_pos#> .
@prefix wikiont: <http://sw.deri.org/2005/04/wikipedia/wikiont.owl#> .
@prefix aowl: <http://atomowl.org/ontologies/atomrdf#> .
@prefix v: <http://www.openlinksw.com/schemas/drupal_v#> .
@prefix sd: <http://www.w3.org/ns/sparql-service-description#> .
@prefix dbpprop: <http://dbpedia.org/property/> .
@prefix dbpedia-owl:	<http://dbpedia.org/ontology/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix grddl: <http://www.w3.org/2003/g/data-view#> .
@prefix xml: <http://www.w3.org/XML/1998/namespace> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix dnr: <http://www.dotnetrdf.org/configuration#> .
@prefix dotnetrdf: <http://www.dotnetrdf.org/configuration#> .
@prefix dc: <http://purl.org/dc/elements/1.1/>.
@prefix dcterms: <http://www.purl.org/dc/terms/>.
@prefix vann: <http://purl.org/vocab/vann/>.
@prefix vs: <http://www.w3.org/2003/06/sw-vocab-status/ns#>.
@prefix fmt: <http://www.w3.org/ns/formats/>.
@prefix siprolog: <http://" + CogbotServerWithPort + @"/siprolog#> .
@prefix robokind: <" +
                RoboKindURI + @"> .
";

            string ss =
                @"
fn http://www.w3.org/2005/xpath-functions
gmlxbt http://www.opengis.net/gml/3.3/xbt
sch http://www.ascc.net/xml/schematron
# gml http://www.opengis.net/gml/3.2
gml http://www.opengis.net/gml/_
gmd http://www.isotc211.org/2005/gmd
xlink http://www.w3.org/1999/xlink
xsl  http://www.w3.org/1999/XSL/Transform 
rdf  http://www.w3.org/1999/02/22-rdf-syntax-ns# 
p3q  http://www.w3.org/2004/01/rdxh/p3q-ns-example 
p3qr http://www.example.org/P3Q-rdf# 
p3dr http://www.example.org/TR/P3P/base# 
ont  http://www.daml.org/2001/03/daml+oil# 
s http://schema.org/
xsd http://www.w3.org/2001/XMLSchema#
eco http://www.ebusiness-unibw.org/ontologies/eclass/5.1.4/
gr http://purl.org/goodrelations/v1#
dc http://purl.org/dc/elements/1.1/
ao	http://purl.org/ao/core/
aoa	http://purl.org/ao/annotea/
aof	http://purl.org/ao/foaf/
aold	http://biotea.ws/ontologies/aold/
aos	http://purl.org/ao/selectors/
aot	http://purl.org/ao/types/
bibo	http://purl.org/ontology/bibo/
bif	bif:
bio2rdf_mesh	http://bio2rdf.org/ns/mesh#
bio2rdf_ns	http://bio2rdf.org/ns/bio2rdf#
chebi	http://purl.obolibrary.org/obo/CHEBI_
cnt	http://www.w3.org/2011/content#
dawgt	http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#
dbpedia	http://dbpedia.org/resource/
dbpprop	http://dbpedia.org/property/
dc	http://purl.org/dc/elements/1.1/
dcterms	http://purl.org/dc/terms/
doco	http://purl.org/spar/doco/
fma	http://purl.org/obo/owl/FMA#FMA_
fn	http://www.w3.org/2005/xpath-functions/#
foaf	http://xmlns.com/foaf/0.1/
geo	http://www.w3.org/2003/01/geo/wgs84_pos#
go	http://purl.org/obo/owl/GO#GO_
gw_property	http://genewikiplus.org/wiki/Special:URIResolver/Property-3A
gw_wiki	http://genewikiplus.org/wiki/Special:URIResolver/
icd9	http://purl.bioontology.org/ontology/ICD9-9/
math	http://www.w3.org/2000/10/swap/math#
mddb	http://purl.bioontology.org/ontology/MDDB/
meddra	http://purl.bioontology.org/ontology/MDR/
medline	http://purl.bioontology.org/ontology/MEDLINEPLUS/
mesh	http://purl.org/commons/record/mesh/
mf	http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#
mged	http://mged.sourceforge.net/ontologies/MGEDOntology.owl#
ncbitaxon	http://purl.org/obo/owl/NCBITaxon#NCBITaxon_
ncithesaurus	http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#
nddf	http://purl.bioontology.org/ontology/NDDF/
ndfrt	http://purl.bioontology.org/ontology/NDFRT/
obi	http://purl.obolibrary.org/obo/OBI_
obo	http://www.geneontology.org/formats/oboInOwl#
omim	http://purl.bioontology.org/ontology/OMIM/
owl	http://www.w3.org/2002/07/owl#
pav	http://purl.org/swan/pav/provenance/
po	http://purl.bioontology.org/ontology/PO/
product	http://www.buy.com/rss/module/productV2/
protseq	http://purl.org/science/protein/bysequence/
prov	http://www.w3.org/ns/prov#
pw	http://purl.org/obo/owl/PW#PW_
rdf	http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfa	http://www.w3.org/ns/rdfa#
rdfdf	http://www.openlinksw.com/virtrdf-data-formats#
rdfs	http://www.w3.org/2000/01/rdf-schema#
sc	http://purl.org/science/owl/sciencecommons/
scovo	http://purl.org/NET/scovo#
sioc	http://rdfs.org/sioc/ns#
skos	http://www.w3.org/2004/02/skos/core#
snomed	http://purl.bioontology.org/ontology/SNOMEDCT/
sql	sql:
swivt	http://semantic-mediawiki.org/swivt/1.0#
symptom	http://purl.org/obo/owl/SYMP#SYMP_
taxonomy	http://www.uniprot.org/taxonomy/
umls	http://berkeleybop.org/obo/UMLS:
uniprot	http://purl.uniprot.org/core/
vcard	http://www.w3.org/2001/vcard-rdf/3.0#
vcard2006	http://www.w3.org/2006/vcard/ns#
virtcxml	http://www.openlinksw.com/schemas/virtcxml#
virtrdf	http://www.openlinksw.com/schemas/virtrdf#
void	http://rdfs.org/ns/void#
xf	http://www.w3.org/2004/07/xpath-functions
xml	http://www.w3.org/XML/1998/namespace
xsd	http://www.w3.org/2001/XMLSchema#
xsl10	http://www.w3.org/XSL/Transform/1.0
xsl1999	http://www.w3.org/1999/XSL/Transform
xslwd	http://www.w3.org/TR/WD-xsl
xsp	http://www.owl-ontologies.com/2005/08/07/xsp.owl#
yago	http://dbpedia.org/class/yago/
";
            ss = s + ss;
            foreach (string s00 in ss.Split('\n', '\r'))
            {
                if (String.IsNullOrEmpty(s00)) continue;
                var s0 = s00.Replace('\t', ' ').Trim();
                if (s0.StartsWith("#")) continue;
                if (s0.StartsWith("@prefix "))
                {
                    s0 = s0.Substring("@prefix ".Length).TrimStart();
                    s0 = s0.TrimEnd('.', ' ');
                }
                while (s0.Contains("  ")) s0 = s0.Replace("  ", " ");
                if (String.IsNullOrEmpty(s0)) continue;
                int spc = s0.IndexOf(' ');
                string prefix = s0.Substring(0, spc).Trim().TrimEnd(' ', ':');
                string uri = s0.Substring(spc).Trim();
                if (uri.StartsWith("<") && uri.EndsWith(">"))
                {
                    uri = uri.Substring(1, uri.Length - 2).Trim();
                }
                if (uri == "#")
                {
                    continue;
                  //  uri = graph.BaseUri.AbsoluteUri;
                }
                if (String.IsNullOrEmpty(prefix))
                {
                    continue;
                    prefix = ":";//"Warn("adding null prefix " + uri);
                }
                if (nm.HasNamespace(prefix))
                {
                    var prev = nm.GetNamespaceUri(prefix).ToString();
                    if (prev != uri)
                    {
                        if (uri.Length < prev.Length)
                        {
                            continue;
                        }
                    }
                }
                nm.AddNamespace(prefix, new Uri(uri));
            }
        }

        public void RegisterHomeGraph(String nsURI, Graph newGraph, bool forced)
        {
            lock (RdfGraphForURI)
            {
                Graph oldG;
                if (RdfGraphForURI.TryGetValue(nsURI, out oldG))
                {
                    if (ReferenceEquals(oldG, newGraph)) return;
                    if (forced)
                    {
                        Warn("Rereging " + nsURI);
                    }
                    else
                    {
                        return;
                    }
                }
                RdfGraphForURI[nsURI] = newGraph;
            }
        }
        public static Graph FindGraph(string prefix)
        {
            var RdfGraphForURI = CurrentProlog.RdfGraphForURI;
            Graph graph;
            lock (RdfGraphForURI)
            {
                if (RdfGraphForURI.TryGetValue(prefix, out graph)) return graph;
            }
            return null;
        }
    }
}