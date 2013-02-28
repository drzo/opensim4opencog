#define MERGED_RDFSTORE
using System;
using System.Collections.Generic;
using System.Collections;
using System.Data;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.Reflection;
using Mono.CSharp;
using MushDLR223.Utilities;
using VDS.RDF;
using VDS.RDF.Parsing;
using VDS.RDF.Query;
using VDS.RDF.Writing.Formatting;
using VDS.RDF.Writing;
using VDS.RDF.Nodes;
using StringWriter = System.IO.StringWriter;
//using TermList = LogicalParticleFilter1.TermListImpl;
//using TermList = System.Collections.Generic.List<LogicalParticleFilter1.SIProlog.Part>;///LogicalParticleFilter1.SIProlog.PartListImpl;
//using PartList = System.Collections.Generic.List<LogicalParticleFilter1.SIProlog.Part>;///LogicalParticleFilter1.SIProlog.PartListImpl;

using TermList = LogicalParticleFilter1.SIProlog.PartListImpl;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;
#if MERGED_RDFSTORE
using GraphWithDef = LogicalParticleFilter1.SIProlog.PNode;
#endif

using System.Threading;
//using GraphWithDef = LogicalParticleFilter1.SIProlog.;
//using ProveResult = LogicalParticleFilter1.SIProlog.PEnv;
namespace LogicalParticleFilter1
{
    public partial class SIProlog
    {
        Dictionary<string, RuleList> ParseKEText(string startMT, string ruleSet, bool immediate, bool clearFirst)
        {
            var tempKB = new CIDictionary<string, RuleList>(new KeyCase(NormalizeKBName));
            LoadKEText(startMT, ruleSet, FindOrCreateRuleList(tempKB, immediate, clearFirst), immediate);
            return tempKB;
        }

        private Func<string, Action<Rule>> FindOrCreateRuleList(IDictionary<string, RuleList> dict, bool immediate, bool clearFirst)
        {
            return ((s) =>
                        {
                            PNode node = FindKB(s);
                            if (immediate)
                            {
                                if (node == null)
                                {
                                    node = FindOrCreateKB(s);
                                }
                            }
                            RuleList rl;
                            lock (dict)
                            {
                                if (!dict.TryGetValue(s, out rl))
                                {
                                    if (node != null)
                                    {
                                        if (clearFirst && immediate)
                                        {
                                            node.Clear();
                                        }
                                    }
                                    rl = dict[s] = new RuleList();
                                }
                            }
                            bool immedAdd = immediate && node != null;
                            if (!immedAdd) return (r) => rl.Add(r);
                            return r =>
                                       {
                                           rl.Add(r);
                                           node.AddImmediate(r);
                                       };
                        });
        }

        void LoadKEText(string startMT, string ruleSet, Func<string, Action<Rule>> tempKB, bool immediate)
        {
            if (startMT != null) tempKB(startMT);
            // if startMT is null use the global variable
            string parseKB = startMT ?? threadLocal.curKB;
            // code below uses parseKB

            ///string[] lines = ruleSet.Split('\n');
            string curConst = "";
            {
                do
                {
                    if (ruleSet.Trim() == "") break;
                    bool readProlog = false;
                    string line = toEndOfLine(ref ruleSet);
                    try
                    {
                        Action CheckImmediate = () =>
                                                 {
                                                     if (!immediate)
                                                     {
                                                         throw new NotSupportedException(
                                                             "In parsing from static cannot process: " + line);
                                                     }
                                                 };

                        if (line.StartsWith(";doc;"))
                        {
                            var line5 = line.Substring(5).Trim() + " .";
                            Term t = ParseTerm(new Tokeniser(line5), startMT) as Term;
                            DocumentTerm(t, false);
                            continue;
                        }
                        if (line.StartsWith(";") || line.StartsWith("%")) continue;
                        if (line.StartsWith("exit:")) return;
                        if (line.StartsWith("."))
                        {
                            continue;
                        }
                        do
                        {
                            if (!line.Contains(":") || line.Contains(":-"))
                            {
                                readProlog = true;
                                break;
                            }
                            string[] args = line.Split(':');
                            string cmd = args[0].Trim().ToLower();
                            // if any non letters in our KE text directive parse it as prolog instead
                            if (!Regex.Match(cmd, "^[a-z]+$").Success)
                            {
                                readProlog = true;
                                break;
                            }
                            // we need to trim off period and and spaces before the period
                            string val = TrimAndRemoveTrailingPeriod(args[1]);
                            if (cmd == "tbc")
                            {
                                continue;
                            }
                            if (cmd == "mt")
                            {
                                parseKB = val;
                                var rl = tempKB(parseKB);
                                threadLocal.curKB = val;
                                continue;
                            }
                            if (cmd == "base")
                            {
                                // todo process base                            
                                continue;
                            }
                            if (cmd == "const" || cmd == "constant" || cmd == "comment")
                            {
                                curConst = val;
                                continue;
                            }
                            if (cmd == "predicate")
                            {
                                Term t = ParseTerm(new Tokeniser(val), startMT) as Term;
                                DocumentTerm(t, false);
                                curConst = t.fname;
                                continue;
                            }
                            if (cmd == "genlmt")
                            {
                                CheckImmediate();
                                connectMT(parseKB, val);
                                continue;
                            }
                            if (cmd == "nonmt")
                            {
                                CheckImmediate();
                                disconnectMT(parseKB, val);
                                continue;
                            }

                            if (cmd == "genlmtconst")
                            {
                                CheckImmediate();
                                connectMT(parseKB, curConst);
                                continue;
                            }
                            if (cmd == "alias")
                            {
                                aliasMap[val] = parseKB;
                                continue;
                            }
                            if (cmd == "include")
                            {
                                LoadKEText(parseKB, FromStream(val), tempKB, immediate);
                                continue;
                            }
                            if (cmd == "chemsys")
                            {
                                string[] sep = { "chemsys:" };
                                args = line.Split(sep, StringSplitOptions.RemoveEmptyEntries);
                                val = args[0].Trim();
                                if (chemSysCommandProcessor != null)
                                {
                                    chemSysCommandProcessor(val);
                                }
                                string[] args2 = val.Split(',');
                                string head = args2[0];
                                string newhead = head + "(";
                                string oldhead = head + ",";
                                string newPred = val.Replace(oldhead, newhead) + ").\n";
                                newPred = newPred.Replace(",,", ",0,");
                                if (!newPred.Contains(":"))
                                {
                                    tempKB(parseKB)(ParseRule(new Tokeniser(newPred), parseKB));

                                }
                                continue;
                            }

                            if (cmd == "module")
                            {
                                // A Macro for CEMA/GOAP
                                // same as 
                                //  mt:module_name
                                //  module(module_name).

                                parseKB = val;
                                threadLocal.curKB = val;
                                val = atomize(val);
                                string uniPred = String.Format("module({0}).\n", val);
                                tempKB(parseKB)(ParseRule(new Tokeniser(uniPred), parseKB));
                                continue;
                            }

                            //default is to make a binary pred of "cmd(curConst,val)."
                            val = TrimAndRemoveTrailingPeriod(val);
                            cmd = TrimAndRemoveTrailingPeriod(args[0]);
                            if (val.Length > 0)
                            {
                                val = atomize(val);
                                string binaryPred = String.Format("{0}({1},{2}).\n", cmd, atomize(curConst), val);
                                var rule = ParseRule(new Tokeniser(binaryPred), parseKB);
                                if (rule != null)
                                {
                                    tempKB(parseKB)(rule);
                                    continue;
                                }
                                else
                                {
                                    readProlog = true;
                                    break;
                                    // fall thru to other reader.. the ":" confused us
                                }
                            }
                        } while (false);
                        if (!readProlog)
                        {
                            continue;
                        }
                        //else
                        {
                            string was = line;
                            Tokeniser firstTokenizer = new Tokeniser(was);
                            var rule = ParseRule(firstTokenizer, parseKB);
                            if (rule == null)
                            {
                                if (!was.EndsWith("."))
                                {
                                    firstTokenizer = new Tokeniser(was + ".");
                                    rule = ParseRule(firstTokenizer, parseKB);
                                }
                            }
                            string remainder = firstTokenizer.remainder;
                            if (remainder.Trim().Length > 0)
                            {
                                prolog_reader_debug("remainder found");
                                ruleSet = remainder + "\n" + ruleSet;
                            }
                            if (rule == null)
                            {
                                if (trace)
                                {
                                    Warn("Could not parse: " + was);
                                    continue;
                                }
                                Tokeniser newTokeniser = new Tokeniser(was);
                                string kb = parseKB;
                                tl_spy_prolog_reader = true;
                                rule = ParseRule(newTokeniser, kb);
                                tl_spy_prolog_reader = false;
                                Warn("Could not parse: " + was);
                                continue;
                            }
                            tempKB(parseKB)(rule);
                        }
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("EXCEPTION: in line '{0}' caused '{1}'", line, e.Message);
                    }
                } while (ruleSet.Trim() != "");
            }
        }

        private string TrimAndRemoveTrailingPeriod(string val)
        {
            return val.Trim().TrimEnd('.').TrimEnd();
        }

        private string toEndOfLine(ref string ruleSet)
        {
            ruleSet = ruleSet.TrimStart();
            int eol = ruleSet.IndexOf('\n');
            if (eol == -1)
            {
                var line0 = ruleSet;
                ruleSet = "";
                return line0;
            }
            var line = ruleSet.Substring(0, eol + 1).TrimEnd('\r', '\n');
            ruleSet = ruleSet.Substring(eol + 1);
            return line;
        }

        public string convertTextToProlist(string text)
        {
            text = text.Replace("?", " questionmark ");
            text = text.Replace("!", " exclamationmark ");
            text = text.Replace(".", " periodmark ");
            while (text.Contains("\n")) text = text.Replace("\n", " ");
            while (text.Contains("\r")) text = text.Replace("\r", " ");
            while (text.Contains("  ")) text = text.Replace("  ", " ");
            text = "[\"" + text.Replace(" ", "\",\"").Trim() + "\"]";
            while (text.Contains("[,")) text = text.Replace("[,", "[");
            while (text.Contains(",]")) text = text.Replace(",]", "]");
            while (text.Contains(",,")) text = text.Replace(",,", ",");
            return text;
        }
        public void postListPredToMt(string pred, string text, string mt)
        {
            // will take text, convert to a list, place inside pred and
            // assert to mt

            string gaf = String.Format("{0}({1}).", pred, convertTextToProlist(text));
            insertKB(gaf, mt);
        }
        public void appendListPredToMt(string pred, string text, string mt)
        {
            // will take text, convert to a list, place inside pred and
            // assert to mt

            string gaf = String.Format("{0}({1}).", pred, convertTextToProlist(text));
            appendKB(gaf, mt);
        }
        public RuleList parseRuleset(string rulesIn, string homeMt)
        {
            var pmt = ParseKEText(homeMt, rulesIn, false, true);
            if (pmt != null && pmt.Count == 1)
            {
                var ruleList0 = pmt.Values.FirstOrDefault();
                return ruleList0;
            }
            string[] rules = rulesIn.Split('\n');
            RuleList ruleList = new RuleList();
            var outi = 0;
            for (var r = 0; r < rules.Length; r++)
            {
                string rule = rules[r];
                if (rule.Length > 0)
                {
                    if (rule.Substring(0, 1) == "#" || rule == "") continue;

                    var or = ParseRule(new Tokeniser(rule), homeMt);
                    if (or == null) continue;
                    or.OptionalHomeMt = homeMt;
                    ruleList.Add(or);
                    // print ("Rule "+outi+" is : ");
                    if (show) or.print(Console.Write);
                }
            }
            return ruleList;
        }

        #region prologParser
        public const string SYNTAX_UriQuotes = "<>"; // uri
        public const string SYNTAX_DoubleQuotes = "\"\""; // strings
        public const string SYNTAX_AtomQuotes = "''"; // atoms and unqualified names
        public const string SYNTAX_NoQuotes = ""; // numbers - if an atom is set to have no quotes that is just how it round trips
        public const string SYNTAX_LiteralDataType = "{}";
        public const string SYNTAX_GraphLiteralType = "[]";
        public const string MustGuessQuotes = null;

        static public Part MakeTermPostReader(String f, bool isHeadVar, PartListImpl partlist)
        {
            if (partlist.Arity == 0)
            {
                if (isHeadVar) return new Variable(f);
                return Atom.FromName(f);
            }
            if (f == "$obj")
            {
                Part partlist1 = partlist[0];
                if (partlist1.fname == "$literal")
                {
                    return Atom.MakeLiteral(partlist[1].AsString(), partlist[2].AsString(), partlist[3].AsString());
                }
                Warn("Not sure how to create a " + partlist);
            }
            return new Term(f, isHeadVar, partlist);
        }

        // The Tiny-Prolog parser goes here.
        public class Tokeniser
        {
            public override string ToString()
            {
                return "now=" + currentTok + " before=" + previousToks + " remainder:" + remainder;
            }
            public string remainder;
            public string typeSyntax;
            public string type;
            public string current;
            public string previousToks = "";

            public string prev_typeSyntax;
            public string prev_type;
            public string prev_current;
            public string initial;

            public string currentTok
            {
                get
                {
                    return "[" + type + ",\"" + current + ",\"" + typeSyntax + "\"],";
                }
            }
            public Tokeniser(Tokeniser copyThis)
            {
                this.initial = copyThis.initial;
                this.previousToks = copyThis.previousToks;
                this.prev_current = copyThis.prev_current;
                this.prev_type = copyThis.prev_type;
                this.prev_typeSyntax = copyThis.prev_typeSyntax;
                this.remainder = copyThis.remainder;
                this.current = copyThis.current;
                this.typeSyntax = copyThis.typeSyntax;
                this.type = copyThis.type;
            }
            public Tokeniser Clone()
            {
                return new Tokeniser(this);
            }
            public Tokeniser(string input)
            {
                this.initial = input;
                Init(input);
            }

            public void Init(string input)
            {
                previousToks = null;
                this.prev_current = null;
                this.prev_type = null;
                this.prev_typeSyntax = null;
                this.remainder = input;
                this.current = null;
                this.typeSyntax = null;
                this.type = null; // "eof", "id", "var", "punc" etc.
                this.consume0(); // Load up the first token.
            }

            public void consume()
            {
                prev_current = current;
                prev_type = type;
                prev_typeSyntax = typeSyntax;
                previousToks = previousToks + currentTok;
                consume0();
            }
            internal void consume0()
            {
                if (this.type == "eof") return;
                // Eat any leading WS
                Match r = Regex.Match(this.remainder, @"^\s*(.*)$");
                if (r.Success)
                {
                    this.remainder = r.Groups[1].Value;
                }

                if (this.remainder == "")
                {
                    this.current = null;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "eof";
                    return;
                }

                // Decimal numbers book suggests: @"^([-+]?[0-9]*\.[0-9]+|[0-9]+)(.*)$".
                r = Regex.Match(this.remainder, @"^([-]?[0-9]*[\.]?[0-9]+[0-9]*)(.*)$");
                //r = this.remainder.match(/^(-[0-9][0-9]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "id";
                    return;
                }

                ParseType[] parseTypes = {
                                               new ParseType("{}", true, true), 
                                               new ParseType(SYNTAX_UriQuotes, false, true),
                                               new ParseType(SYNTAX_DoubleQuotes, true, true),
                                               new ParseType(SYNTAX_AtomQuotes, false, false)
                                           };

                int remLen = remainder.Length;
                foreach (ParseType parseType in parseTypes)
                {
                    if (!this.remainder.StartsWith(parseType.StartSequence)) continue;
                    int remIndex = parseType.StartSequence.Length;
                    if (parseType.CantHaveNext.Contains(remainder[remIndex])) continue;
                    string endSeq = parseType.EndSequence;
                    int endLen = endSeq.Length;
                    if (remLen < endLen) continue;
                    char endChar = '\0';
                    if (endLen > 0)
                    {
                        endChar = endSeq[0];
                    }
                    string soFar = "";
                    string isError = null;
                    do
                    {
                        char next = remainder[remIndex];
                        if (parseType.EscapeChars.Contains(next))
                        {
                            remIndex++;
                            next = remainder[remIndex];
                            soFar += next;
                            continue;
                        }
                        if (next == endChar && (endLen == 1 || remainder.Substring(next).StartsWith(endSeq)))
                        {
                            // found endchar
                            isError = null;
                            break;
                        }
                        if (parseType.CantHave.Contains(next))
                        {
                            isError = String.Format("CantHaveChar: '{0}'", next);
                            break;
                        }
                        soFar += next;
                        remIndex++;
                    } while (remIndex < remLen);

                    if (!String.IsNullOrEmpty(isError))
                    {
                        if (tl_spy_prolog_reader)
                        {
                            Warn("not using quote type becasue: " + isError);
                        }
                        continue;
                    }
                    int remainderLen = remainder.Length;
                    if (remIndex + 1 > remainderLen)
                    {
                        if (tl_spy_prolog_reader)
                        {
                            Warn("not using quote type becasue: is is past end of file: " + this);
                        }
                        continue;
                    }
                    current = soFar;
                    remainder = remainder.Substring(remIndex + 1);
                    type = parseType.typeName;
                    typeSyntax = parseType.quoteName;
                    return;
                }

                r = Regex.Match(this.remainder, @"^([\(\)\.,\[\]\|\!]|\:\-)(.*)$");
                //r = this.remainder.match(/^([\(\)\.,\[\]\|\!]|\:\-)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "punc";
                    return;
                }

                r = Regex.Match(this.remainder, @"^([A-Z_\?][a-zA-Z0-9_\?\-]*)(.*)$");
                //r = this.remainder.match(/^([A-Z_][a-zA-Z0-9_]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "var";
                    return;
                }

                // URLs in curly-bracket pairs
                r = Regex.Match(this.remainder, @"^(\{[^\}]*\})(.*)$");
                //r = this.remainder.match(/^(\{[^\}]*\})(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = "{}";
                    this.type = "id";
                    return;
                }

                // Quoted strings

                r = Regex.Match(this.remainder, @"^(\""[^\""]*\"")(.*)$");
                // r = Regex .Match( this.remainder,@"^(\"\[\^\"\]*\")(.*)$");
                //r = this.remainder.match(/^("[^"]*")(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_DoubleQuotes;
                    this.type = "id";
                    return;
                }

                // Strings of alphanumerics with "_" and ":" 
                r = Regex.Match(this.remainder, @"^([a-zA-Z0-9][a-zA-Z0-9_\:]*)(.*)$");
                //r = this.remainder.match(/^([a-zA-Z0-9][a-zA-Z0-9_]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "id";
                    return;
                }

                // negative integers
                r = Regex.Match(this.remainder, @"^(-[0-9][0-9]*)(.*)$");
                //r = this.remainder.match(/^(-[0-9][0-9]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "id";
                    return;
                }

                // Negative Decimal numbers
                r = Regex.Match(this.remainder, @"^(-?[\d\.]+)(.*)$");
                //r = this.remainder.match(/^(-[0-9][0-9]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "id";
                    return;
                }

                if (tl_spy_prolog_reader)
                {
                    Warn("Unconsumed " + remainder);
                    return;
                }

                this.current = null;
                this.type = "eof";
                this.typeSyntax = SYNTAX_NoQuotes;

            }
        }



        public Rule ParseRule(Tokeniser tk, string mt)
        {
            // A rule is a Head followed by . or by :- Body

            var p = ParseHead(tk, mt);
            if (p == null)
            {
                prolog_reader_debug("cant parse Rule head: " + tk);
                return null;
            }
            Term h = p as Term;
            if (h == null)
            {
                if (!(p is Atom))
                {
                    prolog_reader_debug("didn't parse Rule head as atom or term: " + tk);
                    // return null;
                }
                h = p.AsTerm();
            }
            if (tk.current == ".")
            {
                // A simple rule.
                tk.consume();
                return new Rule(h);
            }

            if (tk.current != ":-")
            {
                prolog_reader_debug("Rule neck missing: " + tk);
                return null;
            }
            tk.consume();
            PartListImpl b = ParseBody(tk, mt);
            if (b == null)
            {
                prolog_reader_debug("Rule body not present: " + tk);
                return null;
            }
            if (tk.current == "." || tk.current == "eof")
            {
                tk.consume();
                return new Rule(h, b);
            }
            prolog_reader_debug("Rule body not terminated: " + tk);
            return null;
        }

        static Term AsTerm(Part p)
        {
            if (p == null) return null;
            return p.AsTerm();
        }

        public Term ParseHead(Tokeniser tk, string mt)
        {
            // A head is simply a term. (errors cascade back up)
            return ParseTerm(tk, mt);
        }

        static public Term ParseTerm(Tokeniser tk, string mt)
        {
            Part p = ParsePart(tk, mt);
            return AsTerm(p);
        }

        // This was a beautiful piece of code. It got kludged to add [a,b,c|Z] sugar.
        static public Part ParsePart(Tokeniser tk, string mt)
        {
//            string mt = threadLocal.curKB;
            // Part -> var | id | id(optParamList)
            // Part -> [ listBit ] ::-> cons(...)     
            if (tk.type == "punc" && tk.current == "[")
            {
                // Parse a list (syntactic sugar goes here)
                tk.consume();
                // Special case: [] = Atom.Make(nil).
                if (tk.type == "punc" && tk.current == "]")
                {
                    tk.consume();
                    return Atom.FromSource(FUNCTOR_NIL);
                }

                // Get a list of parts into l
                ArrayList l = new ArrayList();
                int i = 0;

                while (true)
                {
                    var t = ParsePart(tk, mt);
                    if (t == null)
                    {
                        prolog_reader_debug("cant parse List Part " + tk);
                        return null;
                    }

                    l.Insert(i++, t);
                    if (tk.current != ",") break;
                    tk.consume();
                }

                // Find the end of the list ... "| Var ]" or "]".
                Part append;
                if (tk.current == "|")
                {
                    tk.consume();
                    append = ParsePart(tk, mt);
                }
                else
                {
                    append = Atom.FromSource(FUNCTOR_NIL);
                }
                if (tk.current != "]")
                {
                    prolog_reader_debug("Unclosed List " + tk);
                    return null;
                }
                tk.consume();
                // Return the new cons.... of all this rubbish.
                for (--i; i >= 0; i--)
                {
                    append = MakeList((Part)l[i], append);
                }
                return append;
            }
            var notthis = false;
            if (tk.current == "NOTTHIS")
            {
                notthis = true;
                tk.consume();
            }
            var type = tk.type;
            var name = tk.current;
            var quotingType = tk.typeSyntax;
            tk.consume();
            if (quotingType == SYNTAX_DoubleQuotes)
            {
                return Atom.FromSourceReader(name, quotingType);
            }
            // fail shorthand for fail(), ie, fail/0
            if (type == "id" && name == "fail")
            {
                return MakeTerm("fail");
            }
            // Parse ! as cut/0
            if (type == "punc" && name == "!")
            {
                return MakeTerm("cut");
            }
            // Parse [id|var](ParamList)
            if (tk.current == "(")
            {
                tk.consume();

                PartListImpl p = ParseConjuncts(tk, mt, ")", ",", false);
                if (p == null)
                {
                    prolog_reader_debug("cant read termargs " + tk);
                    return null;
                }
                Part term = MakeTermPostReader(name, type == "var", p);
                if (term is Term)
                {
                    if (notthis) ((Term)term).excludeThis = true;
                }
                return term;
            }
            if (type == "var")
            {
                return new Variable(name);
            }
            return Atom.FromSourceReader(name, quotingType);
        }
        static public PartListImpl ParseConjuncts(Tokeniser tk, string mt, string requiredEnd, string sep, bool eofReturnsPart)
        {
            // Body -> Term {, Term...}

            PartListImpl p = new PartListImpl();
            var i = 0;
            while (true)
            {
                if (tk.current == requiredEnd)
                {
                    tk.consume();
                    break;
                }
                if (tk.current == "eof")
                {
                    if (!eofReturnsPart)
                    {
                        prolog_reader_debug("read EOF " + tk);
                        return null;
                    }
                    break;
                }
                if (tk.current == sep)
                {
                    tk.consume();
                    continue;
                }
                Part t = ParsePart(tk, mt);
                if (t == null)
                {
                    prolog_reader_debug("cant read Conjuct item " + tk);
                    return null;
                }
                p.AddPart(t);
                i++;
            }

            if (i == 0)
            {
                prolog_reader_debug("no items read " + tk);
                return null;
            }
            return p;
        }


        [ThreadStatic]
        public static bool tl_spy_prolog_reader = false;

        private static void prolog_reader_debug(string why)
        {
            if (!tl_spy_prolog_reader) return;
            Warn("prolog_reader_debug: " + why);
        }

        static private Part MakeList(Part li, Part append)
        {
            return MakeTerm(FUNCTOR_CONS, li, append);
        }
        public PartListImpl ParseBody(string query, string mt)
        {
            return ParseBody(new Tokeniser(query), mt);
        }

        public PartListImpl ParseQuery(string query, string mt)
        {
            var qlist = ParseBody(new Tokeniser(query), mt);
            if (qlist == null)
            {
                Warn("An error occurred parsing the query '{0}.\n", query);
            }
            return qlist;
        }
        public PartListImpl ParseBody(Tokeniser tk, string mt)
        {
            // Body -> Term {, Term...}

            PartListImpl p = new PartListImpl();
            var i = 0;

            Term t;
            while ((t = (Term)ParseTerm(tk, mt)) != null)
            {
                p.AddPart(t);
                i++;
                if (tk.current != ",") break;
                tk.consume();
            }

            if (i == 0)
            {
                return null;
            }
            return p;
        }
        #endregion
    }

    internal class ParseType
    {
        public ParseType(string quoteName0, bool canHaveSpaceAfterStart0, bool multiLine0)
        {
            quoteName = quoteName0;
            StartSequence = "" + quoteName0[0];
            EndSequence = "" + quoteName0[quoteName0.Length - 1];
            CantHaveSpaceAfterStart = canHaveSpaceAfterStart0;
            MultiLine = multiLine0;
        }
        public string StartSequence;
        public string EndSequence;
        public string typeName = "id";
        public string quoteName = "atom";
        public bool MultiLine
        {
            set
            {
                CantHave += "\r\n";
            }
            get
            {
                return CantHave.Contains('\n');
            }
        }
        public bool CantHaveSpaceAfterStart
        {
            set
            {
                CantHaveNext += " ";
            }
            get
            {
                return CantHaveNext.Contains(" ");
            }
        }
        public string EscapeChars = "\\";
        public string CantHave = "";
        public string CantHaveNext = "";
    }
}

