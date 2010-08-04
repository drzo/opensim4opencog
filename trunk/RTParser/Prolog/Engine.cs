#if VISUAL_STUDIO
#define debugging
#define arg1index
#define mswindows
#define newor
#endif

/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007-2009 John Pool -- j.pool@ision.nl
                   Contributions 2009 by Lars Iwer -- lars.iwer@inf.tu-dresden.de

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU General Public License as published by the Free Software Foundation; either version
  2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for details, or enter 'license.' at the command prompt.

-------------------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------

  C#Prolog -- A Prolog interpreter written in C# -- version 1.1

  Author: John Pool
          Amersfoort
          Netherland
          j.pool@ision.nl

  The C#Prolog interpreter has been rewritten in C# from scratch, although
  some of the basic mechanisms are inspired by XProlog developed (in Java)
  by Jean Vaucher, who at his turn enhanced the WProlog program developed by
  Michael Winikoff. In comparison to XProlog it has the following new features:

  - A more user-friendly (but still quite basic) DOS-box toplevel user-interface,
    with the possibility of referring to and editing of commands given earlier
    (using !, !!, !<n>, and editing via !<n>/<oldstring>/<newstring>/ );

  - The possibility of defining new operators using :-op(Precedence, Type, Name);

  - Enhanced arithmetic (many of the standard C# builtin functions are available);

  - Builtin types DateTime and TimeSpan;

  - Predicates for manipulating XML (a la PLXML; cf. Google). These predicates
    use the superb C# capabilities for handling XML;

  - (Simple) preprocessor directives #define, #undefine #if, #else, #ifnot, #endif;

  - assert/retract not only for facts, but also for rules;

  - Builtin DCG processing;

  - :-discontiguous directive, to indicate that predicate clauses may be scattered
    over the source file. Multi-file predicates are not allowed, however;

  - :-persistent directive, to indicate that the predicate clauses are to be
    retrieved from / inserted into a database table (or stored procedure). This
    only applies to fully instantiated fact (head-only) predicates. The use of such
    predicates is completely transparent: the only difference is that asserted
    values are still there at the next session. This is also the method that I used
    for storing huge fact databases. The RDBMS that I use is Firebird (freeware),
    which is very reliable, very fast, and has many features;

  - First-argument indexing;

  - An option to set a limit to the execution time of a query;

  - Enhanced debugging and tracing options. Debugging output can be captured into an
    XML-file;

  - Regular expressions;

  - Many new predicates.

  This version appears to be functioning correctly, and fairly fast. I have tried to
  run CHAT-80, which works alright, though in some cases no answer is returned.
  Because of the complexity of the CHAT-80 code I have not been able to trace
  whether this is because of some defect in the interpreter or a bug in the CHAT-80
  code.

  This does not mean to say that there is nothing left to do. Some known problem
  areas to which I did not get around yet, are:

  - The read-predicate may needs some debugging. I have not tested this predicate
    extensively. The same goes for 'seeing' and 'telling'. The consult-predicate
    works fine;

  - The setof-predicate is not implemented according to the official definition, in
    that the existential operator (^) does not work. setof simply returns all solutions
    (sorted) in a single list;

  - A graphical user-interface would be nice;

  - C# does not completely support Unicode. In principle, this should not be to
    difficult, given the C# Unicode facilities;

  - atom(X) is not implemented fully correctly. It works fine for 'regular' atoms,
    but e.g. in case of atom(++) it should return true, but if + is defined as a
    prefix operator ++ is regarded as a compound term +(+);

  - Many of the standard ISO-predicates are lacking. Usually their implementation will
    not be difficult, but I simply have not had the time yet to do this;

  - It would be nice to have a compile option (WAM?);

  For the parser, I have used a grammatical description of the Prolog language which
  I fed into a proprietary parser generator (which is not publically avalailable) that
  turned it into C# code.

  Here is a brief description of the files making up C#Prolog. Only the main features
  of each files are mentioned:

  c.bat, r.bat, cr.bat   Compile, Run, and Compile+Run C#Prolog

  _csc.bat,
  csc_options.inc        Support files for the above commands

  C#Prolog.cs            Top level; contains main()

  Builtins.cs            Contains all predefined predicates

  Engine.cs              Contains the Execute() method, and the implementation of all
                         predefined predicates

  IO.cs                  All IO-related stuff. Must be adapted when a graphical interface
                         is developed

  Operator.cs            Operator details

  Persistent.cs          Implementation of presistent predicates (interface with Firebird)

  PL.cs                  Parser, generated by parser generator. Any modifications here
                         will be wiped out by a next generation!

  PredDescr.cs           Predicate descriptor: all there is to know about a predicate

  PredStorage.cs         Datastructures and methods for storing the 'program': the
                         predefined and consulted predicates

  SimpleDOMParser.cs     XML-related stuff

  Term.cs                Datastructures and methods for creating and handling Terms

  TermNodeList.cs        Datastructures and methods for creating and handling linked
                         lists of Terms

  TermSet.cs             Sets of Terms

  Utils.cs               Miscellaneous stuff

  This version should also run under Mono without many problems, although I must admit
  I have not been able to try that yet. I have tried, though, to identify MS-Windows-
  specific features with the #mswindows compiler define (cf. csc_options.inc)

  John Pool -- February 21, 2007

  Email: j.pool@ision.nl

--------------------------------------------------------------------------------------*/

using System;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.Xml;
using System.Collections;
using System.Threading;
using System.Diagnostics;
using System.Collections.Generic;
using IO = RTParser.Prolog.PrologIO;
#if mswindows
using System.Security.Principal;
using System.Collections.Generic; // available under Mono as well?
#endif

namespace RTParser.Prolog
{
    /// <summary>A choicepoint contains the continuation
    /// (a goal list) and the NextNode clause to be tried.
    /// </summary>
    public class ChoicePoint
    {
        protected TermNode goal;
        protected ClauseNode clause; // next clause to be tried for goal
        protected bool active;
        public TermNode Goal { get { return goal; } }
        public ClauseNode ClauseNode { get { return clause; } set { clause = value; } }
        public bool IsActive { get { return active; } }

        protected ChoicePoint()
        {
        }

        public ChoicePoint(TermNode g, ClauseNode c)
        {
            goal = g;
            clause = c;
            active = true;
        }

        public void Kill()
        {
            active = false;
        }

        public override String ToString()
        {
            return String.Format("choicepoint\ngoal {0}\nclause {1}\nactive {2}", goal, clause, active);
        }
    }

    /// <summary>
    /// The ClauseChoicePoint is needed for the clause/2 predicate implementation.
    /// It saves the next clauses to be returned during backtracking.
    /// </summary>
    public class ClauseChoicePoint : ChoicePoint
    {
        private ClauseNode nextClause;

        public ClauseNode NextClause
        {
            get { return nextClause; }
        }

        public ClauseChoicePoint(TermNode g, ClauseNode c, ClauseNode nextClause)
        {

            goal = g;
            clause = c;
            active = true;
            this.nextClause = nextClause;
        }

    }

    public class VarStack : Stack
    {
        private Stack swap;

        public VarStack()
            : base()
        {
            swap = new Stack();
        }

#if old
    public void DisableChoices (int n) // remove all choicepoints in this predicate until before body-expansion
    {
      object o;
      swap.Clear ();
      int i = Count;

      while (i-- > n)
      {
        if (!((o = Pop ()) is SpyPoint))
        {
          if (o is ChoicePoint) ((ChoicePoint)o).Kill (); // may be useful in debugging to retain rather than to delete?

          swap.Push (o);
        }
      }

      while (swap.Count != 0) Push (swap.Pop ());
    }
#else

        public void DisableChoices(long n)
        {
            int i = Count;

            foreach (object o in this) // works its way down from the top !!!
            {
                if (i-- == n) return;

                if (o is SpyPoint)
                    ((SpyPoint)o).Kill(); // do not retain (failure) spypoints
                else if (o is ChoicePoint)
                    ((ChoicePoint)o).Kill();
            }
        }
#endif
    }


    public class PrologEngine
    {
        #region private members

        private bool halted = false;
        private class AbortQueryException : ApplicationException { }
        //private static Parser seeParser = Globals.CurrentParser = new Parser (ps);   /////////////// is dit wel nodig? !!!!!!!!!!!!!!!!!!!!!!!!!!
        private string currentInputName;
        //private ThreadStart   seeParserRun;
        private Thread seeParserThread;
        private TextReader currentInputReader = Globals.StdIn;
        private TextWriter currentOutputWriter = Globals.StdOut;
        //private int           CallNo; // debugger: for uniquely identifying spy-ports
        //private static int    uCount; // Unify-calls count
        private const int INF = Int32.MaxValue;
        public VarStack varStack = new VarStack(); // stack of variable bindings and choice points
        private PredicateStorage ps = new PredicateStorage();
        private TermNode goalNode;
        private bool trace = false;
        private bool debug;
        private bool firstGoal; // set in ExecuteGoalList() to be able to check whether a goal in the query is the very first
        private bool redo; // set by CanBacktrack if a choice point was found
        private bool qskip = false;
        private bool rushToEnd;
        private string xmlFile = null;
        private bool xmlTrace = false;
        private int xmlElCount; // current approximate number of elements in the XML trace file
        private int xmlMaxEl;   // maximum allowed value of xmlElCount
        private bool reporting;  // debug (also set by 'trace') || xmlTrace
        private XmlTextWriter xtw;
        private ClauseNode retractClause;
        private int levelMin; // lowest recursion level while spying -- for determining left margin
        private int levelMax; // used while spying for determining end of skip
        private int prevLevel;
        private ChoicePoint currentCp;
        private object lastCp;
        private long startTime = -1;
        private TimeSpan procTime = TimeSpan.MinValue;
        private CommandHistory cmdBuf = new CommandHistory();
        private bool goalListProcessed;
        private ManualResetEvent sema;
        private bool goalListResult;
        private int queryTimeout = 0; // maximum number of milliseconds that a query may run -- 0 means unlimited
        private bool findFirstClause; // find the first clause of predicate that matches the current goal (-list head)
        //public  static int    UCount { get { return uCount; } set { uCount = value; } }
        //public  string        UpMs { get { return (elapsedTime == 0) ? null : (uCount / elapsedTime).ToString (); } }
        // choicepoints and variables that need to be unbound upon backtracking.

        #endregion

        #region public properties

        public int CmdNo { get { return cmdBuf.cmdNo; } }
        public bool Debugging { get { return debug; } }
        public bool Halted { get { return halted; } }
        // Term reading stuff -- for reading one by one (to prevent the parser from scanning a file in one go)
        public static string TermMonitor = "something";

        #endregion

        #region static members

        public static Parser parser = null;
        public string VERSION = "1.1.0";
        public string RELEASE = Parser.VersionTimeStamp;
        public static string YES = "\nyes";
        public static string NO = "\nno";
        private static int gensymInt = 0;
        private static int parserCount = 0; ////////////// TESTING ONLY

        #endregion

        private class OpRec
        {
            public int Pr;
            public string Fx;
            public string Op;

            public OpRec(int p, OType t, string o)
            {
                Pr = p;
                Fx = t.ToString();
                Op = o;
            }

            public override string ToString()
            {
                return String.Format("{0} ({1} {2})", Op, Pr, Fx);
            }
        }


        /* Command history commands:
           ========================
           !!                : show numbered list of previous commands
           !                 : repeat previous command
           !<n>              : repeat command number <n>
           !/<old>/<new>/    : repeat previous command, with <old> replaced by <new>.
                               / may be any char, and the end / may be omitted.
           !<n>/<old>/<new>/ : same for command number <n>

           History commands do not need to be followed by a '.'
        */
        private class CommandHistory : ArrayList
        {
            internal int cmdNo { get { return Count + 1; } }

            internal bool PreprocessQuery(ref string query)
            {
                string s = query.TrimEnd('.').Trim(); // remove final dot and leading and trailing spaces

                if (s.Length == 0) return true;

                if (s[0] == '!')
                {
                    int len = s.Length; // assumption: query always end with a .

                    if (s == "!!")
                    {
                        ShowHistory();
                        query = null;

                        return false;
                    }

                    try
                    {
                        int i = (len == 1) ? Count /*'!': last command*/ : Int32.Parse(s.Substring(1, len - 1));

                        if (i < 1 || i > Count) { query = null; return false; }

                        this.Add(query = this[i - 1].ToString());
                    }
                    catch
                    {
                        // check for find/replace: ![commandno]<sepchar><findstr><sepchar><replacestr>[<sepchar>]
                        Regex r = new Regex(@"^!(?<cno>\d+)?(?<sep>\S).{3,}$"); // find the command nr, the separator char, and check on length
                        Match m = r.Match(query);

                        if (!m.Success) { query = null; return false; }

                        char sep = m.Groups["sep"].Value[0];
                        int cmdNo = (m.Groups["cno"].Captures.Count > 0) ? Convert.ToInt32(m.Groups["cno"].Value) : Count;

                        if (cmdNo < 1 || cmdNo > Count) { query = null; return false; }

                        // replace oldSep in query by newSep some char that is bound not to occur in query

                        r = new Regex(@"^!\d*(?:\f(?<str>[^\f]+)){2}\f?$"); // 2 occurances of str
                        m = r.Match(query.Replace(sep, '\f'));

                        if (!m.Success) { query = null; return false; }

                        CaptureCollection cc = m.Groups["str"].Captures;
                        this.Add(query = this[cmdNo - 1].ToString().Replace(cc[0].Value, cc[1].Value));
                    }

                    PrologIO.WriteLine("?- " + query);
                }
                else if (query.Trim().EndsWith("/")) // TEMP
                {
                    query = "/";

                    return true;
                }
                else
                    this.Add(query);

                return true;
            }

            private void ShowHistory()
            {
                for (int i = 0; i < Count; i++) PrologIO.WriteLine("{0,2} {1}", i + 1, this[i]);
            }
        }


        private void RunSeeParser()
        {
            int pc = ++parserCount;
            try
            {
                Utils.WriteLine("RunSeeParser {0} started", pc);
                parser/*seeParser*/.Prefix = "&reading";
                parser/*seeParser*/.LoadFromFile(currentInputName);
            }
            catch
            {
            }
            finally
            {
                Utils.WriteLine("RunSeeParser {0} terminated", pc);
            }
        }


        public PrologEngine()
        {
            parser = new Parser(ps);
            ReadBuiltinPredicates();
            theExt = new Ext(this);
        }


        public string Answer
        {
            get { return (Globals.Answer); }
        }


        public Hashtable Variables // result of execution: query variables + values
        {
            get { return (Globals.Variables); }
        }


        private void ReadBuiltinPredicates()
        {
            //varStack.Clear ();
            ps.Reset();
            parser.StreamIn = Builtins.Predicates;
            retractClause = ps[Term.Key("retract", 1)].GetClauseList(null, null);
            ps.ResolveIndices();
        }


        public bool ExecuteQuery(ref string query)
        {
            bool result = false;

            if (!cmdBuf.PreprocessQuery(ref query)) return true;

            if (query == "/") { halted = true; return true; } // TEMP

            ElapsedTime();
            ProcessorTime();
            parser.StreamIn = query;
            rushToEnd = false;
            xmlFile = null;
            xmlMaxEl = INF;
            xmlTrace = false;
            levelMin = 0;
            levelMax = INF;
            prevLevel = -1;
            firstGoal = true;
            lastCp = null;
            goalNode = parser.QueryNode;

            if (goalNode == null) return true;

            varStack.Clear();

            try
            {
                findFirstClause = true;
                result = queryTimeout == 0 ? ExecuteGoalList() : StartExecuteGoalListThread();
            }
            catch (AbortQueryException)
            {
                result = false;
            }
            //catch //(Exception e)
            //{
            //  throw;
            //}
            finally
            {
                XmlTraceClose();
            }

            return result;
        }



        public bool StartExecuteGoalListThread()
        {
            ThreadStart startExecuteGoalList = new ThreadStart(RunExecuteGoalList);
            Thread run = new Thread(startExecuteGoalList);
            run.Name = "ExecuteGoalList";
            run.IsBackground = true;
            sema = new ManualResetEvent(false);
            goalListProcessed = false;
            goalListResult = false;
            run.Start(); // run will fall through to WaitOne
            sema.WaitOne(queryTimeout, false); // wait for timeOutMSecs (while the RunExecuteGoalList thread runs)

            if (!goalListProcessed) // goalListProcessed is set by RunExecuteGoalList()
            {
                run.Abort();

                return PrologIO.Error("Query execution timed out after {0} milliseconds", queryTimeout);
            }

            return goalListResult;
        }


        public void RunExecuteGoalList()
        {
            try
            {
                goalListResult = ExecuteGoalList();
                goalListProcessed = true;
            }
            catch (ThreadAbortException) // time-out
            {
                return;
            }
            catch // any other exception
            {
                goalListProcessed = true;

                throw;
            }
            finally
            {
                sema.Set();
            }
        }


        public bool ExecuteGoalList() // findFirstClause: find defining predicate
        {
            ElapsedTime();
            ProcessorTime();
            //uCount= 0;
            //CallNo = 0;
            redo = false; // set by CanBacktrack if a choice point was found

            // variables declaration used in goal loop to save stack space
            int stackTop;
            Term goal;
            TermNode currClause;
            Term cleanClauseHead;
            BI builtinId;
            int level;
            Term t;
            TermNode spyGoal;
            TermNode p;
            TermNode pHead;
            TermNode pTail;
            TermNode tn0;
            TermNode tn1;
            bool pFirst;


            while (goalNode != null)
            {
                if (goalNode is SpyPoint)
                {
                    TermNode sp = ((SpyPoint)goalNode).SaveGoal;
                    if (!Debugger(SpyPort.exit, sp, null, false, 1)) goalNode = sp.NextNode;
                    // debugger resets goalNode and returns true if user enters r(etry) or f(ail)
                    continue;
                }

                stackTop = varStack.Count; // varStack reflects the current program state

                if (findFirstClause && !goalNode.FindPredicateDefinition(ps)) // no defining predicate found
                {
                    if (
#if persistent
            !goalNode.IsPersistent &&  // if persistent, the predicate is defined but no matching database fact was found
#endif
PrologIO.Verbose)
                    {
                        goal = goalNode.Term;

                        switch (ps.ActionWhenUndefined(goal.Functor, goal.Arity))
                        {
                            case PredicateStorage.UndefAction.fail:
                                break;
                            case PredicateStorage.UndefAction.error:
                                return PrologIO.Error("Undefined predicate: {0}/{1}", goal.Functor, goal.Arity);
                            default:
                                PrologIO.Warning("Undefined predicate: {0}/{1}", goal.Functor, goal.Arity);
                                break;
                        }
                    }
                    if (!(redo = CanBacktrack(true))) return false;  // redo is set if a choice point was found
                }

                findFirstClause = false; // i.e. advance to the next clause upon backtracking
                currClause = goalNode.NextClause; // definition in program

                if (reporting) varStack.Push(new SpyPoint(SpyPort.call, goalNode)); // to be able to retry goal etc.
                //dmiles

                if (currClause.NextClause != null) // currClause.NextClause will be tried upon backtracking
                    varStack.Push(currentCp = new ChoicePoint(goalNode, currClause.NextClause));
                else if (reporting)
                    varStack.Push(new SpyPoint(SpyPort.fail, goalNode)); // to be able to detect failure in CanBacktrack

                cleanClauseHead = currClause.Term.CleanCopy(); // vars must be retained for clause body
                level = goalNode.Level;
                spyGoal = goalNode; // remember the original goalNode (which may be NextNode-ed, see below)

                if (reporting &&
                    Debugger(redo ? SpyPort.redo : SpyPort.call, goalNode, cleanClauseHead, currClause.NextNode == null, 2))
                    continue;  // Debugger may return some previous version of goalNode (retry- or fail-command)

                // UNIFICATION

                if (cleanClauseHead.Unify(goalNode.Term, varStack))
                {
                    currClause = currClause.NextNode; // body - if any - of defining clause (= matching clause of defining predicate)

                    // FACT
                    if (currClause == null) // defining clause body is null, so matching was against a fact
                    {
                        if (reporting && Debugger(SpyPort.exit, goalNode, null, false, 3)) continue;

                        goalNode = goalNode.NextNode;
                        findFirstClause = true;
                    }
                    // BUILTIN
                    else if ((builtinId = currClause.BuiltinId) != BI.none)
                    {
                        if (builtinId == BI.call)
                        {
                            t = goalNode.Term.Arg(0);

                            if (t.IsVar) return PrologIO.Error("Unbound variable '{0}' in goal list", t.VarName());

                            tn0 = t.ToGoalList(stackTop, goalNode.Level + 1);

                            if (reporting) tn0.Append(new SpyPoint(SpyPort.exit, spyGoal));

                            goalNode = (goalNode == null) ? tn0 : tn0.Append(goalNode.NextNode);
                            findFirstClause = true;
                        }
                        else if (builtinId == BI.or)
                        {
                            if (reporting) { varStack.Pop(); varStack.Pop(); }

                            tn1 = goalNode.Term.Arg(1).ToGoalList(stackTop, goalNode.Level);
                            varStack.Push(new ChoicePoint((goalNode == null) ? tn1 : tn1.Append(goalNode.NextNode), null));

                            tn0 = goalNode.Term.Arg(0).ToGoalList(stackTop, goalNode.Level);
                            goalNode = (goalNode == null) ? tn0 : tn0.Append(goalNode.NextNode);
                            findFirstClause = true;
                        }
                        else if (DoBuiltin(builtinId, out findFirstClause))
                        {
                            if (reporting && Debugger(SpyPort.exit, spyGoal, null, false, 5)) { findFirstClause = true; continue; }
                        }
                        else if (!(redo = CanBacktrack(true)))
                            return false;
                    }
                    // PREDICATE RULE
                    else // replace goal by body of matching clause of defining predicate
                    {
                        pHead = null;
                        pTail = null;
                        pFirst = true;

                        while (currClause != null)
                        {
                            if (currClause.Term is Cut)
                                p = new TermNode(new Cut(stackTop), goalNode.Level + 1); // save the pre-Unification state
                            else // false: keep the varNo constant over all terms of the predicate head+body
                                p = new TermNode(currClause.Term.CleanCopy(false), goalNode.Level + 1);

                            if (pFirst) { pFirst = false; pHead = p; } else pTail.NextNode = p;

                            pTail = p;
                            currClause = currClause.NextNode;
                        }

                        if (reporting)
                        {
                            pTail.NextNode = new SpyPoint(SpyPort.exit, spyGoal);
                            pTail = pTail.NextNode;
                        }

                        pTail.NextNode = goalNode.NextNode;
                        goalNode = pHead; // zal nooit een spypoint zijn
                        findFirstClause = true;
                    }
                }
                else if (!(redo = CanBacktrack(true)))
                    return false; // Unify failed - try backtracking

                firstGoal = false;
            }

            return true;
        }


        public bool CanBacktrack(bool local) // returns true if choice point was found
        {
            Object o;
            ChoicePoint cp;

            findFirstClause = false; // to prevent resetting to the first clause upon re-entering ExecuteGoalList

            while (varStack.Count != 0)
            {
                o = varStack.Pop();
                lastCp = o;

                if (reporting && o is SpyPoint)
                {
                    if (local && ((SpyPoint)o).Port == SpyPort.fail)
                        Debugger(SpyPort.fail, ((SpyPoint)o).SaveGoal, null, false, 6); // may reset goalNode
                }
                else if (o is Term)
                    ((Term)o).Unbind();
                else if (o is ChoicePoint && ((ChoicePoint)o).IsActive)
                {
                    goalNode = (cp = (ChoicePoint)o).Goal;

                    if (cp.ClauseNode == null)
                        findFirstClause = true; // find predicate belonging to the goal list (-head)
                    else
                        goalNode.NextClause = cp.ClauseNode; // next predicate value to be tried

                    return true;
                }
            }

            return false;
        }


        public bool More()
        {
            ElapsedTime();
            ProcessorTime();

            return ExecuteGoalList();
        }


        private bool Debugger(SpyPort port, TermNode goalNode, Term currClause, bool isFact, int callNo)
        {
            if (!reporting) return false;

            // only called if reporting = true. This means that at least one of the following conditions hold:
            // (1) debug is true. This means that trace = true and/or we must check whether this port has a spypoint
            // (2) xmlTrace = true.
            // Console-interaction will only occur if debug && (trace || spied)
            bool spied = false;
            bool console;
            string s;
            int free = 0;
            string lmar;

            if (!trace) // determine spied-status
            {
                if (goalNode.PredDescr == null) goalNode.FindPredicateDefinition(ps);

                spied = (goalNode.Spied && (goalNode.SpyPort | port) == goalNode.SpyPort);
            }
            console = debug && (trace || spied);
            // continue only if either trace or spied, or if an XML-trace is to be constructed
            if (!console && !xmlTrace) return false;

            lmar = null;

            Term goal = goalNode.Term;
            int level = goalNode.Level;

            if (@"\tdebug\tnodebug\tspy\tnospy\tnospyall\tconsult\ttrace\tnotrace\txmltrace\t".IndexOf(goal.Functor) != -1) return false;

            if (console) // this part is not required for xmlTrace
            {
                if (!qskip && level >= levelMax) return false;

                levelMax = INF;   // recover from (q)s(kip) command
                qskip = false; // ...


                const int widthMin = 20; // minimal width of writeable portion of line
#if mswindows
                int width = Utils.NumCols - 10;
#else
        int width = 140;
#endif
                int indent = 3 * (level - levelMin);
                int condensedLevel = 0;

                while (indent > width - widthMin)
                {
                    indent -= width - widthMin;
                    condensedLevel++;
                }

                if (condensedLevel == 0)
                {
                    lmar = Utils.RepeatString("|  ", level).Substring(0, indent);
                    free = width - indent;
                }
                else
                {
                    string dots = "| ... ";
                    lmar = dots + Utils.RepeatString("|  ", level).Substring(0, indent);
                    free = width - indent - dots.Length;
                }

                PrologIO.Write(lmar);
            }

            switch (port)
            {
                case SpyPort.call:
                    if (console)
                    {
                        s = Utils.WrapWithMargin(goal.ToString(), lmar + "|     ", free);
                        PrologIO.Write("{0,2:d2} Goal: {1}", level, s);
                        s = Utils.WrapWithMargin(currClause.ToString(), lmar, free);
                        PrologIO.Write("{0}{1,2:d2} {2}: {3}", lmar, level, "Try ", s);
                    }
                    if (xmlTrace)
                    {
                        if (level > prevLevel)
                        {
                            xtw.WriteStartElement("body");
                            xtw.WriteAttributeString("goal", goal.ToString());
                            xtw.WriteAttributeString("level", level.ToString());
                        }
                        else if (level < prevLevel)
                            xtw.WriteEndElement();
                        else
                            XmlTraceWriteTerm("goal", "goal", goal);
                        XmlTraceWriteTerm("try", isFact ? "fact" : "pred", currClause);
                    }
                    break;
                case SpyPort.redo:
                    if (console)
                    {
                        s = Utils.WrapWithMargin(currClause.ToString(), lmar + "|     ", free);
                        PrologIO.Write("{0,2:d2} {1}: {2}", level, "Try ", s); // fact or clause
                    }
                    if (xmlTrace)
                    {
                        if (level < prevLevel) xtw.WriteEndElement();
                        XmlTraceWriteTerm("try", isFact ? "fact" : "clause", currClause);
                    }
                    break;
                case SpyPort.fail:
                    if (console)
                    {
                        //s = Utils.WrapWithMargin (" ", lmar + "         ", free);
                        //IO.Write ("{0,2:d2} Fail  {1}", level, s);
                        s = Utils.WrapWithMargin(goal.ToString(), lmar + "|     ", free);
                        PrologIO.Write("{0,2:d2} Fail: {1}", level, s);
                    }
                    if (xmlTrace)
                    {
                        if (level < prevLevel) xtw.WriteEndElement();
                        XmlTraceWriteTerm("fail", "goal", goal);
                    }
                    break;
                case SpyPort.exit:
                    if (console)
                    {
                        s = Utils.WrapWithMargin(goal.ToString(), lmar + "         ", free);
                        PrologIO.Write("{0,2:d2} Exit: {1}", level, s);
                    }
                    if (xmlTrace)
                    {
                        if (level < prevLevel) xtw.WriteEndElement();
                        XmlTraceWriteTerm("exit", "match", goal);
                    }
                    break;
            }

            prevLevel = level;

            redo = false;

            if (rushToEnd || !console) return false;

            return DoDebuggingAction(port, lmar, goalNode);
        }


        private bool DoDebuggingAction(SpyPort port, string lmar, TermNode goalNode)
        {
            const string prompt = "|  TODO: ";
            const string filler = "|        ";
            int level;
            string cmd;
            int n = INF;
            int leap = 0; // difference between current level and new level

            while (true)
            {
                level = goalNode.Level;

                while (true)
                {
                    PrologIO.Write(lmar + prompt);
                    cmd = Console.ReadLine().Replace(" ", "");

                    if (cmd.Length > 1)
                    {
                        string cmd0 = cmd.Substring(0, 1);
                        try { n = Int32.Parse(cmd.Substring(1)); }
                        catch { break; }

                        if ("sr".IndexOf(cmd0) != -1 && Math.Abs(n) > level)
                            PrologIO.WriteLine(lmar + filler + "*** Illegal value {0} -- must be in the range -{1}..{1}", n, level);
                        else
                        {
                            if (n != INF && "cloqfgan+-.?h".IndexOf(cmd0) != -1)
                                PrologIO.WriteLine(lmar + filler + "*** Unexpected argument {0}", n);
                            else
                            {
                                if (n < 0) { level += n; leap = -n; } else { leap = level - n; level = n; }

                                cmd = cmd0;
                                break;
                            }
                        }
                    }
                    else
                    {
                        leap = 0;
                        if (cmd != "") cmd = cmd.Substring(0, 1);
                        break;
                    }
                }

                switch (cmd)
                {
                    case "":   // creap
                    case "c":  // ...
                        return false;
                    case "l":  // leap
                        SetSwitch("Tracing", ref trace, false);
                        return false;
                    case "s":  // skip
                        if (n == INF) levelMax = level; else levelMax = n + 1;
                        return false;
                    case "o":  // out (skip to exit or fail)
                        PrologIO.WriteLine(lmar + filler + "*** NOT YET IMPLEMENTED");
                        break;
                    case "q":  // q-skip (skip subgoals except if a spypoint was set)
                        levelMax = level;
                        qskip = true;
                        return false;
                    case "r":  // retry
                        if (port == SpyPort.call && leap == 0)
                        {
                            PrologIO.WriteLine(lmar + filler + "*** retry command has no effect here");

                            return false;
                        }
                        else
                        {
                            RetryCurrentGoal(level);

                            if (xmlTrace)
                            {
                                XmlTraceWriteElement("RETRY",
                                  (n == INF) ? "Retry entered by user" : String.Format("Retry to level {0} entered by user", level));
                                XmlTraceWriteEnds(leap);
                            }
                            return true;
                        }
                    case "f":  // fail
                        if (port == SpyPort.fail)
                        {
                            PrologIO.WriteLine(lmar + filler + "*** fail command has no effect here");

                            return false;
                        }
                        else
                        {
                            if (!CanBacktrack(true)) throw new AbortQueryException();

                            if (xmlTrace)
                            {
                                XmlTraceWriteElement("FAILED",
                                  (n == INF) ? "Goal failed by user" : String.Format("Retry to level {0} entered by user", level));
                                XmlTraceWriteEnds(leap);
                            }
                            return true;
                        }
                    case "g":  // ancestors
                        ShowAncestorGoals(lmar + filler);
                        break;
                    case "n":  // nodebug
                        SetSwitch("Debugging", ref debug, false);
                        return false;
                    case "a":
                        if (xmlTrace) XmlTraceWriteElement("ABORT", "Session aborted by user");
                        throw new AbortQueryException();
                    case "+":  // spy this
                        goalNode.PredDescr.SetSpy(true, goalNode.Term.Functor, goalNode.Term.Arity, SpyPort.full, false);
                        return false;
                    case "-":  // nospy this
                        goalNode.PredDescr.SetSpy(false, goalNode.Term.Functor, goalNode.Term.Arity, SpyPort.full, false);
                        return false;
                    case ".":  // run to completion
                        rushToEnd = true;
                        return false;
                    case "?":  // help
                    case "h":  // ...
                        string[] help = new string[] {
              "c, CR      creep       Single-step to the next port",
              "l          leap        Resume running, switch tracing off; stop at the next spypoint.",
              "s [<N>]    skip        If integer N provided: skip to Exit or Fail port of level N.",
              "o          out         NOT YET IMPLEMENTED. Skip to the Exit or Fail port of the ancestor.",
              "q          quasi-skip  Same as skip, but will stop if an intermediate spypoint is found.",
              "r [<N>]    retry       Transfer control back to the Call port at level N.",
              "f          fail        Fail the current goal.",
              "g          ancestors   Show ancestor goals.",
              "n          nodebug     Switch the debugger off.",
              "a          abort       Abort the execution of the current query.",
              "+          spy this    Set a spypoint on the current goal.",
              "-          nospy this  Remove the spypoint for the current goal, if it exists.",
              ".          rush        Run to completion without furder prompting.",
              "?, h       help        Show this text."
            };
                        foreach (string line in help)
                            PrologIO.WriteLine(lmar + filler + line);
                        break;
                    default:
                        PrologIO.WriteLine(lmar + filler + "*** Unknown command '{0}' -- enter ? or h for help", cmd);
                        break;
                }
            }
        }


        public void RetryCurrentGoal(int level)
        {
            Object o;

            while (varStack.Count != 0)
            {
                o = varStack.Pop();

                if (o is SpyPoint && ((SpyPoint)o).Port == SpyPort.call)
                {
                    goalNode = ((SpyPoint)o).SaveGoal;
                    //Console.WriteLine ("RetryCurrentGoal found goalNode {0} at level {1}", goalNode, goalNode.Level);

                    if (goalNode.Level == level)
                    {
                        goalNode.FindPredicateDefinition(ps); // nextClause had been forwarded -- reset it
                        //Console.WriteLine ("Picked this goalNode");

                        return;
                    }
                }
                else if (o is Term)
                    ((Term)o).Unbind();
            }
        }


        private void ShowAncestorGoals(string lmar)
        {
            Stack<TermNode> ancestors = new Stack<TermNode>();
            TermNode g;
            int l;
            int lPrev = INF;

            foreach (object o in varStack) // works from the top down to the bottom
            {
                if (o is SpyPoint && ((SpyPoint)o).Port == SpyPort.call)
                {
                    if ((l = (g = ((SpyPoint)o).SaveGoal).Level) < lPrev) // level decreases or stays equal at each step
                    {
                        ancestors.Push(g);
                        lPrev = l;
                    }
                }
            }

            while (ancestors.Count != 0) // revert the order
            {
                g = ancestors.Pop();
                Console.WriteLine(lmar + "{0}>{1}", new String(' ', g.Level), g.Term);
            }
        }


        private void XmlTraceOpen(string tag, int maxEl)
        {
            xmlMaxEl = maxEl;
            xmlElCount = 0;
            xmlTrace = true;
            reporting = true;
            xtw = new XmlTextWriter(xmlFile, null);
            xtw.Formatting = Formatting.Indented;
            xtw.WriteStartDocument();
            xtw.WriteStartElement(tag);
        }


        private void XmlTraceWriteTerm(string tag, string attr, Term term)
        {
            xtw.WriteStartElement(tag);
            if (term != null) xtw.WriteAttributeString(attr, term.ToString());
            xtw.WriteEndElement();
            XmlTraceCheckMaxElement();
        }


        private void XmlTraceWriteElement(string tag, string content)
        {
            xtw.WriteStartElement(tag);
            xtw.WriteString(content);
            xtw.WriteEndElement();
            XmlTraceCheckMaxElement();
        }


        private void XmlTraceCheckMaxElement()
        {
            if (xmlElCount++ < xmlMaxEl) return;

            xtw.WriteStartElement("MAX_EXCEEDED");
            xtw.WriteString(String.Format("Maximum number of elements ({0}) written", xmlMaxEl));
            xtw.WriteEndElement();

            XmlTraceClose();
        }


        private void XmlTraceWriteEnds(int leap)
        {
            for (int i = 0; i < leap; i++) xtw.WriteEndElement();
        }


        private void XmlTraceClose()
        {
            if (!xmlTrace) return;

            xtw.WriteEndElement();
            xtw.WriteEndDocument();
            xtw.Flush();
            xtw.Close();
            PrologIO.Message("XML trace file {0} created", xmlFile);
            xmlFile = null;
            xmlTrace = false;
        }


        public int ElapsedTime() // returns numer of milliseconds since last call
        {
            long prevStartTime = (startTime == -1) ? DateTime.Now.Ticks : startTime;

            return (int)((startTime = DateTime.Now.Ticks) - prevStartTime) / 10000;
        }


        public TimeSpan ProcessorTime() // returns numer of milliseconds since last call
        {
            TimeSpan prevProcTime = (procTime == TimeSpan.MinValue) ? Process.GetCurrentProcess().TotalProcessorTime : procTime;

            return ((procTime = Process.GetCurrentProcess().TotalProcessorTime) - prevProcTime);
        }


        public long ClockTicksMSecs()
        {
            return DateTime.Now.Ticks / 10000;
        }


        public void SetStandardOutput()
        {
            Globals.SetStandardOutput();
        }


        public void TryCloseCurrentOutput()
        {
            Globals.TryCloseCurrentOutput();
        }

        private Ext theExt;
        private TermNode lastCurrClause;

        private bool DoBuiltin(BI biId, out bool findFirstClause)
        {
            findFirstClause = false;
            //Console.WriteLine ("DoBuiltin case {0} current goal \n{1}", biId, goalNode.Term);

            Term term = goalNode.Term;
            Term t0, t1, t2, t3;
            TermSet ts;
            int n, y, m, d, h, s;
            int arity;
            string functor;
            char ch;
            bool result;

            switch (biId)
            {
                case BI.cut:  // !
                    varStack.DisableChoices(term.VarNo);
                    break;

                case BI.fail:
                    return false;

                case BI.or:
                    PrologIO.Error("Serious error -- or-operator (;) not handled properly"); // should not occur
                    return false;

                case BI.consult: // individual file or list of files
                    t0 = term.Arg(0);

                    if (t0.IsList)
                    {
                        int lines = 0;
                        int files = 0;

                        while (t0.Arity == 2)
                        {
                            string fName = Utils.FileNameFromTerm(t0.Arg(0), ".pl");
                            if (fName == null) return false;
                            lines += ps.Consult(fName);
                            files++;
                            t0 = t0.Arg(1);
                        }
                        if (files > 1) PrologIO.Message("Grand total is {0} lines", lines);
                        ps.ResolveIndices();
                        break;
                    }
                    if (t0.IsAtom || t0.IsString)
                    {
                        string fName = Utils.FileNameFromTerm(t0, ".pl");
                        if (fName == null) return false;
                        PrologIO.Write("--- Consulting {0} ... ", fName);
                        ps.Consult(fName);
                        PrologIO.WriteLine("{0} lines read", parser.LineCount);
                        ps.ResolveIndices();
                        break;
                    }
                    return PrologIO.Error("Not a valid file name: '{0}'", t0);

                case BI.asserta:
                    ps.Assert(term.Arg(0), true); // true: at beginning
                    break;

                case BI.assert:
                case BI.assertz:
                    ps.Assert(term.Arg(0), false);
                    break;

                case BI.retract:
                    if (ps.Retract(term.Arg(0), varStack, null))
                        currentCp.ClauseNode = retractClause;
                    else
                    {
                        CanBacktrack(true);
                        return false;
                    }
                    break;

                case BI.retractall: // retractall
                    if (!ps.RetractAll(term.Arg(0), varStack)) return false;
                    break;

                case BI.spy: // leash modes [call, exit, redo, fail]
                case BI.nospy:
                    result = true;
                    t0 = term.Arg(0);

                    if (term.Arity == 2) t3 = term.Arg(1); else t3 = null; // leash list

                    if (t0.Functor == "/" && t0.Arity == 2 && (t1 = t0.Arg(0)).IsAtom && (t2 = t0.Arg(1)).IsInteger)
                        result = ps.SetSpy(term.Functor == "spy", t1.Functor, t2.ExprValue.AsInteger, t3);
                    else if (t0.Arity == 0)
                        result = ps.SetSpy(term.Functor == "spy", t0.Functor, -1, t3);

                    if (!result) return false;

                    if (!debug)
                    {
                        debug = true;
                        PrologIO.Message("Debugging switched on");
                    }
                    break;

                case BI.nospyall:
                    ps.SetNoSpyAll();
                    break;

                case BI.verbose:
                    PrologIO.Verbose = true;
                    break;

                case BI.noverbose:
                case BI.silent:
                    PrologIO.Verbose = false;
                    break;

                case BI.trace:
                case BI.notrace:
                    SetSwitch("Tracing", ref trace, term.Functor == "trace");
                    if (trace) debug = true;
                    reporting = debug || xmlTrace;
                    break;

                case BI.debug:
                case BI.nodebug:
                    SetSwitch("Debugging", ref debug, term.Functor == "debug");
                    reporting = debug || xmlTrace;
                    break;

                case BI.setof_init:
                    term.Arg(0).Unify(new ObjectTerm(new TermSet(DupMode.dupIgnore)), varStack);
                    break;

                case BI.setof_add:
                    ts = (TermSet)((ObjectTerm)term.Arg(0).Value).Value;
                    t1 = term.Arg(1);
                    if (t1.IsVar) return false;
                    t2 = t1.CleanUp();
                    ts.Insert(t2);
                    break;

                case BI.setof_exit:
                    ts = (TermSet)((ObjectTerm)term.Arg(0).Value).Value;
                    if (ts.Count == 0) return false; // setof must fail if the Generator yields no matches
                    term.Arg(1).Unify(ts.ToList(), varStack);
                    break;

                case BI.current_op_init:
                    int pr = -1;
                    string op = null;
                    OType ot = OType.noop;
                    OType ot1;

                    t0 = term.Arg(0);
                    if (t0.IsInteger)
                        pr = Convert.ToInt16(t0.ExprValue.AsInteger);
                    else if (!t0.IsVar)
                        return false;

                    t1 = term.Arg(1);
                    if (t1.IsAtom)
                        try
                        {
                            ot = (OType)Enum.Parse(typeof(OType), t1.Functor);
                        }
                        catch
                        {
                            return false;
                        }
                    else if (!t1.IsVar)
                        return false;

                    t2 = term.Arg(2);
                    if (t2.IsAtom)
                        op = t2.Functor;
                    else if (!t2.IsVar)
                        return false;

                    ArrayList ta = parser.TerminalList;
                    ArrayList oa = new ArrayList();
                    int pr1;
                    string op1;

                    for (int i = 0; i < ta.Count; i++) // expand all operator descriptors into array oa
                    {
                        if (ta[i] is OperatorDescr)
                        {
                            OperatorDescr od = (OperatorDescr)ta[i];

                            if (od.IsDefinedAsPrefix(out op1, out pr1, out ot1) &&
                                 (pr == -1 || pr == pr1) && (op == null || op == op1) && (ot == OType.noop || ot == ot1))
                                oa.Add(new OpRec(pr1, ot1, op1));
                            if (od.IsDefinedAsInfix(out op1, out pr1, out ot1) &&
                                 (pr == -1 || pr == pr1) && (op == null || op == op1) && (ot == OType.noop || ot == ot1))
                                oa.Add(new OpRec(pr1, ot1, op1));
                            if (od.IsDefinedAsPostfix(out op1, out pr1, out ot1) &&
                                 (pr == -1 || pr == pr1) && (op == null || op == op1) && (ot == OType.noop || ot == ot1))
                                oa.Add(new OpRec(pr1, ot1, op1));
                        }
                    }

                    if (oa.Count == 0) return false;

                    term.Arg(3).Bind(new ObjectTerm(oa)); // bind the operator array to the penultimate argument
                    term.Arg(4).Bind(new Term((oa.Count - 1).ToString(), FType.number)); // bind the array count to the last argument
                    break;

                case BI.next_op: // '$next_op'(S, I, P, F, N)
                    t0 = term.Arg(0);
                    oa = (ArrayList)((ObjectTerm)term.Arg(0).Value).Value;
                    t1 = term.Arg(1); // index of current entry in oa
                    n = t1.ExprValue.AsInteger;
                    OpRec oprec = (OpRec)oa[n];
                    term.Arg(1).Functor = (--n).ToString();
                    term.Arg(2).Unify(new Term(oprec.Pr.ToString(), FType.number), varStack);
                    term.Arg(3).Unify(new Term(oprec.Fx, FType.atom), varStack);
                    term.Arg(4).Unify(new Term(oprec.Op, FType.atom), varStack);
                    break;

                case BI.version: // version(V, R)
                    if (!term.Arg(0).Unify(new Term(Utils.MakeAtom(VERSION), FType.atom), varStack)) return false;
                    if (!term.Arg(1).Unify(new Term(Utils.MakeAtom(RELEASE), FType.atom), varStack)) return false;
                    break;

                case BI.halt:
                case BI.quit:
                case BI.abort:
                    halted = true;
                    break;

                case BI.length: // length(L, N)
                    t0 = term.Arg(0);
                    if (t0.HasValue)
                    {
                        if (!t0.IsList) return false;
                        n = 0;
                        while (t0.Arity == 2)
                        {
                            n++;
                            t0 = t0.Arg(1);
                        }
                        if (!term.Arg(1).Unify(new Term(n.ToString(), FType.number), varStack)) return false;
                    }
                    else // create a list with N elements
                    {
                        t1 = term.Arg(1);
                        if (!t1.HasValue) return false;
                        arity = Convert.ToInt16(t1.ExprValue.AsInteger);
                        Term[] args = new Term[arity];
                        t1 = Term.NULLLIST; // []
                        for (int i = 0; i < arity; i++)
                            t1 = new Term(Parser.DOT, new Term(), t1, FType.comp, OType.xfy, 100);
                        t0.Unify(t1, varStack);
                    }
                    break;

                case BI.sort: // sort(L, S)
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);

                    if (t0.IsList)
                    {
                        if (!(t1.IsList || t1.IsVar)) return false;

                        TermSet tlist = new TermSet(t0);
                        tlist.Sort();

                        if (!t1.Unify(tlist.ToList(), varStack)) return false;
                    }
                    else
                        return false;
                    break;

                case BI.functor: // functor(T, F, N)
                    t0 = term.Arg(0);
                    if (t0.HasValue)
                    {
                        if (!term.Arg(1).Unify(new Term(t0.Functor, 0), varStack)) return false;
                        if (!term.Arg(2).Unify(new Term(t0.Arity.ToString(), FType.number), varStack)) return false;
                        break;
                    }
                    else // Term (t0) is unbound
                    {
                        t1 = term.Arg(1);
                        if (t1.HasValue) functor = t1.Functor; else return false;

                        t2 = term.Arg(2);
                        if (t2.HasValue) arity = Convert.ToInt16(t2.ExprValue.AsInteger); else return false;

                        Term[] args = new Term[arity];
                        for (int i = 0; i < arity; i++) args[i] = new Term();
                        if (arity == 0)
                            t2 = (functor == Parser.DOT) ? Term.NULLLIST : new Term(functor);
                        else if (arity > 2)
                            t2 = new Term(functor, args);
                        else
                        {
                            OperatorDescr od = Parser.GetOperatorDescr(functor);

                            if (od == null)
                                t2 = new Term(functor, args);
                            else if (arity == 1 &&
                                      (od.IsDefinedAsPrefix(out functor, out pr, out ot) ||
                                       od.IsDefinedAsPostfix(out functor, out pr, out ot)))
                                t2 = new Term(functor, od, args, ot, pr);
                            else if (arity == 2 && od.IsDefinedAsInfix(out functor, out pr, out ot))
                                t2 = new Term(functor, od, args, ot, pr);
                            else
                                t2 = new Term(functor, args);
                        }
                        if (!t0.Unify(t2, varStack)) return false;
                        break;
                    }

                case BI.arg: // arg( N, Term, A)
                    t1 = term.Arg(1);
                    n = term.Arg(1).NArgs;
                    if (n <= 0) return false;
                    n = Convert.ToInt32(term.Arg(0).ExprValue.AsInteger);
                    Term arg = t1.Arg(n - 1); // N is 1-based
                    if (arg == null || !arg.Unify(term.Arg(2), varStack)) return false;
                    break;

                case BI.abolish: // abolish( X/N)
                    t0 = term.Arg(0);
                    result = true;
                    if (t0.Functor == "/" && t0.Arity == 2 && t0.Arg(0).IsAtom && t0.Arg(1).IsInteger)
                        result = ps.Abolish(t0.Arg(0).Functor, t0.Arg(1).Functor);
                    else
                        result = false;
                    if (!result) return false;
                    break;

                case BI.gensym: // gensym( X)
                    t0 = new Term("v" + gensymInt++, 0);

                    if (t0.Unify(term.Arg(0), varStack))
                        break;
                    else
                        return false;

                case BI.var:
                    if (term.Arg(0).IsVar) break;
                    return false;

                case BI.nonvar:
                    if (term.Arg(0).IsNonVar) break;
                    return false;

                case BI.atom_:
                    if (term.Arg(0).IsAtom) break;
                    return false;

                case BI.atomic:
                    if (term.Arg(0).IsAtomic) break;
                    return false;

                case BI.integer:
                    if (term.Arg(0).IsInteger) break;
                    return false;

                case BI.float_:
                    if (term.Arg(0).IsFloat) break;
                    return false;

                case BI.number:
                    if (term.Arg(0).IsNumber) break;
                    return false;

                case BI.compound:
                    if (term.Arg(0).IsCompound) break;
                    return false;

                case BI.list:
                    if (term.Arg(0).IsList) break;
                    return false;

                case BI.string_:
                    if (term.Arg(0).IsString) break;
                    return false;

                case BI.isdatetime:
                    if (term.Arg(0).IsDateTime) break;
                    return false;

                case BI.istimespan:
                    if (term.Arg(0).IsTimeSpan) break;
                    return false;

                case BI.is_: // X is Y
                    t0 = new Term(term.Arg(1).ExprValue);
                    if (term.Arg(0).Unify(t0, varStack)) break;
                    return false;

                case BI.ne_uni: // X \= Y
                    if (term.Arg(0).Unify(term.Arg(1), varStack)) return false;
                    break;

                case BI.eq_num: // X =:=
                    if (term.Arg(0).ExprValue.AsNumber == term.Arg(1).ExprValue.AsNumber) break;
                    return false;

                case BI.ne_num: // X =\= Y
                    if (term.Arg(0).ExprValue.AsNumber != term.Arg(1).ExprValue.AsNumber) break;
                    return false;

                case BI.lt_num:  // X < Y
                    if (term.Arg(0).ExprValue.AsNumber < term.Arg(1).ExprValue.AsNumber) break;
                    return false;

                case BI.le_num: // X =< Y
                    if (term.Arg(0).ExprValue.AsNumber <= term.Arg(1).ExprValue.AsNumber) break;
                    return false;

                case BI.gt_num: // X > Y
                    if (term.Arg(0).ExprValue.AsNumber > term.Arg(1).ExprValue.AsNumber) break;
                    return false;

                case BI.ge_num: // X >= Y
                    if (term.Arg(0).ExprValue.AsNumber >= term.Arg(1).ExprValue.AsNumber) break;
                    return false;

                case BI.eq_str: // X == Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) == 0) break;
                    return false;

                case BI.ne_str: // X \== Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) != 0) break;
                    return false;

                case BI.lt_ord: // X @< Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) < 0) break;
                    return false;

                case BI.le_ord: // X @=< Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) <= 0) break;
                    return false;

                case BI.gt_ord: // X @> Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) > 0) break;
                    return false;

                case BI.ge_ord: // X @>= Y
                    if (term.Arg(0).CompareTo(term.Arg(1)) >= 0) break;
                    return false;

                case BI.univ: // X =.. Y
                    t0 = term.Arg(0);
                    if (t0.HasValue) // create a list representation of the lhs and unify that with the rhs
                    {
                        functor = t0.Functor;
                        arity = t0.Arity;
                        Term[] args = new Term[arity];
                        t1 = Term.NULLLIST; // []
                        for (int i = arity; i > 0; i--) t1 = new Term(Parser.DOT, t0.Arg(i - 1), t1, FType.comp, OType.xfy, 100); // [arg1, arg2, ...]
                        t1 = new Term(Parser.DOT, new Term(functor), t1, FType.comp, OType.yfx, 0); // [functor, arg1, arg2, ...]
                        if (!t1.Unify(term.Arg(1), varStack)) return false;
                        break;
                    }
                    else // Term is unbound. Create a function representation of the list rhs, and bind that to the lhs
                    {
                        t1 = term.Arg(1);
                        if (!t1.HasValue || !t1.IsList) return false;
                        if (t1.Arg(0) == null) return false; // empty list
                        functor = t1.Arg(0).Functor;
                        // convert rest of list to arguments: calculate arity first
                        t1 = t1.Arg(1);
                        arity = 0;
                        t2 = t1;
                        while (t2.Arity == 2)
                        {
                            arity++;
                            t2 = t2.Arg(1);
                        }
                        // create arguments
                        Term[] args = new Term[arity];
                        for (int i = 0; i < arity; i++)
                        {
                            args[i] = t1.Arg(0);
                            t1 = t1.Arg(1);
                        }
                        t1 = new Term(functor, args);
                        t0.Unify(t1, varStack);
                        break;
                    }

                case BI.unifiable: // X can be unified with Y, but without variable bindings
                    if (!term.Arg(0).Unifiable(term.Arg(1), varStack)) return false;
                    break;

                case BI.see: // see(X)
                    currentInputName = Utils.FileNameFromTerm(term.Arg(0), ".pl");
                    if (currentInputName == null) return false;
                    currentInputReader = new StreamReader(currentInputName);
                    Console.SetIn(currentInputReader);
                    break;

                /*
                        case BI.see: // see(X)  // ORIGINAL VERSION, INTENDED FOR USE WITH read/1
                            try
                            {
                              currentInputName = null;
                              while (seeParserThread.IsAlive)
                              {
                                Utils.WriteLine ("Waiting to abort");
                                seeParserThread.Interrupt ();
                                seeParserThread.Abort ();
                              }
                            }
                            catch {}
                          currentInputName = Utils.FileNameFromTerm (term.Arg (0), ".pl");
                          if (currentInputName == null) return false;
                       Utils.WriteLine ("File {0}", currentInputName);
                          Monitor.Enter (TermMonitor);
                       Utils.WriteLine ("see: Monitor entered");
                          seeParserRun = new ThreadStart (RunSeeParser);
                          seeParserThread = new Thread (seeParserRun);
                          seeParserThread.IsBackground = true; // nodig?
                          seeParserThread.Start ();
                       Utils.WriteLine ("see: Thread started");
                          break;
                */
                case BI.read: // read(X)
                    try
                    {
                        if (currentInputName == null) return false;
                        // Monitor.Enter () was issued in see (F)
                        Monitor.Pulse(TermMonitor);
                        if (Monitor.Wait(TermMonitor, 2000))
                        {
                            t0 = term.Arg(0);
                            if (!t0.Unify(parser/*seeParser*/.ReadTerm, varStack)) return false;
                        }
                        else // time-out
                        {
                            Utils.WriteLine("read: Monitor.Wait () timed out");
                            return false;
                        }
                    }
                    finally
                    {
                        if (parser/*seeParser*/.AtEndOfInput)
                        {
                            currentInputName = null;
                            try { Monitor.Exit(TermMonitor); }
                            catch { }
                        }
                    }
                    break;

                case BI.get0: // get0(C): any character
                    n = (char)Console.ReadKey().KeyChar;
                    if (!term.Arg(0).Unify(new Term(n.ToString(), FType.number), varStack)) return false;
                    break;

                case BI.get: // get( C): skip non-printables
                    do
                    {
                        n = Console.ReadKey().KeyChar;
                        if (!Char.IsControl((char)n)) break; // break if printable
                    } while (true);
                    if (!term.Arg(0).Unify(new Term(n.ToString(), FType.number), varStack)) return false;
                    break;

                case BI.seek: // seek( N, C)
                    n = Convert.ToInt32(term.Arg(0).ExprValue.AsInteger);
                    if (!parser/*seeParser*/.StreamInChar(n, out ch)) return false;
                    if (!term.Arg(0).Unify(new Term(ch.ToString()), varStack)) return false;
                    break;

                case BI.seen: // seen
                    currentInputName = null;
                    if (seeParserThread != null) seeParserThread.Abort();
                    seeParserThread = null;  // implicitly set by Abort?
                    break;

                case BI.tell: // tell( F)
                    if (!Globals.SetCurrentOutput(Utils.FileNameFromTerm(term.Arg(0), ".pl"))) return false;
                    break;

                case BI.write: // write( X)
                    PrologIO.PrologPrint(term.Arg(0));
                    break;

                case BI.writeq: // writeq( X)
                    PrologIO.PrologPrint(term.Arg(0).ToStringQ());
                    break;

                case BI.writeln: // writeln( X)
                    PrologIO.PrologPrint(term.Arg(0));
                    PrologIO.PrologPrint(Environment.NewLine);
                    break;

                case BI.put: // put( C)
                    n = Convert.ToInt32(term.Arg(0).ExprValue.AsInteger);
                    PrologIO.PrologPrint(((char)n).ToString());
                    break;

                case BI.nl:
                    PrologIO.PrologPrint(Environment.NewLine);
                    break;

                case BI.tab: // tab( +N)
                    ////////////////////////// testen op Int
                    n = Convert.ToInt32(term.Arg(0).ExprValue.AsInteger);
                    PrologIO.PrologPrint(new String(' ', n));
                    break;

                case BI.display: // like writeln ( X), but always to standard ouput
                    Globals.SetStandardOutput();
                    PrologIO.PrologPrint(term.Arg(0));
                    PrologIO.PrologPrint(Environment.NewLine);
                    Globals.RevertToCurrentOutput();
                    break;

                case BI.told:
                    Globals.TryCloseCurrentOutput();
                    break;

                case BI.atom_chars: // name( ?A, ?L)
                case BI.name: // name( ?A, ?L)
                    t1 = term.Arg(1);
                    if (t1.HasValue)
                    {
                        FType fType;
                        string a;

                        if (t1.IsList)
                        {
                            StringBuilder sb = new StringBuilder();
                            while (t1.Arity == 2)
                            {
                                t2 = t1.Arg(0);
                                if (!t2.IsInteger) return false;
                                sb.Append((char)t2.ExprValue.AsInteger);
                                t1 = t1.Arg(1);
                            }
                            a = Utils.MakeAtom_ic(sb.ToString(), true, out fType);
                            if (!term.Arg(0).Unify(new Term(a, fType), varStack)) return false;
                        }
                        else if (t1.IsString)
                        {
                            a = Utils.MakeAtom_ic(t1.Functor, true, out fType);
                            if (!term.Arg(0).Unify(new Term(a, fType), varStack)) return false;
                        }
                        else
                            return false;
                    }
                    else // create a list containing A's character codes
                    {
                        t0 = term.Arg(0);
                        if (!t0.IsAtomic) return false;
                        char[] chars = t0.Functor.ToCharArray();
                        t0 = Term.NULLLIST; // []
                        for (int i = chars.Length - 1; i >= 0; i--)
                        {
                            t2 = new Term(((int)chars[i]).ToString(), FType.number);
                            t0 = new Term(Parser.DOT, t2, t0, FType.comp, OType.xfy, 100);
                        }
                        t1.Unify(t0, varStack);
                    }
                    break;

                case BI.expand_term: // expand_term( +(P-->Q), -R)
                    t0 = term.Arg(0); // P-->Q
                    t1 = term.Arg(1); // R
                    Term head = t0.Arg(0);
                    TermNode body = t0.Arg(1).ToDCG(ref head);
                    t2 = new Term(new ClauseNode(head, body)).CleanUp();
                    if (!t1.Unify(t2, varStack)) return false;
                    break;

                case BI.numbervars: // numbervars(+X, +B, -E)
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);
                    t2 = term.Arg(2);
                    if (!t1.IsInteger || t2.HasValue) return false;
                    int k = (int)t1.ExprValue.AsInteger;
                    t0.NumberVars(ref k, varStack);
                    t2.Unify(new Term(k.ToString(), FType.number), varStack);
                    break;

                case BI.writef0: // writef( S, L)
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);
                    if (!t0.IsString) return false;
                    ArrayList al = new ArrayList();
                    while (t1.Arity == 2)
                    {
                        t2 = t1.Arg(0);
                        string a = null;
                        if (t2.Functor == "tree" && t2.Arity == 1)
                            a = t2.Arg(0).ToTree();
                        else if (t2.Functor == "raw" && t2.Arity == 1)
                            a = t2.Arg(0).ToRaw();
                        al.Add((a == null) ? t2.ToString() : a);
                        t1 = t1.Arg(1);
                    }
                    PrologIO.WriteLine(t0.Functor, al.ToArray());
                    break;

                case BI.writef: // writef( S)
                    t0 = term.Arg(0);
                    if (!t0.IsString) return false;
                    PrologIO.WriteLine(t0.Functor);
                    break;

                case BI.username: // username( X)
                    if (!term.Arg(0).Unify(new Term(Environment.UserName), varStack)) return false;
                    break;

                case BI.shell: // shell( X [,Args])
                    t0 = term.Arg(0);
                    if (!(t0.IsString || t0.IsAtom))
                        return false;
                    else if (term.Arity == 1)
                        Process.Start(t0.Functor);
                    else if (!((t1 = term.Arg(1)).IsString || t1.IsAtom))
                        return false;
                    else
                        Process.Start(t0.Functor, Utils.Dequoted(t1.Functor));
                    break;

                /*
                namespace MyProcessSample
                {
                    /// <summary>
                    /// Shell for the sample.
                    /// </summary>
                    class MyProcess
                    {
                        // These are the Win32 error code for file not found or access denied.
                        const int ERROR_FILE_NOT_FOUND =2;
                        const int ERROR_ACCESS_DENIED = 5;

                        /// <summary>
                        /// Prints a file with a .doc extension.
                        /// </summary>
                        void PrintDoc()
                        {
                            Process myProcess = new Process();

                            try
                            {
                                // Get the path that stores user documents.
                                string myDocumentsPath =
                                    Environment.GetFolderPath(Environment.SpecialFolder.Personal);

                                myProcess.StartInfo.FileName = myDocumentsPath + "\\MyFile.doc";
                                myProcess.StartInfo.Verb = "Print";
                                myProcess.StartInfo.CreateNoWindow = true;
                                myProcess.Start();
                            }
                            catch (Win32Exception e)
                            {
                                if(e.NativeErrorCode == ERROR_FILE_NOT_FOUND)
                                {
                                    Console.WriteLine(e.Message + ". Check the path.");
                                }

                                else if (e.NativeErrorCode == ERROR_ACCESS_DENIED)
                                {
                                    // Note that if your word processor might generate exceptions
                                    // such as this, which are handled first.
                                    Console.WriteLine(e.Message +
                                        ". You do not have permission to print this file.");
                                }
                            }
                        }
                */

                case BI.predicatePN: // predicate( +P/N)
                    t0 = term.Arg(0);
                    result = true;
                    if (t0.Functor == "/" && t0.Arity == 2 && t0.Arg(0).IsAtom && t0.Arg(1).IsInteger)
                        result = ps.IsPredicate(t0.Arg(0).Functor, (int)t0.Arg(1).ExprValue.AsInteger);
                    else
                        result = false;
                    if (!result) return false;
                    break;

                case BI.predicateX: // predicate( +T)
                    t0 = term.Arg(0);
                    if (t0.IsVar || !ps.IsPredicate(t0.Functor, t0.NArgs)) return false;
                    break;

#if persistent
        case BI.retract_where: // retract(X) where Y -- for persistent predicates only. No backtracking, delete the whole shebang in one go
          t0 = term.Arg (0).Arg (0); // X
          if (!ps.IsPersistent (t0)) IO.Error ("retract/1: 'where' not allowed on non-persistent predicate {0}", t0.KbKey);
          t1 = term.Arg (1); // Y
          if (!ps.Retract (t0, varStack, t1)) return false;
          break;

        case BI.persistent: // persistent( +P/N)
          t0 = term.Arg (0);
          result = true;
          if (t0.Functor == "/" && t0.Arity == 2 && t0.Arg (0).IsAtom && t0.Arg (1).IsInteger)
            result = ps.IsPersistent (t0.Arg (0).Functor, (int)t0.Arg (1).ExprValue.AsInteger);
          else
            result = false;
          if (!result) return false;
          break;

        case BI.whereXY: // X where Y
          t0 = term.Arg (0); // X
          if (!ps.IsPersistent (t0)) IO.Error ("'where' not allowed on non-persistent predicate {0}", t0.KbKey);
          t1 = term.Arg (1); // Y
          // insert the goal in the argument into the current goalNode
          goalNode = t0.ToGoalList ().Append (goalNode.NextNode);
          goalNode.FindPredicateDefinition (ps, t1);
          return true;

        case BI.persistent_info: // persistent_info( X/N, L) -- give a list with a description of each column
          t0 = term.Arg (0);
          t1 = term.Arg (1);
          t2 = null;

          if (t0.Functor == "/" && t0.Arity == 2 && t0.Arg (0).IsAtom && t0.Arg (1).IsInteger)
            t2 = ps.PersistentInfo (t0.Arg (0).Functor, t0.Arg (1).Functor);
          else
            return false;

          if (!t1.Unify (t2, varStack)) return false; // t1 is output-arg
          break;

        case BI.persistent_uncache: // persistent_uncache( X/N) -- empty persistent predicate X/N's cache
          t0 = term.Arg (0);
          t1 = term.Arg (1);
          PredicateDescr pd;
          if (t0.Functor == "/" && t0.Arity == 2 && t0.Arg (0).IsAtom && t0.Arg (1).IsInteger)
            pd = ps [Term.Key (t0.Arg (0).Functor, t0.Arg (1).Functor)];
          else
            return false;
          if (pd == null || !(pd is PersistentPredDescr))
          {
            IO.Warning ("Predicate '{0}/{1}' was not declared as persistent", t0.Arg (0).Functor, t0.Arg (1).Functor);
            return false;
          }
          ((PersistentPredDescr)pd).InvalidateCache ();
          break;
#endif

                case BI.ground: // ground( +T)
                    if (!term.Arg(0).IsGround()) return false;
                    break;

                case BI.today: // date( ?Y, ?M, ?D)
                    y = DateTime.Today.Year;
                    m = DateTime.Today.Month;
                    d = DateTime.Today.Day;
                    if (!term.Arg(0).Unify(new Term(y.ToString(), FType.number), varStack)) return false;
                    if (!term.Arg(1).Unify(new Term(m.ToString(), FType.number), varStack)) return false;
                    if (!term.Arg(2).Unify(new Term(d.ToString(), FType.number), varStack)) return false;
                    break;

                case BI.now: // time( ?H, ?M, ?S)
                    h = DateTime.Now.Hour;
                    m = DateTime.Now.Minute;
                    s = DateTime.Now.Second;
                    if (!term.Arg(0).Unify(new Term(h.ToString(), FType.number), varStack)) return false;
                    if (!term.Arg(1).Unify(new Term(m.ToString(), FType.number), varStack)) return false;
                    if (!term.Arg(2).Unify(new Term(s.ToString(), FType.number), varStack)) return false;
                    break;

                case BI.validdate: // validdate( +Y, +M, +D)
                    t0 = term.Arg(0);
                    if (t0.IsInteger) y = (int)t0.ExprValue.AsInteger; else return false;
                    t1 = term.Arg(1);
                    if (t1.IsInteger) m = (int)t1.ExprValue.AsInteger; else return false;
                    t2 = term.Arg(2);
                    if (t2.IsInteger) d = (int)t2.ExprValue.AsInteger; else return false;
                    try { new DateTime(y, m, d); }
                    catch { return false; }
                    break;

                case BI.validtime: // validtime( +H, +M, +S)
                    t0 = term.Arg(0);
                    if (t0.IsInteger) h = (int)t0.ExprValue.AsInteger; else return false;
                    t1 = term.Arg(1);
                    if (t1.IsInteger) m = (int)t1.ExprValue.AsInteger; else return false;
                    t2 = term.Arg(2);
                    if (t2.IsInteger) s = (int)t2.ExprValue.AsInteger; else return false;
                    try { new DateTime(2000, 1, 1, h, m, s); }
                    catch { return false; }
                    break;

                case BI.dayname: // dayname( +Y, +M, +D, ?N)
                    DayOfWeek dow;
                    t0 = term.Arg(0);
                    if (t0.IsInteger) y = (int)t0.ExprValue.AsInteger; else return false;
                    t1 = term.Arg(1);
                    if (t1.IsInteger) m = (int)t1.ExprValue.AsInteger; else return false;
                    t2 = term.Arg(2);
                    if (t2.IsInteger) d = (int)t2.ExprValue.AsInteger; else return false;
                    try { dow = new DateTime(y, m, d).DayOfWeek; }
                    catch { return false; }
                    if (!term.Arg(3).Unify(new Term(dow.ToString("G"), FType.text), varStack)) return false;
                    break;

                case BI.dayofweek: // dayofweek( +Y, +M, +D, ?N)
                    t0 = term.Arg(0);
                    if (t0.IsInteger) y = (int)t0.ExprValue.AsInteger; else return false;
                    t1 = term.Arg(1);
                    if (t1.IsInteger) m = (int)t1.ExprValue.AsInteger; else return false;
                    t2 = term.Arg(2);
                    if (t2.IsInteger) d = (int)t2.ExprValue.AsInteger; else return false;
                    try { n = (int)new DateTime(y, m, d).DayOfWeek; }
                    catch { return false; }
                    if (!term.Arg(3).Unify(new Term(n.ToString(), FType.number), varStack)) return false;
                    break;

                case BI.dayofyear: // dayofyear( +Y, +M, +D, ?N)
                    t0 = term.Arg(0);
                    if (t0.IsInteger) y = (int)t0.ExprValue.AsInteger; else return false;
                    t1 = term.Arg(1);
                    if (t1.IsInteger) m = (int)t1.ExprValue.AsInteger; else return false;
                    t2 = term.Arg(2);
                    if (t2.IsInteger) d = (int)t2.ExprValue.AsInteger; else return false;
                    try { n = (int)new DateTime(y, m, d).DayOfYear; }
                    catch { return false; }
                    if (!term.Arg(3).Unify(new Term(n.ToString(), FType.number), varStack)) return false;
                    break;

                case BI.leapyear: // leapyear( +Y)
                    t0 = term.Arg(0);
                    if (t0.IsInteger) y = (int)t0.ExprValue.AsInteger; else return false;
                    if (!DateTime.IsLeapYear(y)) return false;
                    break;

                case BI.weekno: // weekno(+Y, +M, +D, ?N) // week number of date Y-M-D, or current week number
                    if (term.Arity == 4)
                    {
                        t0 = term.Arg(0);
                        if (t0.IsInteger) y = (int)t0.ExprValue.AsInteger; else return false;
                        t1 = term.Arg(1);
                        if (t1.IsInteger) m = (int)t1.ExprValue.AsInteger; else return false;
                        t2 = term.Arg(2);
                        if (t2.IsInteger) d = (int)t2.ExprValue.AsInteger; else return false;
                        try { n = Utils.WeekNo(new DateTime(y, m, d)); }
                        catch { return false; } // invalid date
                    }
                    else
                        n = Utils.WeekNo(DateTime.Today);
                    if (!term.Arg(term.Arity - 1).Unify(new Term(n.ToString(), FType.number), varStack)) return false;
                    break;

                case BI.xml_term: // xml_term( [C,] ?X, ?P) converts between XML and Prolog representation
                    string x;
                    Term ss = null;
                    bool settings = (term.Arity == 3);

                    if (settings)
                    {
                        ss = term.Arg(0);
                        t0 = term.Arg(1);
                        t1 = term.Arg(2);
                    }
                    else
                    {
                        t0 = term.Arg(0);
                        t1 = term.Arg(1);
                    }

                    if (t0.HasValue)
                    {
                        x = t0.Functor;
                        bool inFile = (t0.Arity == 1 && x == "see");  // is it the name of a source file containing the XML structure?
                        bool outFile = (t0.Arity == 1 && x == "tell"); // ... or the name of a destination file containing the XML structure?
                        if (inFile || outFile)
                        {
                            x = Utils.FileNameFromTerm(t0.Arg(0), ".xml");
                            if (x == null) return false;
                        }
                        if (outFile)
                        {
                            if (!t1.HasValue) return false;

                            if (!Node.TermToXml(ss, t1, ref x)) return false;
                        }
                        else
                        {
                            t2 = Node.XmlToTerm(x, inFile);
                            //Console.WriteLine (t2.ToTree ());
                            if (!t1.Unify(t2, varStack)) return false;
                        }
                        break;
                    }
                    else if (t1.HasValue)
                    {
                        x = null;
                        if (!Node.TermToXml(ss, t1, ref x)) return false;
                        t2 = new Term(x, FType.text);
                        if (!t0.Unify(t2, varStack)) return false;
                        break;
                    }
                    else
                        return false;

                case BI.listing: // listing
                    if (!ps.ListAll(null, -1, false, true)) return false; // i.e. no predefined, all user
                    break;

                case BI.listingXN: // listing( X/N)
                    t0 = term.Arg(0);
                    result = true;
                    if (t0.Functor == "/" && t0.Arity == 2 && t0.Arg(0).IsAtom && t0.Arg(1).IsInteger)
                        result = ps.ListAll(t0.Arg(0).Functor, t0.Arg(1).ExprValue.AsInteger, false, true);
                    else
                        result = false;
                    if (!result) return false;
                    break;

                case BI.listingX: // listing( X) -- list all predicates X/N (i.e. for each N)
                    t0 = term.Arg(0);
                    if (!t0.IsAtom) return false;
                    if (!ps.ListAll(t0.Functor, -1, false, true)) return false;
                    break;

                case BI.listing0: // listing0
                    if (!ps.ListAll(null, -1, true, false)) return false; // i.e. no user, all predefined
                    break;

                case BI.listing0XN: // listing0( X/N)
                    t0 = term.Arg(0);
                    result = true;
                    if (t0.Functor == "/" && t0.Arity == 2 && t0.Arg(0).IsAtom && t0.Arg(1).IsInteger)
                        result = ps.ListAll(t0.Arg(0).Functor, t0.Arg(1).ExprValue.AsInteger, true, false);
                    else
                        result = false;
                    if (!result) return false;
                    break;

                case BI.listing0X: // listing0( X)
                    t0 = term.Arg(0);
                    if (!t0.IsAtom) return false;
                    if (!ps.ListAll(t0.Functor, -1, true, false)) return false;
                    break;

                case BI.pp_defines: // pp_defines( X) -- preprocessor symbol definitions -- mainly useful for debugging in nested calls
                    t0 = term.Arg(0);
                    if (!t0.IsVar) return false;
                    t1 = Term.NULLLIST; // []
                    //Console.WriteLine ("Parser.PpSymbols.count = {0}", Parser.PpSymbols.Count);
                    foreach (DictionaryEntry de in Parser.PpSymbols)
                        t1 = new Term(Utils.MakeAtom(de.Key as string), new Term(), t1, FType.comp, OType.xfy, 100);
                    t0.Unify(t1, varStack);
                    break;

                case BI.undefineds:
                    ps.FindUndefineds();
                    break;

                case BI.copy_term: // copy_term( X, Y)
                    if (!term.Arg(1).Unify(term.Arg(0).CleanCopy(), varStack)) return false;
                    break;

                case BI.undef_pred_action: // undef_pred_action( X/1, A)
                    t0 = term.Arg(0);
                    t1 = term.Arg(1);
                    t2 = new Term(Parser.COMMA, t0, t1, FType.comp, OType.xfy, 1000);
                    if (!ps.SetUndefPredAction(t2, false)) return false;
                    break;

                case BI.clearall: // clearall
                    ReadBuiltinPredicates();
                    break;

                case BI.spypoints: // spypoints
                    ps.ShowSpypoints();
                    break;

                case BI.clause: // clause(Head,Body)
                    t0 = term.Arg(0); // head
                    t1 = term.Arg(1); // body

                    if (!t0.HasValue)
                    {
                        // head is not instantiated --> no predicate specified
                        // changed from Console.Writeln to general warning LI 2009/10/07
                        PrologIO.Warning("ERROR: Arguments are not sufficiently instantiated for clause/2.");
                        return false;
                    }

                    // find predicate
                    PredicateDescr pdsc = ps[t0.KbKey];
                    if (pdsc == null) return false;

                    // get current clause for it
                    TermNode tn = ((lastCp != null) && (lastCp is ClauseChoicePoint)) ?
                      ((ClauseChoicePoint)lastCp).NextClause : pdsc.GetClauseList(t0, null);

                    if (tn == null) return false;

                    // prepare for redo //maybe only needed if further tn clause existent?
                    varStack.Push(new ClauseChoicePoint(goalNode, goalNode.NextClause, tn.NextClause));
                    findFirstClause = true;

                    // unify t0 with the head and t1 with the clause
                    if (!tn.Term.Unify(t0, varStack))
                        return false;

                    TermNode tm = tn.NextNode;

                    if ((tm == null) || (tm.BuiltinId != BI.none))
                    {
                        // No body for the clause. Either a fact (unify t1 with TRUE) or a buildin predicate.
                        if (!t1.Unify(new Term(Utils.MakeAtom(tm == null ? "true" : "builtin predicate"),
                            FType.atom), varStack)) return false;
                    }
                    else if (tm.NextNode == null)
                    {
                        // body consists of one single goal.
                        if (!tm.Term.Unify(t1, varStack)) return false;
                    }
                    else
                    {
                        t0 = t0.TermSeq(tm);

                        if (!t0.Unify(t1, varStack)) return false;
                    }
                    break;

                case BI.member: // member( X, L, Rest)
                    if (!(t0 = term.Arg(0)).HasValue || !(t1 = term.Arg(1)).IsList) return false;

                    result = false;

                    while (t1.Arity == 2)
                    {
                        if (result = t0.Unify(t1.Arg(0), varStack)) break;

                        t1 = t1.Arg(1);
                    }
                    currentCp.Kill(); // no backtracking to follow -> remove the choicepoint for the alternative clauses
                    if (!result) return false;
                    break;

                // BACKTRACKING VERSION
                //          while (t1.Arity == 2)
                //          {
                //            if (result = t0.Unify (t1.Arg (0), varStack))
                //            {
                //              if ((t0 = t1.Arg (1)).Arity == 0) // empty list
                //                currentCp.Kill (); // no backtracking to follow -> remove the choicepoint for the alternative clauses
                //              else
                //                term.Arg (2).Bind (t0);  // set Rest to remainder of list (for backtracking)
                //
                //              break;
                //            }
                //            t1 = t1.Arg (1);
                //          }

                case BI.append: // append( [_|_], [_|_], L)
                    if (!(t0 = term.Arg(0)).IsList || !(t1 = term.Arg(1)).IsList) return false;

                    currentCp.Kill(); // no backtracking to follow -> remove the choicepoint for the alternative clauses

                    if (!term.Arg(2).Unify(t0.AppendList(t1), varStack)) return false;
                    break;

                case BI.match_regex: // regex( +Source, +Pattern, ?Result, ?Options)
                    if (!((t0 = term.Arg(0)).IsString || t0.IsAtom) || !((t1 = term.Arg(1)).IsString || !t1.IsAtom)) return false;

                    Match[] matches = Utils.FindRegexMatches(t0.Functor, t1.Functor, false);

                    if (matches.Length == 0) return false;

                    //foreach (Match mt in matches) Console.WriteLine ("index {0} match {1}", mt.Index, mt.Value);

                    t2 = Term.NULLLIST; // []
                    bool asAtom = true;

                    for (int i = matches.Length - 1; i >= 0; i--)
                        t2 = new Term(Parser.DOT, Term.MakeMatchTerm(matches[i], asAtom), t2, FType.comp, OType.xfy, 100);

                    if (!term.Arg(2).Unify(t2, varStack)) return false;
                    break;

                case BI.xmltrace: // xmltrace( X) -- send the execution tree of the next query to file X
                    // Must be the first goal of a query, in order to avoid problems that
                    // arise when it gets involved in backtracking.
                    if (xmlTrace)
                        PrologIO.Error("Execution trace is already being logged in {0}", xmlFile);
                    else if (!(t0 = term.Arg(0)).IsAtom && !t0.IsString)
                        PrologIO.Error("Not a valid file name: '{0}'", t0);
                    else if (!firstGoal)
                    {
                        PrologIO.WriteLine("{0}*** {1}/1/2 must be the first goal of the query -- ignored{0}",
                                      Environment.NewLine, term.Functor);
                        break;
                    }
                    else if ((xmlFile = Utils.FileNameFromTerm(t0, ".xml")) == null)
                        return false;

                    n = (term.Arity == 2) ? term.Arg(1).ExprValue.AsInteger : INF;

                    if (n < 1) PrologIO.Error("Maximum number of elements must exceed 0");

                    XmlTraceOpen(term.Functor, n);
                    break;

                case BI.numcols: // numcols( N) -- number of columns in the DOS-box
#if mswindows
                    if (!term.Arg(0).Unify(new Term(Utils.NumCols.ToString(), FType.number), varStack)) return false;
                    break;
#else
          return false;
#endif

                case BI.userroles:
#if mswindows
                    WindowsIdentity ident = WindowsIdentity.GetCurrent();
                    WindowsPrincipal principal = new WindowsPrincipal(ident);
                    //bool admin = principal.IsInRole (WindowsBuiltInRole.Administrator);
                    PrologIO.Message("{0} belongs to: ", principal.Identity.Name.ToString());

                    Array wbirFields = Enum.GetValues(typeof(WindowsBuiltInRole));

                    foreach (object roleName in wbirFields)
                    {
                        try
                        {
                            PrologIO.Message("{0}? {1}.", roleName, principal.IsInRole((WindowsBuiltInRole)roleName));
                        }
                        catch (Exception)
                        {
                            PrologIO.Message("Could not obtain role for RID {0}", roleName);
                        }
                    }
#else
          PrologIO.Error ("userroles only available if compiler symbol mswindows is defined");
#endif
                    break;

                case BI.statistics: // statistics( X, [MSec,_]) // this version actually only returns the current time
                    t1 = term.Arg(1).Arg(0);
                    long time = ClockTicksMSecs();
                    if (!t1.Unify(new Term(time.ToString(), FType.number), varStack)) return false;
                    break;

                case BI.environment: // environment( X, Y) -- unify Y with atom value of environment variable X
                    t0 = term.Arg(0);
                    if (!t0.IsAtom) return false;
                    string es;

                    switch (t0.Functor.ToLower())
                    {
                        case "applicationdata":
                            es = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
                            break;
                        case "localapplicationdata":
                            es = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
                            break;
                        case "cookies":
                            es = Environment.GetFolderPath(Environment.SpecialFolder.Cookies);
                            break;
                        case "desktopdirectory":
                            es = Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory);
                            break;
                        case "internetcache":
                            es = Environment.GetFolderPath(Environment.SpecialFolder.InternetCache);
                            break;
                        case "programfiles":
                            es = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);
                            break;
                        case "startup":
                            es = Environment.GetFolderPath(Environment.SpecialFolder.Startup);
                            break;
                        case "commandline":
                            es = Environment.CommandLine;
                            break;
                        case "currentdirectory":
                            es = Environment.CurrentDirectory;
                            break;
                        case "machinename":
                            es = Environment.MachineName;
                            break;
                        case "newline":
                            es = Environment.NewLine;
                            break;
                        case "osversion":
                            es = Environment.OSVersion.ToString();
                            break;
                        case "stacktrace":
                            es = Environment.StackTrace;
                            break;
                        case "systemdirectory":
                            es = Environment.SystemDirectory;
                            break;
                        case "tickcount":
                            es = Environment.TickCount.ToString();
                            break;
                        case "userdomainname":
                            es = Environment.UserDomainName;
                            break;
                        case "userinteractive":
                            es = Environment.UserInteractive.ToString();
                            break;
                        case "username":
                            es = Environment.UserName;
                            break;
                        case "version":
                            es = Environment.Version.ToString();
                            break;
                        case "workingset":
                            es = Environment.WorkingSet.ToString();
                            break;
                        default:
                            return false;
                    }
                    if (!term.Arg(1).Unify(new Term(es), varStack)) return false;
                    break;

                case BI.query_timeout:
                    t0 = term.Arg(0);
                    if (t0.IsVar)
                    {
                        t0.Unify(new Term(queryTimeout.ToString(), FType.number), varStack);

                        break;
                    }
                    if (!t0.IsInteger || (queryTimeout = t0.ExprValue.AsInteger) < 0) return false;
                    break;

                case BI.inc_counter: // inc_counter( N) -- value of N is increased at each redo!!
                    t0 = term.Arg(0);
                    if (!t0.IsInteger) return false;
                    t0.Functor = (t0.ExprValue.AsInteger + 1).ToString();
                    break;

                //        case BI.vvv:
                //          PredicateDescr p = ps [Term.Key ("value", 2)];
                //          p.DumpClauseList ();
                //          break;

                case BI.jcall0: // jcall0(+ObjOfClass,+MemberName,+ArgsList,?Result)
                    if (!Ext.JCALL0(term, this)) return false;
                    break;
                case BI.jpred0: // jcall0(+ObjOfClass,+MemberName,+ArgsList,?Result)
                    if (!theExt.JPRED0(term, this)) return false;
                    break;

                default:
                    return PrologIO.Error("Undefined built-in: {0}", term);
            }

            goalNode = goalNode.NextNode;

            if (reporting) // advance the goalList until a non-spypoint is encountered. Show spy(-exit)-info.
            {
                TermNode sp = null;

                while (goalNode is SpyPoint)
                {
                    sp = ((SpyPoint)goalNode).SaveGoal;
                    if (Debugger(SpyPort.exit, sp, null, false, 1)) break;
                    goalNode = sp.NextNode;
                }
            }

            findFirstClause = true;

            return true;
        }


        private void SetSwitch(string switchName, ref bool switchVar, bool mode)
        {
            bool current = switchVar;

            if (current == (switchVar = mode))
                PrologIO.Message("{0} already {1}", switchName, (mode ? "on" : "off"));
            else
                PrologIO.Message("{0} switched {1}", switchName, (mode ? "on" : "off"));
        }

    }
}
