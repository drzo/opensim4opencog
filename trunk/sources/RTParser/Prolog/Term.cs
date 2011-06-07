#if (!STANDARD)
#define debugging
#define arg1index
#define mswindows
#define newor
#define partialengine
#endif

#if (!VISUAL_STUDIO)
#undef mswindows
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

using System;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections;
using System.Collections.Specialized;
using System.Collections.Generic;

namespace RTParser.Prolog
{
    // Order of FType is important for term comparison. See CompareTo below
    public enum FType { var, number, atom, text, datetime, timespan, boolean, comp, dcg, opr, vmobj, }   // functor types

    /*
      ----
      Term
      ----
    */

    public class Term : IComparable
    {
        static readonly string DefaultDateFormat = "yyyy-MM-dd HH:mm:ss";

        protected short arity = 0;
        protected int varNo = 0;
        private int verNo = 0;     // used for CleanCopy
        private Term newVar = null; // used for CleanCopy
        protected object uLink;
        protected Term ULink { get { return (Term)uLink; } set { uLink = value; } }
        protected FType fType;
        private OType oType = OType.noop;
        private OperatorDescr oDescr = null;
        protected short precedence;
        protected bool hasValue = true;  // if true: term is bound to a value ('nonvar'), otherwise its value is not set ('var')
        protected bool isUnified = false; // if true: term is unified with some other term (connected via ULink chain)
        protected bool isPacked = false; // for tree construction, set if tree is in parentheses (=> prec = 0)
        protected Term[] args;
        public object _functor; // not to be accessed directly
        protected string functor
        {
            get
            {
                if (_functor == null) return null;

                switch (fType)
                {
                    case FType.number: return _functor.ToString().Replace(',', '.');
                    case FType.datetime: return "'" + ((DateTime)_functor).ToString(DefaultDateFormat) + "'";
                    case FType.timespan: return "'" + ((TimeSpan)_functor).ToString() + "'";
                    case FType.vmobj: return "'" + ((object)_functor).ToString() + "'";
                    default: return _functor.ToString();
                }
            }
            set { _functor = value; }
        }
        public string FunctorU
        {
            get
            {
                Term t = this;
                while (t.isUnified) t = t.ULink;
                var ff = t._functor;
                if (ff == null)
                {
                    return null;
                }
                switch (fType)
                {                                           
                    case Prolog.FType.atom:
                    case Prolog.FType.comp:
                        string typeName = "" + ff;
                        if (typeName[0] == '\'')
                        {
                            typeName = typeName.Substring(1, typeName.Length - 2);
                        }
                        return typeName;
                        break;
                    case FType.number: return ff.ToString().Replace(',', '.');
                    default: return ff.ToString();
                }
            }
        }

        public Term LinkEnd { get { Term t = this; while (t.isUnified) t = t.ULink; return t; } }
        public bool IsPacked { set { isPacked = value; } get { return isPacked; } }
        public bool IsUnified { get { return isUnified; } }
        public FType FType { get { return LinkEnd.fType; } }
        public OType OType { get { return LinkEnd.oType; } }
        public OperatorDescr ODescr { get { return oDescr; } } // no LinkEnd required for the current purpose
        public int Precedence { get { return LinkEnd.precedence; } }
        public int VarNo { get { return varNo; } }
        public bool HasValue { get { return LinkEnd.hasValue; } } // nb: if t.HasValue does *not* hold, t.hasValue can be true anyway !!!
        public Term[] Args { get { return LinkEnd.args; } set { args = value; arity = (short)args.Length; } }

        // statics
        private static int varNoMax = 0;
        private static int verNoMax = 0;
        public static int unboundVarCount;
        public static Term NULLLIST;
        public static Term TREEROOT;
        private const string NUMVAR = "'$VAR'";
        private const string ROOT = "true";
        public static Term EOF = new Term(Parser.EOF);
        public static Term FAIL = new Term("fail");

        public static Term TRUE = new Term(Utils.MakeAtom("@true"), Prolog.FType.boolean);
        public static Term FALSE = new Term(Utils.MakeAtom("@false"), Prolog.FType.boolean);
 
        private static Random rnd = new Random();
        public static bool trace = false;
        private static Dictionary<Term, Term> varDict = new Dictionary<Term, Term>();

        public bool IsVar
        { get { return (!LinkEnd.hasValue); } }

        public bool IsNonVar
        { get { return (LinkEnd.hasValue); } }

        public bool IsGoal
        { get { return (FType == FType.atom || FType == FType.text || FType == FType.comp || this is Cut); } }

        public bool IsAtom
        { get { return (Arity == 0 && FType != FType.number && FType != FType.text); } }

        public bool IsAtomic
        { get { return (Arity == 0); } }

        public bool IsInteger
        { get { return (FType == FType.number && Decimal.Remainder(this.ExprValue.AsNumber, 1) == 0); } }

        public bool IsFloat
        { get { return (FType == FType.number && Decimal.Remainder(this.ExprValue.AsNumber, 1) != 0); } }

        public bool IsNumber
        { get { return (FType == FType.number); } }

        public bool IsDateTime
        { get { return (FType == FType.datetime); } }

        public bool IsTimeSpan
        { get { return (FType == FType.timespan); } }

        public bool IsObjectRef
        { get { return (FType == FType.vmobj); } }

        public bool IsObject
        {
            get
            {
                if (IsObjectRef || IsString || IsNumber || IsDateTime || IsTimeSpan) return true;
                if (arity == 0 && !(_functor is string))
                {
                    return true;
                }
                return false;
            }
        }

        public bool IsCompound
        { get { return (FType == FType.comp); } }

        public bool IsList
        { get { return (Functor == Parser.DOT); } }

        public bool IsString
        { get { return (FType == FType.text); } }

        public bool IsBool
        { get { return (FType == FType.boolean); } }

        public bool IsCut
        { get { return (this is Cut); } }


        public string KbKey
        {
            get
            {
                Term t = this;
                while (t.isUnified) t = t.ULink;

                return t.arity + t.functor; // MUST CORRESPOND EXACTLY TO Key (string f, string a) BELOW
            }
        }


        public static string Key(string f, string a)
        {
            return a + f;
        }


        public static string Key(string f, int a)
        {
            return a + f;
        }


        public string Index // for readability only
        {
            get
            {
                Term t = this;
                while (t.isUnified) t = t.ULink;

                return t.functor + "/" + t.arity;
            }
        }


        public Term Value
        {
            get
            {
                Term t = LinkEnd;
                return t.hasValue ? t : null;
            }
        }


        static Term()
        {
            NULLLIST = new Term(Parser.DOT, FType.comp);
            TREEROOT = new Term(ROOT, FType.comp);
            rnd = new Random();
            varDict = new Dictionary<Term, Term>();
        }


        public Term() // create an unbound variable
        {
            hasValue = false;
            varNo = varNoMax++;
        }


        public Term(object _f, short ar, FType ft, OType ot, short prec) // used only bye CleanCopy and CleanUpEx
        {
            _functor = _f;
            arity = ar;
            args = new Term[arity];
            fType = ft;
            oType = ot;
            precedence = prec;
        }


        public Term(int i) // create var with specified varNo; used by CleanUp() only for neatly renumbering from 0
        {
            hasValue = false;
            varNo = i;
        }


        public Term(string s, FType ft)
        {
            if (ft == FType.number)
                _functor = Decimal.Parse(s, Globals.CI);
            else
                _functor = s;

            fType = ft;
        }


        public Term(OperandValue e)
        {
            _functor = e.Value;
            fType = e.FType;
        }


        public Term(DateTime dt)
        {
            _functor = dt;
            fType = FType.datetime;
        }


        public Term(TimeSpan ts)
        {
            _functor = ts;
            fType = FType.timespan;
        }

        public Term(Object obj,Type type)
        {
            _functor = obj;
            fType = FType.vmobj;
        }

        public Term(string s, Term[] a)
        {
            _functor = s;
            args = a;
            arity = (short)a.Length;
            fType = (arity == 0) ? FType.atom : FType.comp;
        }


        public Term(string s, Term a)
        {
            _functor = s;
            arity = 1;
            args = new Term[arity];
            args[0] = a;
            fType = FType.comp;
        }


        public Term(string s, OperatorDescr od, Term[] a, OType ot, int prec)
        {
            _functor = s;
            args = a;
            arity = (short)a.Length;
            oDescr = od;
            fType = (arity == 0) ? FType.atom : FType.comp;
            oType = ot;
            precedence = (short)prec;
        }


        public Term(string s)
        {
            _functor = s;
            fType = FType.atom;
        }


        public Term(string op, Term a1, Term a2, FType ft, OType ot, int prec)
        {
            _functor = op;
            args = new Term[2];
            args[0] = a1;
            args[1] = a2;
            arity = 2;
            fType = ft; // comp or dcg
            oType = ot;
            precedence = (short)prec;
        }


        public Term(string op, Term t, OType ot, int prec)
        {
            _functor = op;
            args = new Term[1];
            args[0] = t;
            arity = 1;
            fType = FType.comp;
            oType = ot;
            precedence = (short)prec;
        }


        public Term(string op, OType ot, int prec)
        {
            _functor = op;
            fType = FType.atom;
            oType = ot;
            precedence = (short)prec;
        }


        public Term(OperatorDescr od) // Term consisting of an operator only (used at Term construction time only)
        {
            string op;
            int pr;
            OType ot;

            if (od.IsDefinedAsInfix(out op, out pr, out ot))
            {
                _functor = od.Name;
                arity = 2;
                oType = ot;
                oDescr = od;
                precedence = (short)pr;
            }
            else if (od.IsDefinedAsPrefix(out op, out pr, out ot))
            {
                _functor = od.Name;
                arity = 1;
                oType = ot;
                oDescr = od;
                precedence = (short)pr;
            }
            else if (od.IsDefinedAsPostfix(out op, out pr, out ot))
            {
                _functor = od.Name;
                arity = 1;
                oType = ot;
                oDescr = od;
                precedence = (short)pr;
            }
            fType = FType.opr;
        }


        public Term(string op, Term[] terms, FType ft, OType ot, int prec)
        {
            _functor = op;
            arity = (short)terms.Length;
            args = terms;
            fType = ft;
            oType = ot;
            precedence = (short)prec;
        }


        public Term(ClauseNode c)  // Create a Term from a ClauseNode (= Head + Body)
        {
            if (c.NextNode == null) // fact
            {
                Term t = c.Term;
                _functor = t._functor;
                arity = t.arity;
                args = t.args;
                varNo = t.varNo;
                ULink = t.ULink;
                fType = t.fType;
                oType = t.oType;
                oDescr = t.oDescr;
                precedence = t.precedence;
                hasValue = t.hasValue;
                isUnified = t.isUnified;
                isPacked = t.isPacked;
            }
            else
            {
                _functor = Parser.IMPLIES;
                arity = 2;
                args = new Term[2];
                args[0] = c.Term;
                fType = FType.comp;
                oType = OType.xfx;
                args[1] = TermSeq(c.NextNode);
                precedence = 1200;
            }
        }


        public string VarName()
        {
            return Globals.VariableName(this);
        }


        public Term[] ArgumentsToTermArray()
        {
            // Get required array size
            int n = 1;
            Term t = this;

            if (t.Arity == 0 && t.Functor == Parser.COMMA) return new Term[1] { t }; // special case

            while (t.Functor == Parser.COMMA && !t.IsPacked)
            {
                n++;
                t = t.Arg(1); // xfy
            }

            Term[] terms = new Term[n];
            terms[n - 1] = t;
            n = 0;
            t = this;

            while (t.Functor == Parser.COMMA && !t.IsPacked) // in order to distinguish a,b,c from a,(b,c)
            {
                terms[n++] = t.Arg(0);
                t = t.Arg(1);
            }

            return terms;
        }


        public ArrayList AlternativesToArrayList()
        {
            Term t = this;
            ArrayList a = new ArrayList();

            while (t.Functor == Parser.SEMI && t.Arity == 2)
            {
                a.Add(t.Arg(0));
                t = t.Arg(1); // xfy
            }
            a.Add(t);

            return a;
        }


        public ArrayList ArgumentsToArrayList(bool flatten)
        {
            Term t = this;
            ArrayList a = new ArrayList();

            while (t.Functor == Parser.COMMA && (flatten || !t.IsPacked) && t.Arity == 2)
            {
                if (flatten)
                    a.AddRange(t.Arg(0).ArgumentsToArrayList(true));
                else
                    a.Add(t.Arg(0));

                t = t.Arg(1); // xfy
            }
            a.Add(t);

            return a;
        }


        private bool readingDCGClause = false;


        public TermNode ToGoalList() // called during consulting
        {
            return ToGoalList(0, 0);
        }


        public TermNode ToGoalList(int stackTop, int level) // called during execution (when there is a stack)
        {
            TermNode result = null;
            Term t0, t1;

            FType ft = this.FType;
            OType ot = this.OType;
            int pr = this.Precedence;

            if (this is Cut)
            {
                if (stackTop == 0)
                    return new TermNode(this, level);
                else
                    return new TermNode(new Cut(stackTop), level);
            }

            switch (this.Functor)
            {
                case Parser.IMPLIES:
                    t0 = Arg(0);
                    if (!t0.IsGoal)
                        PrologIO.Error("Illegal predicate head: {0}", t0);
                    t1 = Arg(1);
                    result = new TermNode(t0, t1.ToGoalList(stackTop, level));
                    break;
                case Parser.DCGIMPL:
                    t0 = Arg(0);
                    if (!t0.IsGoal) PrologIO.Error("Illegal DCG head: {0}", t0);
                    t1 = Arg(1);
                    result = new TermNode(t0, t1.ToGoalList(stackTop, level));
                    break;
                case Parser.COMMA:
                    t0 = Arg(0);
                    t1 = Arg(1);
                    result = t0.ToGoalList(stackTop, level);
                    result.Append(t1.ToGoalList(stackTop, level));
                    break;
                case Parser.DOT:
                    t0 = Arg(0);
                    t1 = Arg(1);
                    if (readingDCGClause)
                        result = (new ListTerm(t0, t1)).ToGoalList(stackTop, level);
                    else
                        result = (new Term("consult", new ListTerm(t0, t1), OType.fy, 0)).ToGoalList(stackTop, level);
                    break;
                case Parser.CURL:
                    if (readingDCGClause)
                    {
                        t0 = Arg(0);
                        result = t0.ToGoalList(stackTop, level);
                    }
                    else
                        PrologIO.Error("Curly brackets only allowed in a DCG-clause: {0}", this);
                    break;
                default:
                    if (this.IsVar)
                        result = new TermNode(new Term("'$metacall'", this), level);
                    else if (this.IsGoal)
                        result = new TermNode(this, level);
                    else
                        PrologIO.Error("Illegal term {0} in goal list", this);
                    break;
            }
            return result;
        }


        public Term TermSeq(TermNode list)
        {
            if (list.NextNode == null) // last Term of TermNode
                return list.Term;
            else
                return new Term(Parser.COMMA, list.Term, TermSeq(list.NextNode), FType.comp, OType.xfy, 1000);
        }


        public Term Bind(Term t)
        {
            if (this == t) return this; // binding to self should have no effect

            if (hasValue)
                PrologIO.Error("Term.Bind (_{0}) -- Cannot bind nonvar (bound to {1})", this.VarNo, ULink);
            else
            {
                hasValue = true;
                isUnified = true;
                ULink = t;
            }
            return this;
        }


        public void Unbind()
        {
            hasValue = false;
            isUnified = false;
            ULink = null;
        }


        /// <summary>Retrieves an argument of a term </summary>
        public Term Arg(int pos)
        {
            if (hasValue)
            {
                if (isUnified)
                    return ULink.Arg(pos);
                else if (pos < 0 || pos > args.Length - 1)
                    return null;
                else
                    return args[pos];
            }
            else
            {
                PrologIO.Fatal("Term.Arg Error - Arg({0}) does not exist (term is unbound)", pos);

                return null;
            }
        }


        private void SetArg(int pos, Term t)
        {
            if (hasValue)
            {
                if (isUnified)
                    ULink.SetArg(pos, t);
                else if (pos < 0 || pos > args.Length - 1)
                    PrologIO.Fatal("Term.SetArg Error - KbKey out of range");
                else
                    args[pos] = t;
            }
            else
                PrologIO.Fatal("Term.Arg Error - SetArg({0}) failed, term is unbound", pos);
        }


        /// <summary>Retrieves the number of arguments of a term </summary>
        private int nArgs()
        {
            if (hasValue)
            {
                if (isUnified)
                    return ULink.nArgs();
                else
                    return (args == null) ? 0 : args.Length;
            }
            else
                return -1;
        }


        public int NArgs
        {
            get { return nArgs(); }
        }


        public string Functor
        {
            get
            {
                Term t = this;
                while (t.isUnified) t = t.ULink;
                return t.functor;
            }
            set
            {
                Term t = this;
                while (t.isUnified) t = t.ULink;
                t.functor = value;
            }
        }


        public int Arity
        {
            get
            {
                if (hasValue)
                    return isUnified ? ULink.Arity : arity;
                else
                    return -1;
            }
        }


        // Term parsing

        private static byte[,] ActionCode = new byte[5, 5] {
       /*           A   Pr  In  Po  EOT */
       /* Anop  */ {01, 99, 03, 00, 00}, // Anop: atom, but not an operator
       /* Pref  */ {06, 07, 08, 09, 10},
       /* Inf   */ {11, 12, 13, 14, 98},
       /* Postf */ {99, 99, 18, 19, 00},
       /* BOT   */ {00, 00, 23, 24, 99}
       };
        private static byte[,] ActionCodeEx = new byte[7, 7] {
      /*         fx  fy  xf  yf  xfx xfy yfx */
      /* fx  */ {00, 01, 02, 03, 04, 05, 06},
      /* fy  */ {07, 08, 09, 10, 11, 12, 13},
      /* xf  */ {14, 15, 16, 17, 18, 19, 20},
      /* yf  */ {21, 22, 23, 24, 25, 26, 27},
      /* xfx */ {28, 29, 30, 31, 32, 33, 34},
      /* xfy */ {35, 36, 37, 38, 39, 40, 41},
      /* yfx */ {42, 43, 44, 45, 46, 47, 48}
      };
        private static ArrayList termPrefixArray = new ArrayList();
        private static Stack<Term> operatorStack = new Stack<Term>();

        public enum eRMA { Anop, Pref, Inf, Postf, BE }

        private static eRMA TermType(Term t)
        {
            if (t == null)
                return eRMA.BE;
            else if (t.fType != FType.opr)
                return eRMA.Anop;
            else if (OperatorDescr.TypeToGroup(t.OType) == OGroup.fz)  // prefix
                return eRMA.Pref;
            else if (OperatorDescr.TypeToGroup(t.OType) == OGroup.zfz) // infix
                return eRMA.Inf;
            else if (OperatorDescr.TypeToGroup(t.OType) == OGroup.zf)  // postfix
                return eRMA.Postf;
            else
                return eRMA.BE;
        }


        public static void ToAnop(ref Term t)
        {
            t.fType = FType.comp;
            t.oType = OType.noop;
            t.arity = 0;
            t.precedence = 0;
        }


        public static void AnalyzeInput(ref ArrayList termInfixArr, ref Term lastInfix) // cf. Side Notes SN1
        {
            // lastInfix is set equal to the most recent term if that is an infix, and set to null otherwise.
            int termNo = termInfixArr.Count - 1;
            Term t0 = (Term)termInfixArr[termNo - 1];
            Term t1 = (Term)termInfixArr[termNo];
            eRMA RMA0 = TermType(t0);
            eRMA RMA1 = TermType(t1);
            OperatorDescr od;
            string op;
            int pr;
            OType ot;

            //      Console.WriteLine ("Got symbol {0} ({1},{2}: {0}) -- {3}", (t1 == null) ? "<EOT>" : t1.functor,
            //                  RMA0.ToString (), RMA1.ToString (), (ActionCode [(int)RMA0, (int)RMA1] <= 98) ? "Accepted" : "Rejected");

            switch (ActionCode[(int)RMA0, (int)RMA1])
            {
                case 03: // (Anop Inf)
                    lastInfix = t1;
                    //          Console.WriteLine ("03 Set lastInfix to {0}", t1.functor);
                    break;
                case 01: // (Anop Anop)
                    // SPECIAL CASE IF THE SECOND ANOP IS isPacked and the first is (originally)
                    // an operator. Then: convert most recent term to argument list for
                    // penultimate term, and delete last term.
                    lastInfix = null;
                    if (t0.oDescr != null && t1.isPacked) goto case 06;
                    goto case 99; // error
                case 06: // (Pref Anop)
                    lastInfix = null;
                    // SPECIAL CASE IF THE ANOP IS isPacked
                    // In that case: conversion of most recent term to argument list for
                    // penultimate term, and deletion of last term.
                    if (t1.isPacked)
                    {
                        //            t1.isPacked = false; ////// removed 2007-1-9, because not((X=1,Y=2)) was reported to have 2 args
                        t0.args = t1.ArgumentsToTermArray();
                        t0.arity = (short)t0.args.Length;
                        t0.fType = FType.comp;
                        od = t0.oDescr;
                        if ((t0.arity == 1 &&
                               (od.IsDefinedAsPrefix(out op, out pr, out ot) ||
                                 od.IsDefinedAsPostfix(out op, out pr, out ot)
                               )
                             ) ||
                             (t0.arity == 2 &&
                               od.IsDefinedAsInfix(out op, out pr, out ot)
                             )
                           )
                        {
                            t0.precedence = (short)pr;
                            t0.oType = ot;
                            t0.oDescr = od;
                        }
                        else
                        {
                            t0.oType = OType.noop;
                            t0.precedence = 0;
                        }
                        termInfixArr.RemoveAt(termNo); // remove last element
                        //            Console.WriteLine ("Comma situation, new term is {0}", t0.ToTree ());
                    }
                    break;
                case 08: // (Pref Inf)
                    // try changing Inf into a Pref
                    if (((OperatorDescr)t1.oDescr).IsDefinedAsPrefix(out op, out pr, out ot)
                        && CheckOperatorClash(t0, op, pr, ot, false))
                    {
                        t1.precedence = (short)pr;
                        t1.oType = ot;
                        t1.arity = 1;
                        //Console.WriteLine ("08 Infix {0} changed into a prefix", t1.functor);
                        lastInfix = null;
                    }
                    else  // Changing Inf into a Pref not possible. Change t0 (!) into an Anop
                    {
                        //            Console.WriteLine ("08 Set lastInfix to {0} and turned previous term {1} into an Anop", t1.functor, t0.functor);
                        Term.ToAnop(ref t0);
                        lastInfix = t1;
                    }
                    break;
                case 09: // (Pref Postf)
                case 10: // (Pref EOT)
                    lastInfix = null;
                    Term.ToAnop(ref t0);
                    //          Console.WriteLine ("09 10 Turned {0} into Anop", t0.functor);
                    break;
                case 07: // (Pref Pref)
                case 19: // (Postf Postf)
                    CheckOperatorClash(t0, t1);
                    lastInfix = null;
                    break;
                case 11: // (Inf Anop)
                case 12: // (Inf Pref)
                    lastInfix = null;
                    break;
                case 13: // (Inf Inf)
                    if (((OperatorDescr)t0.oDescr).IsDefinedAsPostfix(out op, out pr, out ot))
                    {
                        t0.precedence = (short)pr;
                        t0.oType = ot;
                        t0.arity = 1;
                        lastInfix = t1;
                        //            Console.WriteLine ("13 Infix {0} changed into a postfix", t0.functor);
                    }
                    else if (((OperatorDescr)t1.oDescr).IsDefinedAsPrefix(out op, out pr, out ot))
                    {
                        t1.precedence = (short)pr;
                        t1.oType = ot;
                        t1.arity = 1;
                        lastInfix = null;
                        //            Console.WriteLine ("13 Infix {0} changed into a prefix", t1.functor);
                    }
                    else
                    {
                        Term.ToAnop(ref t1);
                        lastInfix = null;
                        //            Console.WriteLine ("13 Set lastInfix to {0} and turned it into an Anop", t1.functor);
                    }
                    break;
                case 14: // (Inf Postf)
                    lastInfix = null;
                    Term.ToAnop(ref t1);
                    //          Console.WriteLine ("14 Turned {0} into Anop", t1.functor);
                    break;
                case 18: // (Postf Inf)
                    //          Console.WriteLine ("18 Set lastInfix to {0}", t1.functor);
                    lastInfix = t1;
                    break;
                case 23: // (BOT, Inf)
                    lastInfix = null;
                    // try changing Inf into a Pref. If not possible, change it into an Anop if possible
                    if (((OperatorDescr)t1.oDescr).IsDefinedAsPrefix(out op, out pr, out ot))
                    {
                        t1.precedence = (short)pr;
                        t1.oType = ot;
                        t1.arity = 1;
                        // Console.WriteLine ("23 Infix {0} changed into a prefix", t1.functor);
                    }
                    else if (Utils.HasAtomShape(t1.functor))
                    {
                        Term.ToAnop(ref t1);
                        // Console.WriteLine ("23 Infix {0} has no prefix variant and is turned it into an Anop", t1.functor);
                    }
                    else
                        goto case 99;
                    break;
                case 24: // (BOT, Postf)
                    lastInfix = null;
                    if (Utils.HasAtomShape(t1.functor))
                    {
                        Term.ToAnop(ref t1);
                        // Console.WriteLine ("24 Turned {0} into Anop", t1.functor);
                    }
                    else
                        goto case 99;
                    break;
                case 98:
                    {
                        if (lastInfix == null) goto case 99;

                        //          Console.WriteLine ("98 Infix gave rise to rejection, lastInfix is {0} -- about to try Pre- or Postfix variant", lastInfix.functor);
                        od = (OperatorDescr)lastInfix.oDescr;
                        if (od.IsDefinedAsPrefix(out op, out pr, out ot) || od.IsDefinedAsPostfix(out op, out pr, out ot))
                        {
                            //            Console.WriteLine ("98 Resetting {0} to unary operator succeeded", op);
                            lastInfix.precedence = (short)pr;
                            lastInfix.oType = ot;
                            lastInfix.arity = 1;
                            lastInfix.fType = FType.opr;
                            termInfixArr[termNo - 1] = lastInfix; // restore value
                            lastInfix = null;
                            // now try the unary variant
                            termInfixArr.RemoveAt(termNo); // remove last element
                            //            Console.WriteLine ("98 Last term {0} removed from termInfixArr", (t1 == null) ? "<EOT>" : t1.functor);
                            AnalyzeInput(ref termInfixArr, ref lastInfix);
                            // if we got sofar, it succeeded, and we now must reapply the last term
                            termInfixArr.Add(t1);
                            AnalyzeInput(ref termInfixArr, ref lastInfix);
                            break;
                        }
                        //          Console.WriteLine ("98 Could not convert infix to pre- or postfix");
                        goto case 99;
                    }
                case 99:
                    string s;

                    if (t1 == null)
                        s = "Unexpected end of term";
                    else if (t1.IsVar)
                        s = "Unexpected variable";
                    else
                        s = "Unexpected token: " + t1.Functor;

                    PrologIO.Error(s);
                    break;

                default:
                    lastInfix = null;
                    break;
            }
            //      Console.WriteLine ("Last valid Infix is {0}", lastInfix == null ? "<null>" : lastInfix.functor);
        }


        private static bool CheckOperatorClash(Term t0, Term t1) // Side Notes SN2
        {
            if (t0 == null || t1 == null || t0.isPacked || t1.isPacked) return true;

            string op1 = t1.functor;
            int pr1 = t1.precedence;
            OType ot1 = t1.oType;

            return CheckOperatorClash(t0, op1, pr1, ot1, true);
        }


        private static bool CheckOperatorClash(Term t0, string op1, int pr1, OType ot1, bool genXcp)
        {
            if (t0 == null || t0.isPacked) return true;

            bool ambig = false;
            bool ok = true;
            string op0 = t0.functor;
            int pr0 = t0.precedence;
            OType ot0 = t0.oType;

            //      Console.WriteLine ("CheckOperatorClash {0} {1} {2} <=> {3} {4} {5}", op0, pr0, ot0, op1, pr1, ot1);
            //      Console.WriteLine ("ActionCodeEx {0}", ActionCodeEx [(int)ot0, (int)ot1]);

            switch (ActionCodeEx[(int)ot0, (int)ot1]) // t0 and t1 are in the left-to-right order corresponding to the input expression
            {
                case 16:  // xf  xf
                case 18:  // xf  xfx
                case 19:  // xf  xfy
                case 23:  // yf  xf
                case 25:  // yf  xfx
                case 26:  // yf  xfy
                    ok = (pr0 < pr1);
                    break;
                case 17:  // xf  yf
                case 20:  // xf  yfx
                case 24:  // yf  yf
                case 27:  // yf  yfx
                    ok = (pr0 <= pr1);
                    break;
                case 00:  // fx  fx
                case 01:  // fx  fy
                case 28:  // xfx fx
                case 29:  // xfx fy
                case 42:  // yfx fx
                case 43:  // yfx fy
                    ok = (pr0 > pr1);
                    break;
                case 07:  // fy  fx
                case 08:  // fy  fy
                case 35:  // xfy fx
                case 36:  // xfy fy
                    ok = (pr0 >= pr1);
                    break;
                case 04:  // fx  xfx
                case 05:  // fx  xfy
                case 32:  // xfx xfx
                case 33:  // xfx xfy
                case 34:  // xfx yfx
                case 39:  // xfy xfx
                case 41:  // xfy yfx
                case 46:  // yfx xfx
                case 47:  // yfx xfy
                    ok = (pr0 != pr1);
                    break;
                case 10:  // fy  yf
                case 13:  // fy  yfx
                case 38:  // xfy yf
                    ambig = (pr0 == pr1);
                    ok = (!ambig);
                    break;
                case 03:  // fx  yf
                case 06:  // fx  yfx
                case 09:  // fy  xf
                case 11:  // fy  xfx
                case 12:  // fy  xfy
                case 31:  // xfx yf
                case 37:  // xfy xf
                case 40:  // xfy xfy
                case 45:  // yfx yf
                case 48:  // yfx yfx
                    break;
                case 02:  // fx  xf
                case 30:  // xfx xf
                case 44:  // yfx xf
                    ok = false;
                    break;
                // the following situations cannot occur at all: f:f
                case 14:  // xf  fx
                case 15:  // xf  fy
                case 21:  // yf  fx
                case 22:  // yf  fy
                    PrologIO.Error("CheckOperatorClash -- unexpected operator combination {0} {1} {2} with {3} {4} {5}",
                              op0, pr0, ot0, op1, pr1, ot1);
                    break;
            }
            if (!ok && genXcp)
            {
                if (ambig)
                    PrologIO.Error("Ambiguous operator combination: {0} ({1} {2}) with {3} ({4} {5})", op0, pr0, ot0, op1, pr1, ot1);
                else
                    PrologIO.Error("Operator precedence clash: {0} ({1} {2}) with {3} ({4} {5})", op0, pr0, ot0, op1, pr1, ot1);
                return false;
            }
            else
                return ok;
        }


        public static Term InfixToPrefix(ArrayList termInfixArr)
        {
            termPrefixArray.Clear();
            operatorStack.Clear();
            Term t;
            Term top;

            for (int i = termInfixArr.Count - 2; i > 0; i--)
            {
                t = (Term)termInfixArr[i];
                if (t.fType != FType.opr)
                    termPrefixArray.Add(t);
                else
                {
                    //          Console.WriteLine ("InfixToPrefix Operator {0} type {1}", t.functor, t.oType);
                    // pop all operators with lower precedence and move them to the prefix string
                    while (operatorStack.Count > 0 && (operatorStack.Peek()).precedence < t.precedence)
                        termPrefixArray.Add(operatorStack.Pop());

                    // deal with operators with equal precedence
                    if (operatorStack.Count > 0 && t.precedence == (top = operatorStack.Peek()).precedence)
                    {
                        switch (t.oType)
                        {
                            case OType.fx:
                                if (top.oType == OType.xf)
                                    CheckOperatorClash(t, top);
                                break;
                            case OType.fy:
                                if (top.oType == OType.xf)
                                    termPrefixArray.Add(operatorStack.Pop());
                                else if (top.oType == OType.yf)
                                    CheckOperatorClash(t, top); // ambiguous
                                break;
                            case OType.xfy:
                                // pop and push (results in reversing the operator order
                                if (top.oType == OType.xfy || top.oType == OType.fx || top.oType == OType.fy)
                                    termPrefixArray.Add(operatorStack.Pop());
                                break;
                            case OType.yfx:
                                if (top.oType == OType.fx || top.oType == OType.fy) // pop and push (results in reversing the operator order
                                    termPrefixArray.Add(operatorStack.Pop());
                                break;
                        }
                    }
                    operatorStack.Push(t); // may be erroneous -- will become apparent in PrefixArrayToTermTree
                }
            }
            // finally move all remaining stack operators to the prefix string
            while (operatorStack.Count > 0) termPrefixArray.Add(operatorStack.Pop());

            //      Console.Write ("Prefix (reversed): ");
            //      for (int i = 0; i < termPrefixArray.Count; i++) Console.Write (((Term)termPrefixArray [i]).functor);
            //      Console.WriteLine ();

            int k = termPrefixArray.Count - 1;
            t = PrefixArrayToTermTree(ref k);

            //      Console.WriteLine ("InfixToPrefix tree: {0}", t.ToTree ());
            //      Console.WriteLine("InfixToPrefix term: {0}", t.ToString ());

            return t;
        }


        public static Term PrefixArrayToTermTree(ref int i)
        {
            // The way the prefix expression (in termPrefixArray) is converted to a Term
            // follows directly from the BNF definition for a prefix expression P:
            // P -> <op> P P | <var>.
            // Note: the array is in reversed order, with the root as last element.

            Term t = (Term)termPrefixArray[i--];

            if (t.fType == FType.opr)
            {
                Term t0 = PrefixArrayToTermTree(ref i);
                t.Args = new Term[t.arity];
                t.SetArg(0, t0);
                if (t.arity == 2)
                {
                    if (t0.oType != OType.noop) CheckOperatorClash(t0, t); // * inf
                    Term t1 = PrefixArrayToTermTree(ref i);
                    if (t1.oType != OType.noop) CheckOperatorClash(t, t1); // inf *
                    t.SetArg(1, t1);
                }
                t.fType = FType.comp;
            }
            return t;
        }


        public Term RightmostList // Get the final .(t, null) of a list
        {
            get
            {
                Term t0 = LinkEnd;
                Term t1 = t0;
                int a = t0.arity;

                while (a != 0)
                {
                    t1 = t0;
                    t0 = t0.Arg(a - 1);
                    a = t0.arity;
                }
                return t1;
            }
        }


        public Term AppendList(Term list) // append list t to list 'this'. No check on 'list-ness' is performed!
        {
            Term t0, t1, result;

            if (LinkEnd.Arity == 0) return list; // null-list

            t0 = t1 = result = LinkEnd.CleanUp();

            while (t1.Arity == 2)
            {
                t0 = t1;
                t1 = t1.Arg(1);
            }
            t0.SetArg(1, list);

            return result;
        }


        // DCG stuff

        public TermNode ToDCG(ref Term lhs) // cf. Side Notes SN3
        {
            TermNode body = new TermNode();
            Term result = null;

            Term inVar = new Term();
            Term inVarSave = inVar;
            Term outVar = inVar;
            lhs = new DCGTerm(lhs, ref outVar); // outVar becomes new term
            Term remainder;

            try
            {
                readingDCGClause = true;

                ArrayList alternatives = AlternativesToArrayList();

                for (int i = 0; i < alternatives.Count; i++)
                {
                    Term alt = (Term)alternatives[i];
                    bool embedded = (alternatives.Count > 1);
                    ArrayList terms = alt.ArgumentsToArrayList(true); // flatten clause such as X --> (A, B), C.

                    body.Clear();
                    remainder = inVarSave;

                    for (int ii = 0; ii < terms.Count; ii++)
                        DCGGoal((Term)terms[ii], ref body, ref remainder, ref embedded);

                    // create a term-tree from the array
                    if (i == 0)
                        result = TermSeq(body);
                    else
                        result = new Term(Parser.SEMI, result, TermSeq(body), FType.comp, OType.xfy, 1100);

                    remainder.Bind(outVar);
                }
            }
            finally
            {
                readingDCGClause = false;
            }

            return (result == null) ? null : result.ToGoalList(); // empty body treated similar to null
        }


        private static void DCGGoal(Term t, ref TermNode body, ref Term remainder, ref bool embedded)
        {
            Term temp;

            if (t.IsString || t is Cut)
                body.Append(t);
            else if (t.Functor == Parser.CURL)
            {
                while (t.Arity == 2)
                {
                    body.Append(t.Arg(0));
                    t = t.Arg(1);
                    embedded = true;
                }
            }
            else if (t.IsList)
            {
                temp = new Term();
                if (t == NULLLIST) t = temp; else t.SetRightmostArg(temp);

                if (embedded)
                {
                    body.Append(new Term(Parser.EQ, remainder, t, FType.comp, OType.xfx, 700));
                    embedded = false;
                }
                else
                    remainder.Bind(t); // in this case, nothing is appended to body, which may be left empty (e.g. t-->[x])

                remainder = temp;
            }
            else if (t.IsAtom || t.IsCompound)
            {
                t = new DCGTerm(t, ref remainder);
                body.Append(t);
            }
            else if (t.IsVar)
                PrologIO.Error("Variable not allowed in DCG-clause: {0}", t.VarName());
            else
                PrologIO.Error("Illegal term in DCG-clause: {0}", t);
        }


        public void SetRightmostArg(Term tNew) // Set the final .(t, null) of a list to tNew
        {
            Term t0 = this;
            int a0 = t0.arity;
            Term t1;
            int a1;

            if (a0 == 0) // NULLLIST cannot be handled
                PrologIO.Error("SetRightmostArg may not be called with NULLLIST as argument");
            else
            {
                do
                {
                    t1 = t0;
                    a1 = a0;
                    t0 = t0.Arg(a0 - 1);
                    a0 = t0.arity;
                } while (a0 != 0);

                t1.args[a1 - 1] = tNew;
            }
        }


        /// <summary>Checks whether a variable Occurs in the term </summary>
        // // to be sorted out -- does not work correctly. Currently not used.
        private bool Occurs(int var)
        {
            bool b;

            if (varNo == var)
            {
                Utils.WriteLine("Occurs for {0} returns {1}", var, false);
                return false;
            }
            else
            {
                b = OccursEx(var);
                Utils.WriteLine("Occurs for {0} returns {1}", var, b);
                return b;
            }
        }


        private bool OccursEx(int var)
        {
            if (hasValue)
            {
                if (isUnified)
                    return ULink.OccursEx(var);
                else
                {
                    // unified and not LinkEnd
                    for (int i = 0; i < arity; i++)
                        if (args[i].OccursEx(var))
                        {
                            Utils.WriteLine("Occurs !!!!!!");
                            return true;
                        }
                    return false;
                }
            }
            // unbound
            else
                return (varNo == var);
        }


        // UNIFICATION
        // The stack is used to store the variables and choice points that are bound by the unification.
        // This is needed when backtracking. Unify does not do any unbinding in case of failure. This will
        // be done when backtracking.
        public bool Unify(Term t, VarStack varStack)
        {
            return Unify(t, varStack, false);
        }

        public bool Unify(Term t, VarStack varStack, bool occurCheck)
        {
            if (isUnified) return ULink.Unify(t, varStack, occurCheck);
            if (t.isUnified) return Unify(t.ULink, varStack, occurCheck);
            //Interpreter.UCount++;

            if (hasValue)
            {
                if (t.hasValue)
                {
                    if (_functor.Equals(t._functor) && arity == t.Arity) // => two cuts will unify as well
                    {
                        for (int i = 0; i < arity; i++) if (!args[i].Unify(t.Arg(i), varStack, occurCheck)) return false;

                        return true;
                    }
                    else
                        return false;
                }
                // t is an unbound variable (add occur check here if deemed necessary)
                t.Bind(this);
                varStack.Push(t);
            }
            else // 'this' is an unbound variable. Add occur check here if deemed necessary
            {
                this.Bind(t);
                varStack.Push(this);
            }

            return true;
        }


        public bool Unifiable(Term t, VarStack varStack) // as Unify, but does not actually bind
        {
            int top = varStack.Count;

            bool result = Unify(t, varStack);

            for (int i = varStack.Count - top; i > 0; i--) // unbind all varNoSet that got bound by Unify
            {
                Term s = (Term)varStack.Pop();
                s.Unbind();
            }

            return result;
        }


        // Create an identical new term, with verNo+varNo's that do not occur in any other term created sofar
        // The ULinks are resolved. A new term is essentially constructed by creating a new Term for each
        // term that has no value. This new term gets a version number that is one higher than the original
        // term.
        public Term CleanCopy()
        {
            verNoMax++;

            return CleanCopy(verNoMax);
        }


        public Term CleanCopy(bool newVersion)
        {
            if (newVersion) verNoMax++;

            return CleanCopy(verNoMax);
        }


        public Term CleanCopy(int newVerNo)
        {
            if (hasValue)
            {
                if (isUnified)
                    return LinkEnd.CleanCopy(newVerNo);
                else if (arity == 0) // hasValue && not isUnified
                    return this;
                else
                {
                    Term t = new Term(_functor, arity, fType, oType, precedence);
                    t.oDescr = oDescr;

                    for (int i = 0; i < arity; i++)
                        t.args[i] = args[i].CleanCopy(newVerNo); // recursively refresh arguments

                    return t;
                }
            }
            else if (newVerNo == this.verNo)
                return this.newVar;
            else
            {
                this.verNo = newVerNo; // so that you can detect in the recursion that a copy has been created

                return (this.newVar = new Term(this.varNo)); // keep the old varNo (not essential)
            }
        }


        // Copy a term to put it in the database with freshly renumbered (from 0) variables.
        //  Not strictly necessary, but makes debugging easier. Used by assert(a/z) only.
        public Term CleanUp()
        {
            varDict.Clear();
            int varNo = 0;
            return CleanUp(ref varNo);
        }


        public Term CleanUp(ref int varNo)
        {
            Term t;

            if (this is Cut) return this;

            if (hasValue)
            {
                if (isUnified)
                    return ULink.CleanUp(ref varNo);
                else if (arity == 0)
                    return this;
                else
                {
                    t = new Term(_functor, arity, fType, oType, precedence);

                    for (int i = 0; i < arity; i++)
                        t.args[i] = args[i].CleanUp(ref varNo);
                }
            }
            else // unbound
            {
                t = varDict[this];

                if (t == null)
                {
                    t = new Term(varNo++);
                    varDict[this] = t;
                }
            }
            return t;
        }


        public void NumberVars(ref int k, VarStack s)
        {
            Term t;

            if (hasValue)
            {
                if (isUnified)
                    ULink.NumberVars(ref k, s);
                else if (arity != 0) // hasValue & not isUnified
                    for (int i = 0; i < arity; i++) args[i].NumberVars(ref k, s);
            }
            else // unbound variable
            {
                Term a = new Term(k.ToString(), FType.number);
                k++;
                t = new Term("'$VAR'", a, OType.noop, 0);
                this.Unify(t, s);
            }
        }


        public static Term MakeMatchTerm(Match m, bool asAtom)
        {
            Term[] args = new Term[4];

            if (asAtom)
                args[0] = new Term(Utils.MakeAtomic(m.Value));
            else
                args[0] = new Term(m.Value.ToString(), FType.text);
            args[1] = new Term(m.Index.ToString(), FType.number);
            args[2] = new Term(m.Length.ToString(), FType.number);
            args[3] = new Term("m.Groups");

            return new Term("match", args);
        }


        public bool IsGround()
        {
            if (this is Cut) return true;

            if (hasValue)
            {
                if (isUnified)
                    return ULink.IsGround();
                else
                {
                    for (int i = 0; i < arity; i++)
                        if (!args[i].IsGround()) return false;

                    return true;
                }
            }
            else // unbound
                return false;
        }


        private class StringBuffer
        {
            private const char nullChar = '\x0';
            private StringBuilder sb;

            public StringBuffer()
            {
                sb = new StringBuilder();
            }

            public StringBuffer(string s)
            {
                sb = new StringBuilder(s);
            }

            public void Append(string s)
            {
                if (s == null || s == "") return;

                char startChar = s[0];
                int len = sb.Length;
                char finalChar = (len == 0) ? nullChar : sb[len - 1];

                if ((finalChar != nullChar) &&
                     (finalChar != '(') &&  // no space after a '('
                     (startChar != ')') &&  // ... or before a ')'
                     (startChar != '(') &&  // ... or before a '('
                     (startChar != ']') &&  // ... or before a ']'
                     (startChar != ',') &&  // ... or before a ','
                     (Char.IsLetterOrDigit(finalChar) && Char.IsLetterOrDigit(startChar) ||
                       ".'".IndexOf(finalChar) >= 0 ||            // always a space after these characters
                       "_'".IndexOf(startChar) >= 0 ||            // always a space before these characters
                       (Globals.SpecialAtomChars.IndexOf(finalChar) >= 0 &&
                         Globals.SpecialAtomChars.IndexOf(startChar) >= 0
                       )
                     )
                   )
                    sb.Append(' ');
                sb.Append(s);
            }

            public void AppendTerm(Term t, ListDictionary sqlVarNames)
            {
                this.Append(t.ToStringEx(sqlVarNames));
            }

            public void AppendPackedTerm(Term t, ListDictionary sqlVarNames)
            {
                this.Append("(");
                this.Append(t.ToStringEx(sqlVarNames));
                this.Append(")");
            }


            public override string ToString()
            {
                return sb.ToString();
            }
        }


        public override string ToString()
        {
            return ToStringEx(null);
        }


        public string ToStringSql(ListDictionary sqlVarNames) // special version for where-clause that may accompany a persistent predicate call
        {
            return ToStringEx(sqlVarNames); // ... substitutes column names for varNo's
        }


        private string ToStringEx(ListDictionary sqlVarNames)
        {
            StringBuffer sb;
            int n;

            if (this is Cut)
                return "!";
            else if (!hasValue)
            {
                if (sqlVarNames == null)
                {
                    if (this is NamedVar)
                        return ((NamedVar)this).Name;
                    else
                        return ("_" + varNo);
                }
                else
                {
                    string colName = (string)sqlVarNames[varNo];
                    if (colName == null) PrologIO.Error("Unbound variable not allowed in where-part of persistent predicate call");
                    return (colName);
                }
            }
            else if (isUnified)
                return LinkEnd.ToStringEx(sqlVarNames);
            else if (IsString)
            {
                if (sqlVarNames == null)
                    return "\"" + functor + "\"";
                else // SQL
                    return "'" + functor + "'";
            }
            else if (this == NULLLIST)
                return "[]";
            else if (IsList && arity == 2)
            {
                Term t;
                sb = new StringBuffer("[" + args[0].ToStringEx(sqlVarNames));
                t = args[1];
                while (t.Arity == 2 && t.IsList)
                {
                    sb.Append("," + t.Arg(0).ToStringEx(sqlVarNames));
                    t = t.Arg(1);
                }

                if (t.IsList)
                    sb.Append("]");
                else
                    sb.Append("|" + t.ToStringEx(sqlVarNames) + "]");
                return sb.ToString();
            }
            else if (functor == Parser.CURL)
            {
                if (arity == 0)
                    return "{}";
                else
                {
                    Term t;
                    sb = new StringBuffer("{" + args[0].ToStringEx(sqlVarNames));
                    t = args[1];
                    while (t.Arity == 2)
                    {
                        sb.Append("," + t.Arg(0).ToStringEx(sqlVarNames));
                        t = t.Arg(1);
                    }
                    sb.Append("}");
                }
            }
            else if (functor == NUMVAR && arity == 1 && Arg(0).IsInteger && (n = (int)Arg(0).ExprValue.AsNumber) >= 0)
            {
                int m = n / 26;
                n = n % 26;
                sb = new StringBuffer("ABCDEFGHIJKLMNOPQRSTUVWXYZ".Substring(n, 1) + ((m == 0) ? "" : m.ToString()));
            }
            else if (functor == Parser.COMMA && arity == 2)
                sb = new StringBuffer("(" + args[0].ToStringEx(sqlVarNames) + "," + args[1].ToStringEx(sqlVarNames) + ")");
            else
            {
                sb = new StringBuffer();

                if (arity == 0)
                {
                    if (oDescr == null)
                        sb.Append(functor);
                    else // functors which are actually operators are parenthesized
                        sb.Append("(" + functor + ")");
                }
                else
                {
                    if (oType == OType.xfx || oType == OType.xfy || oType == OType.yfx)
                    {
                        if (precedence < args[0].Precedence ||
                             (precedence == args[0].Precedence && (oType == OType.xfx || oType == OType.yfx)))
                            sb.AppendPackedTerm(args[0], sqlVarNames);
                        else
                            sb.AppendTerm(args[0], sqlVarNames);

                        sb.Append(functor);

                        if (precedence < args[1].Precedence ||
                             (precedence == args[1].Precedence && (oType == OType.xfx || oType == OType.yfx)))
                            sb.AppendPackedTerm(args[1], sqlVarNames);
                        else
                            sb.AppendTerm(args[1], sqlVarNames);
                    }
                    else if (oType == OType.fx && arity <= 1)
                    {
                        sb.Append(functor);
                        if (precedence <= args[0].Precedence)
                            sb.AppendPackedTerm(args[0], sqlVarNames);
                        else
                            sb.AppendTerm(args[0], sqlVarNames);
                    }
                    else if (oType == OType.fy && arity <= 1)
                    {
                        sb.Append(functor);
                        if (precedence < args[0].Precedence)
                            sb.AppendPackedTerm(args[0], sqlVarNames);
                        else
                            sb.AppendTerm(args[0], sqlVarNames);
                    }
                    else if (oType == OType.xf)
                    {
                        if (precedence <= args[0].Precedence)
                            sb.AppendPackedTerm(args[0], sqlVarNames);
                        else
                            sb.AppendTerm(args[0], sqlVarNames);
                        sb.Append(functor);
                    }
                    else if (oType == OType.yf)
                    {
                        if (precedence < args[0].Precedence)
                            sb.AppendPackedTerm(args[0], sqlVarNames);
                        else
                            sb.AppendTerm(args[0], sqlVarNames);
                        sb.Append(functor);
                    }
                    else
                    {
                        sb.Append(functor);
                        sb.Append("(");
                        for (int i = 0; i < (arity - 1); i++)
                            sb.Append(args[i].ToStringEx(sqlVarNames) + ",");
                        sb.Append(args[arity - 1].ToStringEx(sqlVarNames));
                        sb.Append(")");
                    }
                }
            }
            return sb.ToString();
        }


        public string ToStringQ()
        {
            StringBuilder sb = new StringBuilder();

            if (this == null)
                return null;
            else if (this is Cut)
                return "!"; // + varNo;
            else if (!hasValue)
                return ("_" + varNo);
            else if (isUnified)
                return LinkEnd.ToStringQ();
            else if (IsString)
                return "\"" + functor + "\"";
            else if (IsNumber)
                return functor;
            else
            {
                sb.Append("'" + functor + "'");
                if (arity > 0)
                {
                    sb.Append("(");
                    for (int i = 0; i < (arity - 1); i++)
                        sb.Append(args[i].ToStringQ() + ",");
                    sb.Append(args[arity - 1].ToStringQ());
                    sb.Append(")");
                }
                return sb.ToString();
            }
        }


        public string ToRaw()
        {
            StringBuilder sb = new StringBuilder();

            if (this == null)
                return "<null>";
            else if (this is Cut)
                return "!"; // + varNo;
            else if (!hasValue)
                return ("_" + varNo);
            else if (isUnified)
                return "^" + LinkEnd.ToRaw();
            else if (IsString)
                return "\"" + functor + "\"";
            else
            {
                sb.Append("'" + functor + "'");
                if (arity > 0)
                {
                    sb.Append("(");
                    for (int i = 0; i < (arity - 1); i++)
                        sb.Append(args[i].ToRaw() + ",");
                    sb.Append(args[arity - 1].ToRaw());
                    sb.Append(")");
                }
                return sb.ToString();
            }
        }


        public string ToTree()
        {
            return ToTreeEx(0);
        }


        public string ToTreeEx(int indent)
        {
            StringBuilder sb = new StringBuilder(Environment.NewLine + new string(' ', indent));

            if (this == null)
                sb.Append("<null>");
            else if (this is Cut)
                sb.Append("!");
            else if (!hasValue)
                sb.Append("_" + varNo);
            else if (isUnified)
                sb.Append(LinkEnd.ToTreeEx(indent));
            else if (IsString)
                sb.Append("\"" + functor + "\"");
            else
            {
                sb.Append(functor);
                if (oType != OType.noop) sb.Append(String.Format(" ({0} {1})", precedence, oType));
                if (arity > 0)
                    for (int i = 0; i < arity; i++) sb.Append(args[i].ToTreeEx(indent + 2));
            }
            return sb.ToString();
        }


        /*
          -----------------------------------------------
          Types for expression evaluation ('is'-operator)
          -----------------------------------------------
        */


        public abstract class OperandValue : IComparable
        {
            protected object value;
            protected FType fType;
            public object Value { get { return value; } }
            public FType FType { get { return fType; } }
            private static readonly string intro = "Unable or unwilling to convert {0} to ";

            public virtual string AsAtom
            { get { PrologIO.Error(intro + "an atom", value.ToString()); return null; } }
            public virtual string AsString
            { get { PrologIO.Error(intro + "a string", value.ToString()); return null; } }
            public virtual Decimal AsNumber
            { get { PrologIO.Error(intro + "a number", value.ToString()); return 0; } }
            public virtual int AsInteger
            { get { PrologIO.Error(intro + "an integer", value.ToString()); return 0; } }
            public virtual DateTime AsDateTime
            { get { PrologIO.Error(intro + "a date/time", value.ToString()); return DateTime.MinValue; } }
            public virtual TimeSpan AsTimeSpan
            { get { PrologIO.Error(intro + "a timespan", value.ToString()); return TimeSpan.MinValue; } }
            public virtual bool AsBool
            { get { PrologIO.Error(intro + "a bool", value.ToString()); return false; } }
            public virtual object AsObject
            { get { return value; } }

            public abstract int CompareTo(object o);
        }


        public class AtomValue : OperandValue
        {
            public AtomValue(string a)
            {
                value = a;
                fType = FType.atom;
            }

            public override string AsAtom
            { get { return (value == null) ? null : Utils.Dequote((string)value, '\''); } }
            public override string AsString
            { get { return (value == null) ? null : Utils.Dequote((string)value, '\'').Replace("\"", "\"\""); } }

            public override int CompareTo(object o)
            {
                return this.AsString.CompareTo(((AtomValue)o).AsString);
            }
        }


        public class StringValue : OperandValue
        {
            public StringValue(string s)
            {
                value = s;
                fType = FType.text;
            }

            public override string AsAtom { get { return value as string; } }
            public override string AsString { get { return value as string; } }

            public override int CompareTo(object o)
            {
                return this.AsString.CompareTo(((StringValue)o).AsString);
            }
        }


        public class NumberValue : OperandValue
        {
            public NumberValue(Decimal d)
            {
                value = d;
                fType = FType.number;
            }

            public override Decimal AsNumber { get { return (decimal)value; } }
            public override int AsInteger
            {
                get
                {
                    try { return (Convert.ToInt32(value)); }
                    catch { PrologIO.Error("{0} is not a valid integer", value); return 0; }
                }
            }

            public override int CompareTo(object o)
            {
                return this.AsNumber.CompareTo(((NumberValue)o).AsNumber);
            }
        }


        public class ObjectValue : OperandValue
        {
            public override DateTime AsDateTime { get { return (DateTime)value; } }
            public override TimeSpan AsTimeSpan { get { return (TimeSpan)value; } }
            public override Decimal AsNumber { get { return (decimal)value; } }
            public override Int32 AsInteger { get { return (int)value; } }
            public override object AsObject { get { return value; } }
            public override bool AsBool { get { return (bool)value; } }
            public ObjectValue(Object d)
            {
                value = d;
                fType = FType.vmobj;
            }

            public override int CompareTo(object o)
            {
                int v = value.GetType().Name.CompareTo(o.GetType().Name);
                if (v == 0)
                {
                    return value.GetHashCode().CompareTo(o.GetHashCode());
                }
                return v;
            }
        }

        public class DateTimeValue : OperandValue
        {
            public DateTimeValue(DateTime dt)
            {
                value = dt;
                fType = FType.datetime;
            }

            public override DateTime AsDateTime { get { return (DateTime)value; } }

            public override int CompareTo(object o)
            {
                return this.AsDateTime.CompareTo(((DateTimeValue)o).AsDateTime);
            }
        }


        public class TimeSpanValue : OperandValue
        {
            public TimeSpanValue(TimeSpan ts)
            {
                value = ts;
                fType = FType.timespan;
            }

            public override TimeSpan AsTimeSpan { get { return (TimeSpan)value; } }

            public override int CompareTo(object o)
            {
                return this.AsTimeSpan.CompareTo(((TimeSpanValue)o).AsTimeSpan);
            }
        }


        public class BoolValue : OperandValue
        {
            public BoolValue(bool b)
            {
                value = b;
                fType = FType.boolean;
            }

            public override bool AsBool { get { return (bool)value; } }

            public override int CompareTo(object o)
            {
                return this.AsBool.CompareTo(((BoolValue)o).AsBool);
            }
        }

        /*
           1. Variables < Numbers < Atoms < Strings < Compound Terms
           2. Variables are sorted by address.
           3. Atoms are compared alphabetically.
           4. Strings are compared alphabetically.
           5. Numbers are compared by value. Integers and floats are treated identically.
           6. Compound terms are first checked on their functor-name (alphabetically), then on their arity
              and finally recursively on their arguments, leftmost argument first.
        */

        public int CompareTo(object o)
        {
            Term t0 = this.LinkEnd;
            Term t1 = ((Term)o).LinkEnd;
            int result = t0.fType.CompareTo(t1.fType);

            if (result != 0) return result;

            // equal ftypes
            switch (t0.fType)
            {
                case FType.var:
                    return t0.varNo.CompareTo(t1.varNo);
                case FType.number:
                    return t0.ExprValue.AsNumber.CompareTo(t1.ExprValue.AsNumber);
                case FType.atom:
                case FType.text:
                    return t0.functor.CompareTo(t1.functor);
                case FType.datetime:
                    return t0.ExprValue.AsDateTime.CompareTo(t1.ExprValue.AsDateTime);
                case FType.timespan:
                    return t0.ExprValue.AsTimeSpan.CompareTo(t1.ExprValue.AsDateTime);
                case FType.vmobj:
                    return t0.ExprValue.CompareTo(t1.ExprValue.AsObject);
                case FType.comp:
                case FType.dcg:
                    result = t0.functor.CompareTo(t1.functor); // compare functor names

                    if (result != 0) return result; // unequal

                    result = t0.arity.CompareTo(t1.arity); // same functor: lowest arity first

                    if (result != 0 || t0.arity == 0) return result;

                    for (int i = 0; i < t0.args.Length; i++)
                    {
                        result = t0.Arg(i).CompareTo(t1.Arg(i));

                        if (result != 0) return result;
                    }
                    return 0;
                default:
                    PrologIO.Error("Term has illegal FType enum value: {0}", t0.fType.ToString());
                    return 0;
            }
        }


        private decimal Trunc(decimal d)
        {
            //return Math.Sign (d) * Math.Floor (Math.Abs (Convert.ToDouble (d)));
            return Math.Sign(d) * Convert.ToDecimal(Math.Floor(Convert.ToDouble(Math.Abs(d))));
        }


        public OperandValue ExprValue
        {
            get
            {
                OperandValue v0, v1, v2, v3;

                if (!hasValue)
                    PrologIO.Error("Unbound term cannot be evaluated to a number");
                else if (isUnified)
                    return ULink.ExprValue;
                else if (KbKey == "rnd/1")
                    return new NumberValue(rnd.Next(Convert.ToInt32(args[0].ExprValue)));
                else if (FType == FType.number)
                    return new NumberValue(Convert.ToDecimal(_functor));
                else if (FType == FType.text)
                    return new StringValue((string)_functor);
                else if (FType == FType.datetime)
                    return new DateTimeValue((DateTime)_functor);
                else if (FType == FType.vmobj)
                    return new ObjectValue((Object)_functor);
                else if (FType == FType.boolean)
                    return new BoolValue((bool)_functor);
                else if (Arity == 0)
                    switch (functor)
                    {
                        case "pi":
                            return new NumberValue(Convert.ToDecimal(Math.PI));
                        case "e":
                            return new NumberValue(Convert.ToDecimal(Math.E));
                        case "now":
                            return new DateTimeValue(DateTime.Now);
                        case "today":
                            return new DateTimeValue(DateTime.Now.Date);
                        case "yesterday":
                            return new DateTimeValue(DateTime.Now.AddDays(-1).Date);
                        case "tomorrow":
                            return new DateTimeValue(DateTime.Now.AddDays(1).Date);
                        default:
                            return new AtomValue((string)_functor);
                    }
                else if (arity == 1)
                {
                    v0 = args[0].ExprValue;

                    switch (functor)
                    {
                        case "+":
                            return new NumberValue(v0.AsNumber);
                        case "-":
                            return new NumberValue(-v0.AsNumber);
                        case @"\":
                            return new NumberValue(~(int)v0.AsNumber);
                        case "abs":
                            return new NumberValue(Math.Abs(v0.AsNumber));
                        case "exp":
                            return new NumberValue(Convert.ToDecimal(Math.Exp(Convert.ToDouble(v0.AsNumber))));
                        case "sin":
                            return new NumberValue(Convert.ToDecimal(Math.Sin(Convert.ToDouble(v0.AsNumber))));
                        case "cos":
                            return new NumberValue(Convert.ToDecimal(Math.Cos(Convert.ToDouble(v0.AsNumber))));
                        case "tan":
                            return new NumberValue(Convert.ToDecimal(Math.Tan(Convert.ToDouble(v0.AsNumber))));
                        case "sinh":
                            return new NumberValue(Convert.ToDecimal(Math.Sinh(Convert.ToDouble(v0.AsNumber))));
                        case "cosh":
                            return new NumberValue(Convert.ToDecimal(Math.Cosh(Convert.ToDouble(v0.AsNumber))));
                        case "tanh":
                            return new NumberValue(Convert.ToDecimal(Math.Tanh(Convert.ToDouble(v0.AsNumber))));
                        case "asin":
                            return new NumberValue(Convert.ToDecimal(Math.Asin(Convert.ToDouble(v0.AsNumber))));
                        case "acos":
                            return new NumberValue(Convert.ToDecimal(Math.Acos(Convert.ToDouble(v0.AsNumber))));
                        case "atan":
                            return new NumberValue(Convert.ToDecimal(Math.Atan(Convert.ToDouble(v0.AsNumber))));
                        case "log":
                            return new NumberValue(Convert.ToDecimal(Math.Log(Convert.ToDouble(v0.AsNumber))));
                        case "log10":
                            return new NumberValue(Convert.ToDecimal(Math.Log10(Convert.ToDouble(v0.AsNumber))));
                        case "round":
                            return new NumberValue(Math.Round(v0.AsNumber));
                        case "floor":
                            return new NumberValue(Convert.ToDecimal(Math.Floor(Convert.ToDouble(v0.AsNumber))));
                        case "trunc":
                            return new NumberValue(Trunc(v0.AsNumber));
                        case "ceil":
                            return new NumberValue(Convert.ToDecimal(Math.Ceiling(Convert.ToDouble(v0.AsNumber))));
                        case "sign":
                            return new NumberValue(Convert.ToDecimal(Math.Sign(v0.AsNumber)));
                        case "sqrt":
                            return new NumberValue(Convert.ToDecimal(Math.Sqrt(Convert.ToDouble(v0.AsNumber))));
                        case "sqr":
                            {
                                decimal d = v0.AsNumber;
                                return new NumberValue(d * d);
                            }
                        // string handling
                        case "length":
                            return new NumberValue((v0.AsString).Length);
                        case "upcase":
                            return new StringValue((v0.AsString).ToUpper());
                        case "upcase1": // upcase first char; rest unchanged
                            {
                                string s = v0.AsString;
                                return new StringValue(Char.ToUpper(s[0]) + s.Substring(1));
                            }
                        case "lowcase":
                            return new StringValue((v0.AsString).ToLower());
                        case "trim":
                            return new StringValue((v0.AsString).Trim());
                        case "trimstart":
                            return new StringValue((v0.AsString).TrimStart());
                        case "trimend":
                            return new StringValue((v0.AsString).TrimEnd());
                        // DateTime stuff
                        case "year":
                            return new NumberValue((v0.AsDateTime).Year);
                        case "month":
                            return new NumberValue((v0.AsDateTime).Month);
                        case "day":
                            return new NumberValue((v0.AsDateTime).Day);
                        case "hour":
                            return new NumberValue((v0.AsDateTime).Hour);
                        case "minute":
                            return new NumberValue((v0.AsDateTime).Minute);
                        case "second":
                            return new NumberValue((v0.AsDateTime).Second);
                        case "millisecond":
                            return new NumberValue((v0.AsDateTime).Millisecond);
                        case "date":
                            return new DateTimeValue((v0.AsDateTime).Date);
                        case "dayofweek":
                            return new NumberValue((int)((v0.AsDateTime).DayOfWeek));
                        case "dayofyear":
                            return new NumberValue((v0.AsDateTime).DayOfYear);
                        case "ticks":
                            return new NumberValue((v0.AsDateTime).Ticks);
                        case "today":
                            return new DateTimeValue(DateTime.Today);
                        case "timeofday":
                            return new TimeSpanValue(DateTime.Now.TimeOfDay);
                        default:
                            PrologIO.Error("Not a built-in function: {0}/1", functor);
                            break;
                    }
                }
                else if (arity == 2)
                {
                    v0 = args[0].ExprValue;
                    v1 = args[1].ExprValue;

                    switch (functor)
                    {
                        case "+":
                            {
                                if (v0.FType == FType.text)
                                    return new StringValue(v0.AsString + v1.AsString);
                                else if (v0.FType == FType.atom)
                                    return new AtomValue(Utils.MakeAtom(v0.AsAtom + v1.AsAtom));
                                else if (v0.FType == FType.datetime && v1.FType == FType.timespan)
                                    return new DateTimeValue((v0.AsDateTime).Add(v1.AsTimeSpan));
                                else
                                    return new NumberValue(v0.AsNumber + v1.AsNumber);
                            }
                        case "-":
                            if (v0.FType == FType.datetime)
                            {
                                if (v1.FType == FType.timespan)
                                    return new DateTimeValue((v0.AsDateTime).Subtract(v1.AsTimeSpan));
                                else // timespan assumed
                                    return new TimeSpanValue((v0.AsDateTime).Subtract(v1.AsDateTime));
                            }
                            else
                                return new NumberValue(v0.AsNumber - v1.AsNumber);
                        case "<<":
                            return new NumberValue((long)v0.AsNumber << (int)v1.AsNumber);
                        case ">>":
                            return new NumberValue((long)v0.AsNumber >> (int)v1.AsNumber);
                        case "=":
                            return new BoolValue(v0.CompareTo(v1) == 0);
                        case "<>":
                            return new BoolValue(v0.CompareTo(v1) != 0);
                        case "<":
                            return new BoolValue(v0.CompareTo(v1) < 0);
                        case "<=":
                            return new BoolValue(v0.CompareTo(v1) <= 0);
                        case ">":
                            return new BoolValue(v0.CompareTo(v1) > 0);
                        case ">=":
                            return new BoolValue(v0.CompareTo(v1) >= 0);
                        case "*":
                            return new NumberValue(v0.AsNumber * v1.AsNumber);
                        case "/":
                            return new NumberValue(v0.AsNumber / v1.AsNumber);
                        case "//":
                            return new NumberValue(Trunc(v0.AsNumber / v1.AsNumber));
                        case "#":
                            return new NumberValue((long)v0.AsNumber ^ (long)v1.AsNumber);
                        case @"/\":
                            return new NumberValue((long)v0.AsNumber & (long)v1.AsNumber);
                        case @"\/":
                            return new NumberValue((long)v0.AsNumber | (long)v1.AsNumber);
                        case "^":
                            return new NumberValue(Convert.ToDecimal(Math.Pow(Convert.ToDouble(v0.AsNumber),
                                                                               Convert.ToDouble(v1.AsNumber))));
                        case "mod":
                            return new NumberValue(v0.AsNumber % v1.AsNumber);
                        case "round":
                            return new NumberValue(Math.Round(v0.AsNumber, Convert.ToInt32(v1.AsNumber)));
                        case "atan2":
                            return new NumberValue(Convert.ToDecimal(Math.Atan2(Convert.ToDouble(v0.AsNumber),
                                                                                  Convert.ToDouble(v1.AsNumber))));
                        case "max":
                            return new NumberValue(Math.Max(v0.AsNumber, v1.AsNumber));
                        case "min":
                            return new NumberValue(Math.Min(v0.AsNumber, v1.AsNumber));
                        case "format":
                            if (v0.FType == FType.datetime)
                                return new StringValue((v0.AsDateTime).ToString(v1.AsString));
                            else if (v1.FType == FType.number)
                                return new StringValue(string.Format(v0.AsString, v1.AsNumber));
                            else
                            {
                                PrologIO.Error("Unable to format '{0}'", v1.Value);
                                break;
                            }
                        // string handling
                        case "indexof":
                            return new NumberValue((v0.AsString).IndexOf(v1.AsString));
                        case "padleft":
                            return new StringValue((v0.AsString).PadLeft(v1.AsInteger));
                        case "padright":
                            return new StringValue((v0.AsString).PadRight(v1.AsInteger));
                        case "remove":
                            {
                                int len = v1.AsInteger;
                                return new StringValue((v0.AsString).Remove(len, (v1.AsString).Length - len));
                            }
                        case "substring":
                            {
                                int len = v1.AsInteger;
                                return new StringValue((v0.AsString).Substring(len, (v0.AsString).Length - len));
                            }
                        case "wrap":
                            return new StringValue(Utils.ForceSpaces(v0.AsString, v1.AsInteger));
                        // date/time
                        case "addyears":
                            return new DateTimeValue((v0.AsDateTime).AddYears(v1.AsInteger));
                        case "addmonths":
                            return new DateTimeValue((v0.AsDateTime).AddMonths(v1.AsInteger));
                        case "adddays":
                            return new DateTimeValue((v0.AsDateTime).AddDays(v1.AsInteger));
                        case "addhours":
                            return new DateTimeValue((v0.AsDateTime).AddHours(v1.AsInteger));
                        case "addminutes":
                            return new DateTimeValue((v0.AsDateTime).AddMinutes(v1.AsInteger));
                        case "addseconds":
                            return new DateTimeValue((v0.AsDateTime).AddSeconds(v1.AsInteger));
                        default:
                            PrologIO.Error("Not a built-in function: {0}/2", functor);
                            break;
                    }
                }
                else if (arity == 3)
                {
                    v0 = args[0].ExprValue;
                    v1 = args[1].ExprValue;
                    v2 = args[2].ExprValue;

                    switch (functor)
                    {
                        case "indexof":
                            return new NumberValue((v0.AsString).IndexOf(v1.AsString, v2.AsInteger));
                        case "remove":
                            return new StringValue((v0.AsString).Remove(v1.AsInteger, v2.AsInteger));
                        case "substring":
                            return new StringValue((v0.AsString).Substring(v1.AsInteger, v2.AsInteger));
                        case "replace":
                            return new StringValue((v0.AsString).Replace(v1.AsString, v2.AsString));
                        case "time":
                        case "timespan":
                            return new TimeSpanValue(new TimeSpan(v0.AsInteger, v1.AsInteger, v2.AsInteger));
                        case "date":
                        case "datetime":
                            return new DateTimeValue(new DateTime(v0.AsInteger, v1.AsInteger, v2.AsInteger));
                        case "if":
                            return new StringValue(v0.AsBool ? v1.AsString : v2.AsString);
                        default:
                            PrologIO.Error("Not a built-in function: {0}/3", functor);
                            break;
                    }
                }
                else if (arity == 4)
                {
                    v0 = args[0].ExprValue;
                    v1 = args[1].ExprValue;
                    v2 = args[2].ExprValue;
                    v3 = args[3].ExprValue;

                    if (functor == "timespan")
                        return new TimeSpanValue(new TimeSpan(v0.AsInteger, v1.AsInteger, v2.AsInteger, v3.AsInteger));
                    else
                        PrologIO.Error("Not a built-in function: {0}/4", functor);
                }
                else
                    PrologIO.Error("Not a built-in function: {0}/{1}", functor, arity);

                return null;
            }
        }
    }


    /*
      --------
      NamedVar
      --------
    */

    public class NamedVar : Term // Program mode: carries a variable's symbolic name as found in the source
    {
        private string name;
        public string Name { get { return name; } }

        public NamedVar(string variableName)
            : base()
        {
            name = variableName;
        }
    }


    /*
      ----------
      ObjectTerm
      ----------
    */

    public class ObjectTerm : Term // dummy term, to be able to carry an arbitrary object through consecutive resolution steps
    {
        public ObjectTerm(Object o)
            : base(null, OType.fx, 0)
        {
            uLink = o;
        }

        public new Object Value
        {
            get { return (Object)uLink; }
        }
    }


    /*
      ---
      Cut
      ---
    */

    public class Cut : Term
    {
        private const string CUT = "!";

        public Cut(int stackSize)
            : base(null, OType.fx, 0)
        {
            _functor = CUT;
            hasValue = true;
            isUnified = false;
            varNo = stackSize;
        }
    }


    /*
      ----
      List
      ----
    */

    // Experimental, not used yet. Start with using the notation lyst = (. <args .) in the parser

    public class List : Term
    {
        public List(params Term[] args)
            : base(Parser.DOT, args)
        {
        }

        new static List NULLLIST = new List();

        public Term Head { get { return arity == 0 ? null : args[0]; } }

        //    public List Tail { get { return arity == 0 ? null : new List (args [1..N]); } }

    }


    /*
      --------
      ListTerm
      --------
    */

    public class ListTerm : Term
    {
        public ListTerm(Term t) : base(Parser.DOT, t, NULLLIST, FType.comp, OType.xfy, 100) { }

        public ListTerm(string s) : base(Parser.DOT, new Term(s), NULLLIST, FType.comp, OType.xfy, 100) { }

        public ListTerm(Term t0, Term t1) : base(Parser.DOT, t0, t1, FType.comp, OType.xfy, 100) { }
    }


    /*
      -------
      DCGTerm
      -------
    */

    public class DCGTerm : Term
    {
        public DCGTerm(Term t, ref Term z)
            : base(t.Functor, new Term[t.Arity + 2])
        {
            for (int i = 0; i < t.Arity; i++) Args[i] = t.Args[i];
            Args[Arity - 2] = z;
            Args[Arity - 1] = z = new Term();
        }

        public DCGTerm() : base(Parser.CURL, FType.dcg) { }

        public DCGTerm(Term t) : base(Parser.CURL, t, new DCGTerm(), FType.dcg, OType.xfy, 99) { }

        public DCGTerm(Term t0, Term t1) : base(Parser.CURL, t0, t1, FType.dcg, OType.xfy, 99) { }
    }
}
