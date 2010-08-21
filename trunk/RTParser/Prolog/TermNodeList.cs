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

namespace RTParser.Prolog
{
    /*
      --------
      TermNode
      --------

      A TermNode serves two purposes: to store the goals of a query (the TermNode proper)
      and to store predicates (clauses)

      A goal list is constructed as a simple chained list of TermNodes. This makes it easy
      (and also more efficient in terms of GC) to revert to a previous state upon backtracking
      (in contrast to e.g. an ArrayList).

      A predicate consists of one or more clauses. A clause consist of a head and optionally a
      body. A head is a term, the body is a sequence of terms. A predicate is stored as a chain
      of TermNodes, where each TermNode represents a clause. These TermNodes are linked via the
      nextClause field. In each clause/TermNode the clause head is stored in term, and the
      clause body (which may be null) in nextNode.

    */


    public class TermNode
    {
        #region private fields
        private BI builtinId = BI.none;
        #endregion

        #region protected properties
        protected Term term;              // for a ClauseNode: predicate head
        protected TermNode nextNode = null;   // next node in the chain
        protected ClauseNode nextClause = null; // next predicate clause (for a TermNode: advanced upon backtracking)
        protected PredicateDescr predDescr;
        protected int level = 0;         // debugging (for indentation)
        #endregion

        #region public properties
        public int Level { get { return level; } set { level = value; } }
        public Term Term { get { return term; } }
        public TermNode NextNode { get { return nextNode; } set { nextNode = value; } }
        public virtual ClauseNode NextClause { get { return nextClause; } set { nextClause = value; } }
#if persistent
    public    bool            IsPersistent { get { return PredDescr == null ? false : PredDescr is PersistentPredDescr; } }
#endif
        public bool Spied { get { return PredDescr == null ? false : PredDescr.Spied; } }
        public SpyPort SpyPort { get { return PredDescr == null ? SpyPort.none : PredDescr.SpyPort; } }
        public BI BuiltinId { get { return builtinId; } set { builtinId = value; } }
        public PredicateDescr PredDescr { get { return predDescr; } set { predDescr = value; } }
        #endregion

        public TermNode()
        {
        }


        public TermNode(Term t)
        {
            term = t;
        }


        public TermNode(Term t, int l)
        {
            term = t;
            level = l;
        }


        public TermNode(string tag) // builtin predicates
        {
            try
            {
                builtinId = (BI)Enum.Parse(typeof(BI), tag, false);
            }
            catch
            {
                PrologIO.Error("Unknown builtin-tag {0}", tag);
            }
        }


        public TermNode(Term t, TermNode n)
        {
            term = t;
            nextNode = n;
        }


        public bool FindPredicateDefinition(PredicateStorage predicateStorage)
        {
            return FindPredicateDefinition(predicateStorage, null);
        }


        // put the predicate definition (if found) into the TermNode if it is not already there
        public bool FindPredicateDefinition(PredicateStorage predicateStorage, Term whereTerm) // whereTerm != null : for persistent predicates only
        {
            if (predDescr == null) predDescr = predicateStorage[term.KbKey];

            if (predDescr == null) return false;

#if arg1index
            Term arg;

            if (predDescr.IsFirstArgIndexed)
            {
                if ((arg = term.Arg(0)).IsVar)
                    nextClause = predDescr.FirstArgVarClause();
                else // not a variable
                {
                    nextClause = predDescr.FirstArgNonvarClause(arg.Functor);
                    // check whether there is an indexed var clause
                    if (nextClause == null) nextClause = predDescr.FirstArgVarClause();
                    // if the above failed, the entire predicate fails (no unification possible)
                    if (nextClause == null) nextClause = ClauseNode.FAIL; // "fail."
                }

                if (nextClause == null)
                    nextClause = predDescr.GetClauseList(term, whereTerm); // GetClauseList: in PredicateStorage
            }
            else // not indexed
#endif
                nextClause = predDescr.GetClauseList(term, whereTerm); // GetClauseList: in PredicateStorage

            return true;
        }


        public void Append(Term t)
        {
            if (term == null) { term = t; return; } // empty list

            TermNode tail = this;
            TermNode next = nextNode;

            while (next != null)
            {
                tail = next;
                next = next.nextNode;
            }
            tail.nextNode = new TermNode(t);
        }


        public virtual TermNode Append(TermNode t)
        {
            TermNode tail = this;
            TermNode next = nextNode;

            while (next != null) // get the last TermNode
            {
                tail = next;
                next = next.nextNode;
            }

            tail.nextNode = t;

            return this;
        }


        public void Clear()
        {
            term = null;
            nextNode = null;
            nextClause = null;
            level = 0;
        }


        public Term ToList()
        {
            if (term == null) // last Term of TermNode
                return Term.NULLLIST; // [];
            else if (nextNode == null)
                return new Term(Parser.DOT, term, Term.NULLLIST, FType.comp, OType.xfy, 100); // [];
            else
                return new Term(Parser.DOT, term, nextNode.ToList(), FType.comp, OType.xfy, 100);
        }


        public override string ToString()
        {
            return ToString(2);
        }


        public virtual string ToString(int indent)
        {
            return (term == null) ? null : new String(' ', 2 * indent) + term.ToString() +
                                           (nextNode == null ? null : ",\n" + nextNode.ToString(indent));
        }
    }


    /*
      ----------
      ClauseNode
      ----------
    */

    public class ClauseNode : TermNode
    {

        public ClauseNode(Term t, TermNode body)
            : base(t, body)
        {
        }


        public ClauseNode CleanUp() // renumbers all terms
        {
            return this;
        }


        public override string ToString() // list the terms (connected by NextNode) of a single clause
        {
            StringBuilder sb = new StringBuilder("\n" + term.ToString());

            bool first = true;
            TermNode tl = nextNode;

            if (tl == null) return sb.ToString() + ".\n";

            while (true)
            {
                if (first) sb.Append(" :-");

                sb.Append("\n  " + tl.Term);

                if ((tl = tl.NextNode) == null)
                    return sb.ToString() + ".\n";
                else if (!first)
                    sb.AppendFormat(",");

                first = false;
            }
        }


        public static ClauseNode FAIL = new ClauseNode(Term.FAIL, null);
    }


#if persistent
  public class PersistentClauseNode : ClauseNode
  {
    private DataRowCollection dataRowCollection;
    private int rowNo;
    private int rowNoMax;

    public PersistentClauseNode (DataRowCollection drc, int clauseNo, PredicateDescr pd) : base (null, null)
    {
      dataRowCollection = drc;
      rowNo      = clauseNo;
      rowNoMax   = drc.Count-1;
      nextClause = null;
      term   = DataRowAsTerm (rowNo);
      predDescr  = pd;
    }


    private Term DataRowAsTerm (int rowNo)
    {
      DataRow row = dataRowCollection [rowNo];

      // caching does not work !!!!!!!!!!!!!!!!! (Conversion to Term fails for unknown reason)
      //if (row [0] != DBNull.Value) return (Term)row [0.Value]; // return cached value, else construct value into row[0]

      PersistentPredDescr ppd = (PersistentPredDescr)predDescr;
      DbMetaDataCollection mdc = ppd.MetaDataCollection;

      // construct the term, by inspecting each argument of the input term.
      // If it is a Var: take the corresponding value from the result set row, otherwise: simply copy the argument
      int colNo = 0;
      int nCols = ppd.Query.Arity;
      Term[] args = new Term [nCols];

      for (int i = 0; i < nCols; i++)
      {
        Term a = ppd.Query.Args [i];

        if (a.IsVar)
          args [i] = DbAccess.DbScalarToTerm (row[1+colNo++], mdc [i].DbType, a.FType, ppd.DbEntity);
        else
          args [i] = a;
      }

      Term rowTerm = new Term (ppd.Name, args);
      row [0] = rowTerm; // cache /// BUT DOES NOT WORK

      return rowTerm;
    }

    public override ClauseNode NextClause
    {
      get
      {
        if (nextClause == null)
          return (rowNo == rowNoMax) ? null : (nextClause = new PersistentClauseNode (dataRowCollection, rowNo+1, predDescr));
        else
          return nextClause; // cached
      }
    }
  }
#endif


    /*
    --------
    SpyPoint
    --------
  */

    //  pushed on the VarStack to detect failure, inserted in the goalNode to detect exit.

    class SpyPoint : TermNode // TermNode only used as type-compatible vehicle for port and saveGoal
    {
        private SpyPort port;
        public SpyPort Port { get { return port; } }
        private TermNode saveGoal;
        public TermNode SaveGoal { get { return saveGoal; } }

        public SpyPoint(SpyPort p, TermNode g)
            : base()
        {
            port = p;
            saveGoal = g;
            level = g.Level;
        }

        public void Kill()
        {
            port = SpyPort.none;
        }

        public override string ToString()
        {
            return "[" + port + "-spypoint] " + saveGoal.Term.ToString() + " ...";
        }
    }

}
