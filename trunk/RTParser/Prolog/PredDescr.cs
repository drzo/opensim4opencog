#define debugging
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
using System.Collections;
using System.Collections.Specialized;
using System.Data;
using IO = RTParser.Prolog.PrologIO;


namespace RTParser.Prolog
{
    [Flags]
    public enum SpyPort : short { none = 0, call = 1, exit = 2, fail = 4, redo = 8, full = 15 }

    public class PredicateDescr
    {
        private string module; // TO BE IMPLEMENTED
        private string definingFile;
        protected string name;
        public string Name { get { return name; } }
        protected int arity;
        public int Arity { get { return arity; } }
#if arg1index
        private const short ARG0COUNT_MIN = 8;
        private Hashtable arg0Index = null; // ... for first argument indexing
#endif
        private ClauseNode clauseList;
        public ClauseNode ClauseNode { get { return clauseList; } set { clauseList = value; } }
        private ClauseNode clauseListEnd;
        private bool isDiscontiguous = false; // pertains to a single definingFile only. A predicate must always be in a single definingFile
        public bool IsDiscontiguous { get { return isDiscontiguous; } set { isDiscontiguous = value; } }
#if debugging
        private bool spied;
        public bool Spied { get { return spied; } }
        private SpyPort spyMode;
        public SpyPort SpyPort { get { return spyMode; } }
#endif
        public string Module { get { return module; } set { module = value; } }
        public string DefiningFile { get { return definingFile; } }

        public PredicateDescr(string m, string f, string n, int a, ClauseNode c)
        {
            module = m;
            definingFile = (f == null) ? "predefined or asserted predicate" : f;
            name = n;
            arity = a;
#if debugging
            spyMode = SpyPort.none;
#endif
            clauseList = clauseListEnd = c;
            // disqualify this predicate for first-argument indexing if arity=0
        }


        public void SetClauseListHead(ClauseNode c)
        {
            clauseList = clauseListEnd = c;

            while (clauseListEnd.NextClause != null) clauseListEnd = clauseListEnd.NextClause;

#if arg1index
            DestroyFirstArgIndex();
#endif
        }


        public void AdjustClauseListEnd() // forward clauseListEnd to the last clause
        {
            if ((clauseListEnd = clauseList) != null)
                while (clauseListEnd.NextClause != null) clauseListEnd = clauseListEnd.NextClause;
        }


        public void AppendToClauseList(ClauseNode c) // ClauseNode and ClauseListEnd are != null
        {
            clauseListEnd.NextClause = c;

            do
                clauseListEnd = clauseListEnd.NextClause;
            while
              (clauseListEnd.NextClause != null);

#if arg1index
            DestroyFirstArgIndex();
#endif
        }


        /* First-argument indexing

           A hashtable is maintained of clauses that have a non-var first argument. Each
           predicate has its own hashtable. The functors of these first arguments are
           distinct (that is, in the hashtable). For predicate clauses that have identical
           first argument functors, only the first of these clauses is included in the
           hashtable. The clauses are traversed in the order in which they were read in.

           If a clause with a *variable* first argument is encountered, it is included in
           the hashtable as well, but no more entries wil be included after that.
           It will serve as a catch-all for instantiated arguments that do not match with
           any of the other hashtable entries.

           First-argument indexing is applied when a goal is about to be resolved. If the
           first argument of the goal is a non-var, it will be looked up in the hashtable.
           If it is found, the first clause of the defining predicate (cf. Execute()) is
           set to the hashtable entry. If it is not found, and the hashtable has a var-
           entry, then the first clause is set to that entry. The (non-var) goal fails if
           it is not found in the hashtable.

           First-argument indexing is not used upon backtracking. Choicepoints are simply
           the next clauses after the current one. This might be optimized in the future.

           Notice that this implementation of first-argument indexing is of limited use if
           there are many similar first arguments that are scattered throughout the
           predicate clauses, especially when backtracking takes place.
           A more sopisticated scheme could be envisaged where all similar fa-clauses are
           chained together. Now, for optimal use the user has to take care that all similar
           fa-clauses are grouped together (which seems advisable anyway).

           Persistent predicates are not subject to first-argument indexing. For 'regular'
           predicates first-argument indexing is not applied if there are less than
           ARG0COUNT_MIN clauses. Also, if in the end the hashtable turns out to have a
           single entry only, it is abolished altogether.
        */

#if arg1index
        private const bool VARARG = true;

        public bool CreateFirstArgIndex()
        {
            return CreateFirstArgIndex(false); // false: do not create if it already exists
        }


        public bool CreateFirstArgIndex(bool force) // Create the index if the predicate qualifies
        {
            //return false; // testing only

#if persistent
      if (this is PersistentPredDescr) return false; // not applicable to persistent predicates
#endif

            if (arg0Index != null && !force) return false; // index already exists

            // check each clause whether with the addition of this clause the predicate still qualifies for first argument indexing.
            // Indexing y/n must be (re)cetermined after a file consult or an assert.

            ClauseNode c = clauseList;
            short arg0Count = 0;

            while (c != null)
            {
                if (c.Term.Arity != 0) // no first arg
                {
                    arg0Count++; // *number* of clauses will be relevant as well. Indexing not worthwile if only a few.

                    if (c.Term.Arg(0).IsVar) break; //
                }
                c = c.NextClause;
            }

            if (arg0Count < ARG0COUNT_MIN) return false;

            // second pass: build the index

            arg0Index = new Hashtable();
            c = clauseList;

            while (c != null)
            {
                string s;

                Term t = c.Term.Arg(0);

                if (t.IsVar)
                {
                    arg0Index[VARARG] = c; // stop immediately after having included the first variable

                    break;
                }
                else if (arg0Index[s = t.Functor] == null)
                {
                    //Console.WriteLine ("CreateFirstArgIndex -- just added {0} -> {1}", s, c.Term);
                    arg0Index[s] = c;
                }
                c = c.NextClause;
            }

            if (arg0Index.Count == 1) // e.g. c(a(1)), c(a(2)), c(a(3)), ...
            {
                arg0Index = null;

                return false;
            }
            else
                return true;
        }


        public bool IsFirstArgIndexed { get { return (arg0Index != null); } }

        public bool IsFirstArgMarked(ClauseNode c)
        {
            if (arg0Index == null) return false;

            Term t = c.Term.Arg(0);

            return (t.IsVar) ? (arg0Index[VARARG] == c) : (arg0Index[t.Functor] == c);
        }


        public ClauseNode FirstArgNonvarClause(string arg)
        {
            return (ClauseNode)arg0Index[arg];
        }


        public ClauseNode FirstArgVarClause()
        {
            return (ClauseNode)arg0Index[VARARG];
        }


        public void DestroyFirstArgIndex()
        {
            arg0Index = null;
        }
#endif

        public void DumpClauseList()
        {
            ClauseNode c = clauseList;

            while (c != null)
            {
                Console.WriteLine("DumpValues -- {0}", c);
                c = c.NextClause;
            }
        }


        public virtual ClauseNode GetClauseList(Term t, Term where)
        {
            return clauseList;
        }


        public override string ToString()
        {
            return string.Format("pd[{0}/{1} clauselist {2}]", name, arity, clauseList);
        }


#if debugging
        public void SetSpy(bool enabled, string functor, int arity, SpyPort setPorts, bool warn)
        {
            string spySet = "[";

            if (enabled)
            {
                spied = true;
                spyMode = SpyPort.none;

                foreach (SpyPort port in Enum.GetValues(typeof(SpyPort)))
                    if (setPorts > 0 && (setPorts | port) == setPorts)
                    {
                        spyMode |= port;

                        if (port != SpyPort.full)
                            spySet += port.ToString() + (port == SpyPort.redo ? "]" : ",");
                    }

                if (setPorts != SpyPort.none)
                    PrologIO.Message("Spying {0} enabled for {1}/{2}", spySet, functor, arity);
            }
            else if (spied) // nospy
            {
                spied = false;
                PrologIO.Message("Spying disabled for {0}/{1}", functor, arity);
            }
            else if (warn)
                PrologIO.Message("There was no spypoint on {0}/{1}", functor, arity);
        }


        public void ShowSpypoint()
        {
            if (spyMode == SpyPort.none) return;

            string spySet = "[";

            foreach (SpyPort port in Enum.GetValues(typeof(SpyPort)))
                if (port > 0 && port != SpyPort.full && (spyMode | port) == spyMode)
                    spySet += port.ToString() + (port == SpyPort.redo ? "]" : ",");

            PrologIO.WriteLine("{0}/{1}: {2}", Name, Arity, spySet);
        }
#endif
    }


    /*
      -------------------
      PersistentPredDescr
      -------------------
    */

#if persistent
  public abstract class PersistentPredDescr : PredicateDescr
  {
    protected string dbEntity = null;
    private   DataSet dataSet = new DataSet ();
    private   Hashtable queriesTable;
    protected Term query = null; // the term for which the persistent predicate is called
    public    Term Query { get { return query; } }
    protected DbLogin dbLogin;
    private   DbAccess dbAc = null; // there is not necessarily one database, so a DbAcces for each predicate
    protected DbAccess DbAc { get { return (dbAc == null) ? (dbAc = new DbAccess (dbLogin)) : dbAc; } } /// 'lazy' creation, only when needed
    private   DbMetaDataCollection metaDataCollection = null;
    public    DbLogin DbLogin { get { return dbLogin; } set { dbLogin = value; } }

    public DbMetaDataCollection MetaDataCollection
    { get
      {
        if (metaDataCollection == null) // get column data
          metaDataCollection = MetaDataCollectionEx (name, arity, dbEntity);

        return metaDataCollection;
      }
      set
      {
        metaDataCollection = value;
      }
    }
    public string DbEntity { get { return dbEntity; } set { dbEntity = (value == null ? null : value.ToUpper ()); } }
    public string Dbc { get { return dbLogin.Dbc; } }
    public string Uid { get { return dbLogin.Uid; } }
    public string Pwd { get { return dbLogin.Pwd; } }

    protected abstract DbMetaDataCollection MetaDataCollectionEx (string name, int arity, string dbEntity);

    public PersistentPredDescr (string m, string f, string n, int a, ClauseNode c) : base (m, f, n, a, c)
    {
      queriesTable = new Hashtable ();
    }


    public override ClauseNode GetClauseList (Term t, Term where)
    {
      return GetPersistentData (t, where);
    }


    protected abstract ClauseNode GetPersistentData (Term t, Term where);


    public Term PersistentInfo ()
    {
      Term result = Term.NULLLIST;

      for (int i = MetaDataCollection.Count - 1; i >= 0; i--)
      {
        DbDataItem di = MetaDataCollection [i];
        result = new ListTerm (di.ToTerm (), result);
      }
      return PersistentInfo (result); // wraps "table" or "procedure" around result
    }


    protected abstract Term PersistentInfo (Term metaDataList);

    public abstract void Assert (Term assertion);

    public abstract void Retract (Term Term, VarStack stack, Term where);


    protected DataRowCollection GetResultsetForSelect (string queryText, int keyStart)
    {
      string key = queryText.Substring (keyStart);
      DataTable dt;

      if ((dt = (DataTable)queriesTable [key]) == null) // query has not been executed yet
      {
        dt = dataSet.Tables [key];
        if (dt != null) dt.Clear ();  // first, clear whatever was stored under this key
        DbAc.FillDataSet (queryText, dataSet, key);
        queriesTable [key] = dt = dataSet.Tables [key];  // ... store result in cache
        //Console.WriteLine ("New GetResultsetForSelect");
      }
//      else
//      {
//        Console.WriteLine ("Cached GetResultsetForSelect");
//      }
      return (dt == null) ? null : dt.Rows;
    }


    protected void ExecuteSqlCommand (string command)
    {
      DbAc.ExecuteSqlCommand (command);
      InvalidateCache ();
    }



    public void InvalidateCache ()
    // called automatically after an assert or retract, or on user demand (persistent_uncache)
    // (or anything that causes a mismatch between the cache and the actual table content)
    {
      dataSet.Tables.Clear ();
      queriesTable.Clear ();
    }


    protected string Conj (ref bool first)
    {
      string c;

      if (first) { first = false; c = " WHERE"; } else c = " AND";

      return c;
    }
  }


  /*
    --------------
    TablePredDescr
    --------------
  */

  public class TablePredDescr : PersistentPredDescr
  {
    public TablePredDescr (string m, string f, string n, int a, ClauseNode c) : base (m, f, n, a, c)
    {
    }

    protected override ClauseNode GetPersistentData (Term t, Term where)
    {
      query = t;
      ListDictionary varNrs = new ListDictionary ();
      TableColumnCollection tcc = (TableColumnCollection)MetaDataCollection;

      if (tcc.Count != t.Arity)
        IO.Error ("Number of columns ({0}) in table/view '{1}' does not match the arity of persistent predicate {2}/{3}",
                  tcc.Count, dbEntity, name, arity);
      // get all output arguments from t and construct the corresponding SELECT-statement.

      StringBuilder fetchStat = new StringBuilder ("SELECT NULL");

      for (int i = 0; i < tcc.Count; i++)
        if (t.Args [i].IsVar) fetchStat.AppendFormat (", {0}", tcc [i].Name);

      fetchStat.Append (" FROM ");
      int keyStart = fetchStat.Length; // Dataset key, to test whether the query result has already been executed before
      fetchStat.Append (dbEntity);

      int colNo = 0;
      bool first = true;

      // construct a condition in a where-clause for each instantiated argument of t

      foreach (Term tt in t.Args)
      {
        string colName = tcc [colNo].Name;

        if (tt.IsAtom || tt.IsString)
          fetchStat.AppendFormat ("{0} {1}={2}", Conj (ref first), colName, Utils.EnsureQuoted (tt.Functor));
        else if (tt.IsNumber)
          fetchStat.AppendFormat ("{0} {1}={2}", Conj (ref first), colName, tt.Functor);
        else if (tt.IsVar)
        {
          object prevColNo;
          // check whether value occurred as argument before (anonymous vars always have a unique varNo)
          if ((prevColNo = varNrs [tt.VarNo]) != null) // ... yes
            fetchStat.AppendFormat ("{0} {1}={2}", Conj (ref first), tcc [(int)prevColNo].Name, colName);
          varNrs [tt.VarNo] = colNo; // save for comparison with next argument(s)
        }
        else
          IO.Error ("Illegal argument {0} in persistent term {1}", tt, t);

        colNo++;
      }

      if (where != null) // add the extra where-clause
      {
        // Copy the ref-table of column numers into a ref-table of corresponding column names
        ListDictionary varNames = new ListDictionary (); // ... in-situ modifications of Collection-values is not possible
        foreach (DictionaryEntry de in varNrs)
          varNames [(int)de.Key] = tcc [(int)de.Value].Name;
        fetchStat.AppendFormat ("{0} {1}", Conj (ref first), where.ToStringSql (varNames));
      }

      fetchStat.Append (";");
//Console.WriteLine ("TablePredDescr.GetPersistentData -- fetchStat = \"{0}\"", fetchStat);
      DataRowCollection drc = GetResultsetForSelect (fetchStat.ToString (), keyStart); // get the data

      return (drc == null || drc.Count == 0) ? null : new PersistentClauseNode (drc, 0, this);
    }


    public override void Assert (Term assertion)
    {
      StringBuilder insertStat = new StringBuilder ("INSERT INTO " + dbEntity + " VALUES (");
      TableColumnCollection tcc = (TableColumnCollection)MetaDataCollection;
      bool first = true;

      for (int i = 0; i < assertion.Arity; i++)
      {
        Term arg = assertion.Arg (i);
        if (first) first = false; else insertStat.Append (", ");

        if (arg.IsAtom || arg.IsString)
          insertStat.Append (Utils.EnsureQuoted (arg.Functor));
        else if (arg.IsNumber)
          insertStat.Append (arg.Functor);
        else if (arg.IsVar)
        {
          // check whether this column may be null
          if (!tcc [i].IsNullable)
            IO.Error ("Illegal attempt to insert a null-value in column '{0}' of table '{1}'",
                      tcc [i].Name, dbEntity);

          insertStat.Append ("null");
        }
        else
          IO.Error ("Illegal argument {0} for assert of persistent clause {1}", arg, assertion);
      }

      insertStat.Append (");");
//Console.WriteLine ("Assert -- about to ExecuteInsert {0}", insertStat.ToString ());
      ExecuteSqlCommand (insertStat.ToString ());
    }


    public override void Retract (Term Term, VarStack stack, Term where)
    {
      StringBuilder deleteStat = new StringBuilder ("DELETE FROM " + dbEntity);
      TableColumnCollection tcc = (TableColumnCollection)MetaDataCollection;
      ListDictionary varNrs = new ListDictionary ();
      bool first = true;

      for (int i = 0; i < Term.Arity; i++)
      {
        Term arg = Term.Arg (i);
        string colName = tcc [i].Name;

        if (arg.IsAtom || arg.IsString)
          deleteStat.AppendFormat ("{0} {1}={2}", Conj (ref first), colName, Utils.EnsureQuoted (arg.Functor));
        else if (arg.IsNumber)
          deleteStat.AppendFormat ("{0} {1}={2}", Conj (ref first), colName, arg.Functor);
        else if (arg.IsVar)
        {
          object prevColNo;
          // check whether value occurred as argument before (anonymous vars always have a unique varNo)
          if ((prevColNo = varNrs [arg.VarNo]) != null) // ... yes
            deleteStat.AppendFormat ("{0} {1}={2}", Conj (ref first), tcc [(int)prevColNo].Name, colName);
          varNrs [arg.VarNo] = i; // save for comparison with next argument(s)
        }
        else if (!arg.IsVar)
          IO.Error ("Illegal argument {0} for retract of persistent clause {1}", arg, Term);
      }

      if (where != null) // add the extra where-clause (if any)
      {
        // Copy the ref-table of column numers into a ref-table of corresponding column names
        ListDictionary varNames = new ListDictionary (); // ... in-situ modifications of Collection-values is not possible
        foreach (DictionaryEntry de in varNrs) varNames [(int)de.Key] = tcc [(int)de.Value].Name;
        deleteStat.AppendFormat ("{0} {1}", Conj (ref first), where.ToStringSql (varNames));
      }

      if (first)
        IO.Warning ("Retract -- DELETE without WHERE -- will NOT be executed !!!!!!!!");
      else
        ExecuteSqlCommand (deleteStat.ToString ());
    }


    protected override DbMetaDataCollection MetaDataCollectionEx (string name, int arity, string dbEntity)
    {
      return DbAc.GetTableColumnCollection (name, arity, dbEntity);
    }


    protected override Term PersistentInfo (Term metaDataList)
    {
      Term[] a = new Term [2] {new Term (Utils.MakeAtomic (dbEntity)), metaDataList};

      return new Term ("table", a);
    }
  }


  /*
    ------------------
    ProcedurePredDescr
    ------------------
  */

  public class ProcedurePredDescr : PersistentPredDescr
  {
    private bool isSelectable;
    private int  inputParamCount;
    private int  outputParamCount;
    public  int  InputParamCount  { get { return inputParamCount; } }
    public  int  OutputParamCount { get { return outputParamCount; } }
    public  bool IsSelectable { get { return isSelectable; } }

    public ProcedurePredDescr (string m, string f, string n, int a, ClauseNode c, bool s) : base (m, f, n, a, c)
    {
      isSelectable = s;
    }

    protected override ClauseNode GetPersistentData (Term t, Term where)
    {
      query = t;

      if (isSelectable)
      {
        ClauseNode result = GetSelectablePersistentData (t, where);
        Console.WriteLine ("GetPersistentData result = {0}", result);
        return result;
      }
      else if (where != null)
      {
        IO.Error ("where-clause in '{0}' not allowed for executable stored procedure '{1}'", where, dbEntity);
        return null;
      }
      else
        return GetExecutablePersistentData (t);
    }


    protected ClauseNode GetSelectablePersistentData (Term t, Term where)
    {
      ListDictionary varNrs = new ListDictionary ();
      ProcParamCollection ppc = (ProcParamCollection)MetaDataCollection;
      bool first;
      int  varNo;

      if (ppc.Count != t.Arity)
        IO.Error ("Number of parameters ({0}) for stored procedure '{1}' does not match the arity of persistent predicate {2}/{3}",
                  ppc.Count, dbEntity, name, arity);
      // get all arguments from t and construct the corresponding SELECT-statement.
      // The arguments must be either atomic or var, but may not be compound (apart from date(...))

      StringBuilder fetchStat = new StringBuilder ("SELECT NULL");

      for (int i = inputParamCount; i < ppc.Count; i++) fetchStat.AppendFormat (", {0}", ppc [i].Name); // output parameters only

      fetchStat.Append (" FROM ");
      int keyStart = fetchStat.Length; // Dataset key, to test whether the query result has already been executed before
      fetchStat.Append (dbEntity);

      // append stored procedure input parameters (if any)

      if (inputParamCount > 0)
      {
        fetchStat.Append (" (");
        varNo = 0;
        first = true;

        for (int i = 0; i < inputParamCount; i++)
        {
          if (first) first = false; else fetchStat.Append (", ");

          Term tt = t.Arg (varNo++);

          if (tt.IsAtom || tt.IsString)
            fetchStat.Append ("'" + tt.Functor + "'");
          else if (tt.IsNumber)
            fetchStat.Append (tt.Functor);
          else
            IO.Error ("Illegal stored procedure input parameter {0} in persistent term {1}", tt, t);
        }
        fetchStat.Append (")");
      }

      // construct a condition in a where-clause for each instantiated (output) argument of t

      varNo = 0; // t.Arg (0) is first output parameter (if any)
      first = true;

      for (int colNo = inputParamCount; colNo < ppc.Count; colNo++)
      {
        string colName = ppc [colNo].Name;
        Term   tt = t.Arg (varNo);

        if (tt.IsVar)
        {
          object prevColNo;
          // check whether value occurred as argument before (anonymous vars always have a unique varNo)
          if ((prevColNo = varNrs [tt.VarNo]) != null) // ... yes
            fetchStat.AppendFormat ("{0} {1}={2}", Conj (ref first), ppc [(int)prevColNo].Name, colName);
          varNrs [tt.VarNo] = colNo; // save for comparison with next argument(s)
        }
        else if (!(tt.IsAtom || tt.IsString || tt.IsNumber))
         IO.Error ("Illegal argument {0} in persistent term {1}", tt, t);

        varNo++;
      }

      // add the extra where-clause (if any)

      if (where != null)
      {
        // Copy the ref-table of column numers into a ref-table of corresponding column names
        ListDictionary varNames = new ListDictionary (); // ... in-situ modifications of Collection-values is not possible
        foreach (DictionaryEntry de in varNrs)
          varNames [(int)de.Key] = ppc [(int)de.Value].Name;
        fetchStat.AppendFormat ("{0} {1}", Conj (ref first), where.ToStringSql (varNames));
      }

      fetchStat.Append (";");
//Console.WriteLine ("ProcedurePredDescr.GetPersistentData -- fetchStat = \"{0}\"", fetchStat);
      DataRowCollection drc = GetResultsetForSelect (fetchStat.ToString (), keyStart); // get the data

      return (drc == null || drc.Count == 0) ? null : new PersistentClauseNode (drc, 0, this);
    }


    protected ClauseNode GetExecutablePersistentData (Term t)
    {
      ProcParamCollection ppc = (ProcParamCollection)MetaDataCollection;

      if (ppc.Count != t.Arity)
        IO.Error ("Number of parameters ({0}) for stored procedure '{1}' does not match the arity of persistent predicate {2}/{3}",
                  ppc.Count, dbEntity, name, arity);
      // get all output arguments from t and construct the corresponding SELECT-statement.

      DbAc.InitExecProcedure (dbEntity, inputParamCount);

      // set up input parameters
      for (int i = 0; i < inputParamCount; i++)
      {
        Term   arg   = t.Arg (i);
        string value = null;

        if (arg.IsAtom || arg.IsString)
          value = Utils.EnsureQuoted (arg.Functor);
        else if (arg.IsNumber)
          value = arg.Functor;
        else if (arg.IsVar)
          value = "null";
        else
          IO.Error ("Illegal stored procedure input parameter {0} in persistent term {1}", arg, t);

        DbAc.AddExecInputParameter (i, ppc [i], value);
      }

      // set up output parameters
      for (int i = inputParamCount; i < ppc.Count; i++) DbAc.AddExecOutputParameter (i, ppc [i]);

      // perform the procedure call
      Term result = DbAc.DoExecProcedure (dbEntity, query);
//Console.WriteLine ("GetExecutablePersistentData -- result = {0}", result);
      return new ClauseNode (result, null);
    }


    public override void Assert (Term assertion)
    {
      IO.Error ("Unable to insert in '{0}' (not a table)", dbEntity);
    }


    public override void Retract (Term Term, VarStack stack, Term where)
    {
      IO.Error ("Unable to delete from '{0}' (not a table)", dbEntity);
    }


    protected override DbMetaDataCollection MetaDataCollectionEx (string name, int arity, string dbEntity)
    {
      return DbAc.GetProcParamCollection (name, arity, dbEntity, out inputParamCount, out outputParamCount);
    }


    protected override Term PersistentInfo (Term metaDataList)
    {
      Term[] a = new Term [3] {
        new Term (Utils.MakeAtom (dbEntity)),
        metaDataList,
        new Term (isSelectable?"selectable":"executable")};

      return new Term ("procedure", a);
    }
  }
#endif
}
