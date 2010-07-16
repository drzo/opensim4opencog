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

namespace RTParser.Prolog
{
    /*
      ----------------
      PredicateStorage
      ----------------
    */

    public class PredicateStorage
    {
        private Hashtable predTable;
        private Hashtable predefineds;
        private Hashtable moduleName;
        private Hashtable definedInCurrFile;
        private Hashtable isDiscontiguous;
        private Hashtable actionWhenUndefined;
        private string prevIndex = null;
        private bool allDiscontiguous = false; // should normally not be used (used for messy code from others)
        private const string SLASH = "/";

        public PredicateStorage()
        {
            predTable = new Hashtable();
            predefineds = new Hashtable();
            moduleName = new Hashtable();
            definedInCurrFile = new Hashtable();
            isDiscontiguous = new Hashtable();
            actionWhenUndefined = new Hashtable();
        }


        public void Reset()
        {
            predTable.Clear();
            predefineds.Clear();
            moduleName.Clear();
            definedInCurrFile.Clear();
            isDiscontiguous.Clear();
            actionWhenUndefined.Clear();
            prevIndex = null;
        }


        public PredicateDescr this[string key]
        {
            get { return (PredicateDescr)predTable[key]; }
            set { predTable[key] = value; }
        }


        public bool IsPredefined(string key)
        {
            return predefineds.Contains(key);
        }


        public void SetFailWhenUndefined(string f, int a)
        {
            actionWhenUndefined[Term.Key(f, a)] = "true";
        }


        public UndefAction ActionWhenUndefined(string f, int a)
        {
            object o;

            o = actionWhenUndefined[Term.Key(f, a)];

            return (o == null) ? UndefAction.unknown : (UndefAction)o;
        }

#if persistent
    public bool IsPersistent (Term t)
    {
      PredicateDescr pd = (PredicateDescr)predTable [t.KbKey];

      return (pd == null) ? false : pd is PersistentPredDescr;
    }


    public bool IsPersistent (string functor, int arity)
    {
      PredicateDescr pd = (PredicateDescr)predTable [Term.Key (functor, arity)];

      return (pd == null) ? false : pd is PersistentPredDescr;
    }
#endif

        public bool IsPredicate(string functor, int arity)
        {
            return this.Contains(Term.Key(functor, arity));
        }


#if debugging
        public bool SetSpy(bool enabled, string functor, int arity, Term list)
        {
            SpyPort ports;

            if (list == null)
                ports = SpyPort.full;
            else
            {
                ports = SpyPort.none;
                string s;

                while (list.Arity == 2)
                {
                    s = list.Arg(0).Functor;

                    try
                    {
                        ports |= (SpyPort)Enum.Parse(typeof(SpyPort), s);
                    }
                    catch
                    {
                        PrologIO.Error("Illegal value '{0}'", s);
                    }
                    list = list.Arg(1);
                }
            }

            PredicateDescr pd;

            if (arity == -1)
            {
                bool found = false;

                foreach (DictionaryEntry de in predTable)
                    if ((pd = (PredicateDescr)de.Value).Name == functor)
                    {
                        found = true;
                        pd.SetSpy(enabled, pd.Name, pd.Arity, ports, !enabled);
                    }

                if (!found) PrologIO.Error("Predicate does not exist: {0}", functor);

                return found;
            }
            else
            {
                if ((pd = (PredicateDescr)predTable[Term.Key(functor, arity)]) == null)
                {
                    PrologIO.Error("Predicate does not exist: {0}/{1}", functor, arity);

                    return false;
                }
                pd.SetSpy(enabled, functor, arity, ports, !enabled);
            }

            return true;
        }

        public void SetNoSpyAll()
        {
            PredicateDescr pd;

            foreach (DictionaryEntry de in predTable)
                ((pd = (PredicateDescr)de.Value)).SetSpy(false, pd.Name, pd.Arity, SpyPort.none, false);
        }

        public void ShowSpypoints()
        {
            foreach (DictionaryEntry de in predTable)
                ((PredicateDescr)de.Value).ShowSpypoint();
        }
#endif

        private PredicateDescr SetClauseList(PredEnum predType, string f, int a, ClauseNode c)
        {
            string key = Term.Key(f, a);
            PredicateDescr pd = this[key];

            if (pd == null)
            {
                switch (predType)
                {
#if persistent
          case PredEnum.table:
            this [key] = pd = new TablePredDescr (Globals.ConsultModuleName, Globals.ConsultFileName, f, a, c);
            break;
          case PredEnum.selproc:
            this [key] = pd = new ProcedurePredDescr (Globals.ConsultModuleName, Globals.ConsultFileName, f, a, c, true);
            break;
          case PredEnum.execproc:
            this [key] = pd = new ProcedurePredDescr (Globals.ConsultModuleName, Globals.ConsultFileName, f, a, c, false);
            break;
#endif
                    default:
                        this[key] = pd = new PredicateDescr(Globals.ConsultModuleName, Globals.ConsultFileName, f, a, c);
                        break;
                }
            }
            else
                pd.SetClauseListHead(c);

            pd.AdjustClauseListEnd();

            return pd;
        }


        public bool Contains(string key)
        {
            return (this[key] != null);
        }


        public override string ToString()
        {
            return predTable.ToString();
        }


        public int Consult(string fName)
        {
            Parser saveCurrentParser = Globals.CurrentParser;
            Parser parser = Globals.CurrentParser = new Parser(this);
            allDiscontiguous = false;

            try
            {
                prevIndex = null;
                definedInCurrFile.Clear();
                isDiscontiguous.Clear();
                actionWhenUndefined.Clear();
                Globals.ConsultFileName = fName.ToLower();
                Globals.ConsultModuleName = null;
                parser.Prefix = "&program";
                PrologIO.Write("--- Consulting {0} ... ", fName);
                parser.LoadFromFile(fName);
                PrologIO.WriteLine("{0} lines read", parser.LineCount);
            }
            finally
            {
                Globals.ConsultFileName = null;
                Globals.ConsultModuleName = null;
                Globals.CurrentParser = saveCurrentParser;
            }
            return parser.LineCount;
        }


        public void AddPredefined(ClauseNode clause)
        {
            Term Term = clause.Term;

            string key = Term.KbKey;

            PredicateDescr pd = this[key];

            if (pd == null)
            {
                predefineds[key] = "true"; // any value != null will do
                SetClauseList(PredEnum.session, Term.Functor, Term.Arity, clause); // create a pd
            }
            else if (prevIndex != null && key != prevIndex)
                PrologIO.Error("Definition for predefined predicate '{0}' must be contiguous", Term.Index);
            else
                pd.AppendToClauseList(clause);

            prevIndex = key;
        }


        public void SetDiscontiguous(Term t)
        {
            if (t == null || t.Functor != SLASH || !t.Arg(0).IsAtom || !t.Arg(1).IsInteger)
                PrologIO.Error("Illegal or missing argument '{0}' for discontiguous/1", t);

            // The predicate descriptor does not yet exist (and may even not come at all!)
            string key = Term.Key(t.Arg(0).Functor, t.Arg(1).Functor);

            //Console.WriteLine ("--- Setting discontiguous for {0} in definingFile {1}", key, Globals.ConsultFileName);
            isDiscontiguous[key] = "true";
        }


        public void SetDiscontiguous(bool mode)
        {
            allDiscontiguous = mode;
        }


        public enum UndefAction { fail, succeed, warning, error, unknown }

        public bool SetUndefPredAction(Term t, bool err)
        {
            Term pred = null;
            Term action = null;
            ArrayList args = null;
            string msg;
            bool result = true;

            t.IsPacked = false;
            if (t != null) args = t.ArgumentsToArrayList(false);

            bool OK =
              t != null &&
              (args.Count == 2) &&
              (pred = (Term)args[0]).Arity == 2 &&
              pred.Functor == SLASH &&
              pred.Arg(0).IsAtom &&
              pred.Arg(1).IsInteger;

            if (!OK)
            {
                result = false;
                msg = string.Format("Bad first argument '{0}' for undef_pred_action( <predicate>/<arity>, ...)", t.Arg(0));

                if (err) PrologIO.Error(msg); else PrologIO.Warning(msg);
            }

            OK =
              (action = (Term)args[1]) != null &&
              (action.Functor == "fail" ||
                //          action.Functor == "succeed" ||
                action.Functor == "error" ||
                action.Functor == "warning");

            if (!OK)
            {
                result = false;
                msg = string.Format("Bad second argument '{0}' for undef_pred_action( ..., fail/succeed/warning)", action);

                if (err) PrologIO.Error(msg); else PrologIO.Warning(msg);
            }

            if (result)
            {
                string key = Term.Key(pred.Arg(0).Functor, pred.Arg(1).Functor);
                actionWhenUndefined[key] = (UndefAction)Enum.Parse(typeof(UndefAction), action.Functor, false);
            }

            return result;
        }


        private enum PredEnum { session, table, execproc, selproc } // table and view are treated identically

#if persistent
    public void SetPersistent (Term t)
    {
      Term pred       = null;
      Term db_info    = null;  // table( <name>) or procedure( <name>, <executable/selectable>)
      bool isTable    = false;
      bool isExec     = false; // executable stored procedure (as opposed to selectable)
      Term dbEntity   = null;
      DbLogin dbLogin = null;
      ArrayList args  = null;
      PredEnum predType;

      t.IsPacked = false;
      if (t != null) args = t.ArgumentsToArrayList (false);
      //for (int i = 0; i < args.Count; i++) Console.WriteLine ("args [{0}] = {1}", i, args [i]);

      bool OK =
        t != null &&
        (args.Count == 2 || args.Count == 5) &&
        (pred = (Term)args [0]).Arity == 2 &&
        pred.Functor == SLASH &&
        pred.Arg (0).IsAtom &&
        pred.Arg (1).IsInteger;

      if (!OK)
        IO.Error ("Bad first argument '{0}' for persistent( <predicate>/<arity>, ...)", t.Arg (0));

      OK =
        (db_info = (Term)args [1]) != null &&
        ( (isTable = (db_info.Functor == "table" || db_info.Functor == "view")) ||
          db_info.Functor == "procedure" ||
          db_info.Functor == "proc") &&
          db_info.Arity > 0 &&
        ( (dbEntity = db_info.Arg (0)).IsAtom || dbEntity.IsString );

      if (!OK)
        IO.Error ("Bad second argument '{0}' for persistent( ..., table/view/procedure(...))", db_info);

      if (isTable)
      {
        if (db_info.Arity != 1)
          IO.Error ("Bad second argument '{0}' for persistent( ..., table/view( <db_entity_name>))", db_info);

        predType = PredEnum.table;
      }
      else
      {
        string invocation;

        OK = (db_info.Arity == 2) &&  // procedure( <name>, <executable/selectable>)
             ( (isExec = ((invocation = db_info.Arg (1).Functor) == "executable" || invocation == "exec")) ||
               invocation == "selectable" || invocation == "select" );

        if (!OK)
          IO.Error ("Bad second argument '{0}' for persistent( ..., procedure( <db_entity_name>, [selectable|executable]))", t.Arg (1));

        predType = isExec ? PredEnum.execproc : PredEnum.selproc;
      }

      string functor = pred.Arg (0).Functor;
      int    arity   = Convert.ToInt32 (pred.Arg (1).Functor);
      string index   = Term.Key (functor, arity);

      if (predefineds [index] != null)
        IO.Error ("Predefined predicate '{0}/{1}' cannot be declared as persistent", functor, arity);

      if (args.Count == 5)
      {
        for (int i = 2; i <= 4; i++)
          if (!(((Term)args [i]).IsAtom || ((Term)args [i]).IsString))
            IO.Error ("Argument '{0}' not an atom or string", (Term)args [i]);

        dbLogin = new DbLogin (((Term)args [2]).Functor, ((Term)args [3]).Functor, ((Term)args [4]).Functor);
      }

      PredicateDescr pd = this [index];

      if (pd != null) // apparently already defined
      {
        string definingFile = (pd.DefiningFile == Globals.ConsultFileName) ? "this file" : pd.DefiningFile;

        if (!(pd is PersistentPredDescr))
          IO.Error ("Predicate '{0}/{1}' cannot be declared as persistent (predicate defined in {2})", functor, arity, definingFile);
      }

      pd = SetClauseList (predType, functor, arity, null);

      ((PersistentPredDescr)pd).DbEntity = dbEntity.Functor;
      ((PersistentPredDescr)pd).DbLogin  = dbLogin;
    }


    public void SetUnpersistent (Term t)
    {
      Term s;

      if ( t == null ||
           t.Arity != 1 ||
           (s = t.Arg (0)).Arity != 2 ||
           s.Functor != SLASH ||
           !s.Arg (0).IsAtom ||
           !s.Arg (1).IsInteger )
        IO.Error ("Bad argument(s) {0} for unpersistent( <predicate>/<arity>)", t);

      string functor = t.Arg (0).Arg (0).Functor; // do not touch case
      int    arity   = Convert.ToInt32 (t.Arg (0).Arg (1).Functor);
      string key     = Term.Key (functor, arity);

      if (predefineds [key] != null)
        IO.Error ("Predefined predicate '{0}/{1}' cannot be declared as unpersistent", functor, arity);

      PredicateDescr pd = this [key];

      if (pd == null || !(pd is PersistentPredDescr))
        IO.Error ("Predicate '{0}/{1}' was not declared as persistent", functor, arity);
      else
        this [key] = null;
    }
#else
        public void SetPersistent(Term t)
        {
            PrologIO.Error("Persistent predicates are not available in this version");
        }

        public void SetUnpersistent(Term t)
        {
            PrologIO.Error("Persistent predicates are not available in this version");
        }
#endif

#if persistent
    public Term PersistentInfo (string functor, string arity)
    {
      string index = Term.Key (functor, arity);

      PredicateDescr pd = this [index];

      if (pd == null || !(pd is PersistentPredDescr))
      {
        IO.Error ("Predicate '{0}/{1}' was not declared as persistent", functor, arity);
        return null;
      }
      return ((PersistentPredDescr)pd).PersistentInfo ();
    }
#endif

        public void SetModuleName(string n)
        {
            object o = moduleName[n];
            string currFile = Globals.ConsultFileName;

            if (o == null)
            {
                moduleName[n] = currFile;
                Globals.ConsultModuleName = null;
            }
            else if ((string)o != currFile)
                PrologIO.Error("Module name {0} already declared in definingFile {1}", n, (string)o);

            // ACTUAL FUNCTIONALITY TO BE IMPLEMENTED
        }


        public void AddClause(ClauseNode clause)
        {
            Term Term = clause.Term;

            string key = Term.KbKey;
            string index = Term.Index;

            if (predefineds.Contains(key))
            {
                PrologIO.WarnOrError("Modification of predefined predicate {0} not allowed", index);
                return;
            }

            if (prevIndex == key) // previous clause was for the same predicate
            {
                PredicateDescr pd = this[key];
                pd.AppendToClauseList(clause);
            }
            else // first predicate or different predicate
            {
                PredicateDescr pd = this[key];

                if (definedInCurrFile[key] == null) //  very first clause of this predicate in this file -- reset at start of consult
                {
                    // check whether it was defined as persistent

                    if (pd != null && pd.DefiningFile != Globals.ConsultFileName)
                    {
#if persistent
            if (pd is PersistentPredDescr)
              IO.Error ("No predicate definitions allowed for persistent predicate '{0}'", index);
            else
#endif
                        PrologIO.Error("Predicate '{0}' is already defined in {1}", index, pd.DefiningFile);
                    }
                    definedInCurrFile[key] = true;
                    pd = SetClauseList(PredEnum.session, Term.Functor, Term.Arity, clause); // implicitly erases all previous definitions
                    pd.IsDiscontiguous = (isDiscontiguous[key] != null || allDiscontiguous);
                    // NOTE: SetClauseListHead does not reset all remaining fields in the possibly existing PredicateDescr -- look at this later
                    prevIndex = key;
                }
                else // not the first clause. First may be from another definingFile (which is an error). If from same, IsDiscontiguous must hold
                {
                    if (pd.IsDiscontiguous)
                    {
                        if (pd.DefiningFile == Globals.ConsultFileName)
                            pd.AppendToClauseList(clause);
                        else // OK
                            PrologIO.Error("Discontiguous predicate {0} must be in one file (also found in {1})", index, pd.DefiningFile);
                    }
                    else if (pd.DefiningFile == Globals.ConsultFileName) // error
                        PrologIO.Error("Predicate '{0}' occurs discontiguously but is not declared as such", index);
                    else
                        PrologIO.Error("Predicate '{0}' is already defined in {1}", index, pd.DefiningFile);
                }
            }
        }


        public void Assert(Term assertion, bool asserta)
        {
            assertion = assertion.CleanCopy(); // make a fresh copy

            Term head;
            TermNode body = null;

            if (assertion.Functor == Parser.IMPLIES)
            {
                head = assertion.Arg(0);
                body = assertion.Arg(1).ToGoalList();
            }
            else
                head = assertion;

            if (head.IsVar) PrologIO.Error("Cannot assert a variable as predicate head");

            string key = head.KbKey;

            // A predefined predicate (which may also be defined as operator) may not be redefined.
            // Operators ':-', ',', ';', '-->', '->' (precedence >= 1000) may also not be redefined.

            if (predefineds.Contains(key) || (head.Precedence >= 1000))
                PrologIO.Error("assert cannot be applied to predefined predicate or system operator {0}", assertion.Index);

            PredicateDescr pd = (PredicateDescr)predTable[key];

#if persistent
      if (pd != null && pd is PersistentPredDescr)
      {
        ((PersistentPredDescr)pd).Assert (head);

        return;
      }
#endif

            ClauseNode newC = new ClauseNode(head, body);

            if (pd == null) // first head
            {
                SetClauseList(PredEnum.session, head.Functor, head.Arity, newC);
                ResolveIndices();
            }
            else if (asserta) // at beginning
            {
                newC.NextClause = pd.ClauseNode; // pd.ClauseNode may be null
                SetClauseList(PredEnum.session, head.Functor, head.Arity, newC);
#if arg1index
                pd.CreateFirstArgIndex(); // re-create
#endif
            }
            else // at end
            {
                pd.AppendToClauseList(newC);
#if arg1index
                pd.CreateFirstArgIndex(); // re-create
#endif
            }
        }


        public bool Retract(Term t, VarStack varStack, Term where)
        {
            string key = t.KbKey;

            if (predefineds.Contains(key))
                PrologIO.Error("retract of predefined predicate {0} not allowed", key);

            PredicateDescr pd = this[key];

            if (pd == null) return false;

#if persistent
      if (pd is PersistentPredDescr)
      {
        ((PersistentPredDescr)pd).Retract (t, varStack, where);

        return true;
      }
#endif

            ClauseNode c = pd.GetClauseList(null, null);
            ClauseNode prevc = null;
            Term cleanTerm;
            int top;

            while (c != null)
            {
                cleanTerm = c.Term.CleanCopy();

                top = varStack.Count;

                if (cleanTerm.Unify(t, varStack)) // match found -- remove this term from the chain
                {
                    if (prevc == null) // remove first clause
                    {
                        if (c.NextClause == null) // we are about to remove the last remaining clause for this predicate
                        {
                            predTable.Remove(key);        // ... so remove its PredicateDescr as well
#if arg1index
                            pd.CreateFirstArgIndex(); // re-create
#endif
                            ResolveIndices();
                        }
                        else
                            pd.SetClauseListHead(c.NextClause);
                    }
                    else // not the first
                    {
                        prevc.NextClause = c.NextClause;
                        prevc = c;
                        pd.AdjustClauseListEnd();
#if arg1index
                        pd.CreateFirstArgIndex(); // re-create
#endif
                    }

                    return true; // possible bindings must stay intact (e.g. if p(a) then retract(p(X)) yields X=a)
                }

                Term s;
                for (int i = varStack.Count - top; i > 0; i--) // unbind all vars that got bound by the above Unification
                {
                    s = (Term)varStack.Pop();
                    s.Unbind();
                }

                prevc = c;
                c = c.NextClause;
            }

            ResolveIndices();

            return false;
        }


        public bool RetractAll(Term t, VarStack varStack) // should *always* return true ?????
        {
            // remark: first-argument indexing is not affected by deleting clauses

            string key = t.KbKey;

            if (predefineds.Contains(key))
                PrologIO.Error("retract of predefined predicate {0} not allowed", key);

            PredicateDescr pd = this[key];

            if (pd == null) return true;

#if persistent
      if (pd is PersistentPredDescr)
      {
        ((PersistentPredDescr)pd).Retract (t, varStack, null);

        return true;  /////////////JPO persistent retract always to succeed ????
      }
#endif

            ClauseNode c = pd.GetClauseList(null, null);
            ClauseNode prevc = null;
            bool match = false;

            while (c != null)
            {
                Term cleanTerm = c.Term.CleanCopy();

                if (cleanTerm.Unifiable(t, varStack)) // match found -- remove this term from the chain
                {
                    match = true; // to indicate that at least one term was found

                    if (prevc == null) // remove first clause
                    {
                        if (c.NextClause == null) // we are about to remove the last remaining clause for this predicate
                        {
                            predTable.Remove(key);        // ... so remove its PredicateDescr as well

                            break;
                        }
                        else
                            pd.SetClauseListHead(c.NextClause);
                    }
                    else // not the first
                    {
                        prevc.NextClause = c.NextClause;
                        prevc = c;
                    }
                }
                else
                    prevc = c;

                c = c.NextClause;
            }

            if (match)
            {
#if arg1index
                pd.DestroyFirstArgIndex(); // rebuild by ResolveIndices()
#endif
                pd.AdjustClauseListEnd();
                ResolveIndices();
            }

            return true;
        }


        public bool Abolish(string functor, string arity)
        {
            string key = Term.Key(functor, arity);

            if (predefineds.Contains(key))
                PrologIO.Error("abolish of predefined predicate '{0}/{1}' not allowed", functor, arity);

            PredicateDescr pd = this[key];

            if (pd == null) return false;

#if persistent
      if (pd is PersistentPredDescr)
        IO.Error ("abolish of persistent predicate '{0}/{1}' not allowed", functor, arity);
#endif

            predTable.Remove(key);

#if arg1index
            pd.DestroyFirstArgIndex(); // rebuild by ResolveIndices()
#endif
            ResolveIndices();

            return true;
        }


        private bool ListClause(PredicateDescr pd, string functor, int arity, int seqno)
        {
            ClauseNode clause = null;
            string details;

#if persistent
      if (pd is PersistentPredDescr)
      {
        details = "persistent, details: " + pd.DefiningFile;
      }
      else
#endif
            {
                if ((clause = pd.GetClauseList(null, null)) == null) return false;

                details = "source: " + pd.DefiningFile;
            }

#if arg1index
            if (pd.IsFirstArgIndexed) details += "; arg1-indexed (jump points marked with '.')";
#endif

            Console.WriteLine("\n{0}/{1}: ({2}) {3}", functor, arity,
                               details,
                               ((seqno == 1) ? "" : ("(" + seqno.ToString() + ")")));

            while (clause != null)
            {
                TermNode next;

#if arg1index
                // prefix a clause that is pointed to by first-argument indexing with '.'
                Console.Write(" {0}{1}", (pd.IsFirstArgMarked(clause)) ? "." : " ", clause.Term);
#else
        Console.Write("  {0}", clause.Term);
#endif

                if ((next = clause.NextNode) != null)
                {
                    BI builtinId = next.BuiltinId;
                    Console.Write(" :-\n{0}", (builtinId == BI.none) ? next.ToString() : builtinId.ToString());
                }
                Console.WriteLine(".");
                clause = clause.NextClause;
            }

            return true;
        }


        public bool ListAll(string functor, int arity, bool showPredefined, bool showUserDefined)
        {
            bool result = false; // no such predicate assumed
            PredicateDescr pd;

            SortedList sl = new SortedList(); // for sorting the predicates alphabetically

            foreach (DictionaryEntry de in predTable)
            {
                pd = (PredicateDescr)de.Value;

                if (functor == null || functor == pd.Name)
                {
                    bool isPredefined = IsPredefined((string)de.Key);

                    if ((showPredefined && showUserDefined ||
                          showPredefined && isPredefined ||
                          showUserDefined && !isPredefined) &&
                         (arity == -1 || arity == pd.Arity))
                        sl.Add(pd.Name + pd.Arity.ToString(), pd);
                }
            }

            int seqNo = 0;

            foreach (DictionaryEntry de in sl)
                result = ListClause(pd = (PredicateDescr)de.Value, pd.Name, pd.Arity, ++seqNo) || result;

            return result;
        }


        /*
          ---------------------------------------------
          ResolveIndices (functor/arity-key resolution)
          ---------------------------------------------
        */

        public void ResolveIndices()
        {
            PredicateDescr pd;

            foreach (DictionaryEntry de in predTable) // traverse all program predicates
            {
                ResolveIndex(pd = (PredicateDescr)de.Value);
#if arg1index
                pd.CreateFirstArgIndex(); // check whether first-argument indexing is applicable, and build the index if so
#endif
            }
        }


        private void ResolveIndex(PredicateDescr pd)
        {
#if persistent
      if (pd is PersistentPredDescr) return; // does not have a clauseTerm
#endif

            ClauseNode clause = pd.GetClauseList(null, null);

            while (clause != null) // iterate over all clauses of this predicate. ClauseNode.Term contains predicate clauseHead
            {
                Term clauseHead = clause.Term; // clause = clauseHead :- clauseTerm*
                TermNode clauseTerm = clause.NextNode;

                while (clauseTerm != null) // non-facts only. Iterate over all clauseTerm-terms t of this clause
                {
                    if (clauseTerm.BuiltinId == BI.none) clauseTerm.PredDescr = this[clauseTerm.Term.KbKey];
                    // builtins (>=0) are handled differently (in Execute ())

                    clauseTerm = clauseTerm.NextNode;
                }
                clause = clause.NextClause;
            }
            return;
        }


        public void FindUndefineds()
        {
            SortedList sd = new SortedList();

            foreach (DictionaryEntry de in predTable) FindUndefined(sd, (PredicateDescr)de.Value);

            PrologIO.WriteLine("The following predicates are undefined:");

            foreach (DictionaryEntry de in sd) Console.WriteLine("  {0}", de.Key);
        }


        private void FindUndefined(SortedList sd, PredicateDescr pd)
        {
#if persistent
      if (pd is PersistentPredDescr) return;
#endif

            ClauseNode clause = pd.GetClauseList(null, null);
            Term clauseHead;
            TermNode clauseTerm;

            while (clause != null) // iterate over all clauses of this predicate
            {
                clauseHead = clause.Term;
                clauseTerm = clause.NextNode;

                while (clauseTerm != null) // non-facts only. Iterate over all clauseTerm-terms t of this clause
                {
                    if (clauseTerm.BuiltinId == BI.none && clauseTerm.PredDescr == null) sd[clauseTerm.Term.Index] = null;

                    clauseTerm = clauseTerm.NextNode;
                }
                clause = clause.NextClause;
            }
            return;
        }

    }
}
