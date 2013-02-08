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
        private readonly List<Action> OnPreUseHooks = new List<Action>();
        public void runPreProverHooks()
        {
            IList<Action> hooksTodo = null;
            lock (OnPreUseHooks)
            {
                if (OnPreUseHooks.Count == 0) return;
                hooksTodo = LockInfo.CopyOf(OnPreUseHooks);
                OnPreUseHooks.Clear();
            }
            int completed = 0;
            foreach (Action list in hooksTodo)
            {
                try
                {
                    list();
                    completed++;
                }
                catch (Exception e)
                {
                    lock (OnPreUseHooks)
                    {
                        for (int i = completed + 1; completed < hooksTodo.Count; i++)
                        {
                            OnPreUseHooks.Add(hooksTodo[i]);
                        }
                    }
                    throw ErrorBadOp("Prologs OnPreUseHooks ERROR: " + list + " " + e);
                }
            }
            // once more incase more hooks got added
            runPreProverHooks();
        }

        public void AddOnPreUseHooks(Action act)
        {
            lock (OnPreUseHooks)
            {
                OnPreUseHooks.Add(act);
            }
        }

        public ProveResult TripleQuery(Term thisTerm, PartListImpl goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            // bagof(Term, ConditionTerm, ReturnList)
            //  PartList goalList = (PartList)goalIn;

            Part collect0 = value((Part)thisTerm.ArgList[0], environment);
            Part subgoal = value((Part)thisTerm.ArgList[1], environment);
            Part into = value((Part)thisTerm.ArgList[2], environment);

            Part collect = renameVariables(collect0, level, thisTerm);
            //var newGoal = new Term(subgoal.name, renameVariables(subgoal.ArgList, level, thisTerm));
            Term newGoal = new Term(subgoal.fname, false,
                                    (PartListImpl)renameVariables(((PartListImpl)subgoal), level, thisTerm));
            newGoal.parent = thisTerm;

            //var newGoals = [];
            //newGoals[0] = newGoal;
            PartListImpl newGoals = new PartListImpl();
            newGoals.AddPart(newGoal);

            // Prove this subgoal, collecting up the environments...
            PartListImpl anslist = new PartListImpl();
            anslist.renumber = -1;
            var ret = prove(newGoals, environment, db, level + 1, BagOfCollectFunction(collect, anslist));

            // Turn anslist into a proper list and unify with 'into'

            // optional here: nil anslist -> fail?
            Part answers = Atom.FromSource(FUNCTOR_NIL);

            /*
            print("Debug: anslist = [");
                for (var j = 0; j < anslist.length; j++) {
                    anslist[j].print();
                    print(", ");
                }
            print("]\n");
            */

            for (int i = anslist.Arity; i > 0; i--)
            {
                answers = MakeList(anslist.ArgList[i - 1], answers);
            }

            //print("Debug: unifying "); into.print(); print(" with "); answers.print(); print("\n");
            var env2 = unify(into, answers, environment);

            if (env2 == null)
            {
                //print("Debug: bagof cannot unify anslist with "); into.print(); print(", failing\n");
                return null;
            }

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, env2, db, level + 1, reportFunction);
        }

        // Aux function: return the reportFunction to use with a bagof subgoal
        public reportDelegate TripleQueryCollectFunction(Part collect, PartListImpl anslist)
        {
            return delegate(PEnv env)
            {
                /*
                print("DEBUG: solution in bagof/3 found...\n");
                print("Value of collection term ");
                collect.print();
                print(" in this environment = ");
                (value(collect, env)).print();
                print("\n");
                printEnv(env);
                */
                // Rename this appropriately and throw it into anslist
                anslist.AddPart(renameVariables(value(collect, env), anslist.renumber--, null));
                return true;
            };
        }


        public ProveResult IstQuery(Term thisTerm, PartListImpl goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            // bagof(Term, ConditionTerm, ReturnList)
            //  PartList goalList = (PartList)goalIn;

            Part collect0 = value((Part)thisTerm.ArgList[0], environment);
            Part subgoal = value((Part)thisTerm.ArgList[1], environment);
            Part into = value((Part)thisTerm.ArgList[2], environment);

            Part collect = renameVariables(collect0, level, thisTerm);
            //var newGoal = new Term(subgoal.name, renameVariables(subgoal.ArgList, level, thisTerm));
            Term newGoal = new Term(subgoal.fname, false,
                                    (PartListImpl)renameVariables(((PartListImpl)subgoal), level, thisTerm));
            newGoal.parent = thisTerm;

            //var newGoals = [];
            //newGoals[0] = newGoal;
            PartListImpl newGoals = new PartListImpl();
            newGoals.AddPart(newGoal);

            // Prove this subgoal, collecting up the environments...
            PartListImpl anslist = new PartListImpl();
            anslist.renumber = -1;
            var ret = prove(newGoals, environment, db, level + 1, BagOfCollectFunction(collect, anslist));

            // Turn anslist into a proper list and unify with 'into'

            // optional here: nil anslist -> fail?
            Part answers = Atom.FromSource(FUNCTOR_NIL);

            /*
            print("Debug: anslist = [");
                for (var j = 0; j < anslist.length; j++) {
                    anslist[j].print();
                    print(", ");
                }
            print("]\n");
            */

            for (int i = anslist.Arity; i > 0; i--)
            {
                answers = MakeList(anslist.ArgList[i - 1], answers);
            }

            //print("Debug: unifying "); into.print(); print(" with "); answers.print(); print("\n");
            var env2 = unify(into, answers, environment);

            if (env2 == null)
            {
                //print("Debug: bagof cannot unify anslist with "); into.print(); print(", failing\n");
                return null;
            }

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, env2, db, level + 1, reportFunction);
        }

        // Aux function: return the reportFunction to use with a bagof subgoal
        public reportDelegate IstQueryCollectFunction(Part collect, PartListImpl anslist)
        {
            return delegate(PEnv env)
            {
                /*
                print("DEBUG: solution in bagof/3 found...\n");
                print("Value of collection term ");
                collect.print();
                print(" in this environment = ");
                (value(collect, env)).print();
                print("\n");
                printEnv(env);
                */
                // Rename this appropriately and throw it into anslist
                anslist.AddPart(renameVariables(value(collect, env), anslist.renumber--, null));
                return true;
            };
        }

    }
}