using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl.AProg
{
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.7, page 4
    /// <code><![CDATA[
    /// function TABLE-DRIVEN-AGENT(percept) returns an action
    ///   persistent: percepts, a sequence, initially empty
    ///            table, a table of actions, indexed by percept sequences, initially fully specified
    ///         
    ///   append percept to end of percepts
    ///   action <- LOOKUP(percepts, table)
    ///   return action
    /// ]]></code>
    /// Figure 2.7 The TABLE-DRIVEN-AGENT program is invoked for each new percept and 
    /// returns an action each time. It retains the complete percept sequence in memory.
    /// </summary>
    public class TableDrivenAgentProgram : IAgentProgram 
    {
        private IList<IPercept> percepts = new List<IPercept>();

        //TODO: What is a good replacement for java's table?
        private Table<IList<IPercept>, string, IAction> table;

        private const string Action = "action";

        // persistent: percepts, a sequence, initially empty
        // table, a table of actions, indexed by percept sequences, initially fully
        // specified
        public TableDrivenAgentProgram(Dictionary<IList<IPercept>, IAction> perceptSequenceActions) 
        {

            //TODO: Is this rowHeaders initialization really doing what java code did?
            // 		List<List<Percept>> rowHeaders = new ArrayList<List<Percept>>(
            //		perceptSequenceActions.keySet());
            IList<IList<IPercept>> rowHeaders = new List<IList<IPercept>>(
                    perceptSequenceActions.Keys);

            IList<string> colHeaders = new List<string>();
            colHeaders.Add(Action);

            table = new Table<IList<IPercept>, string, IAction>(rowHeaders, colHeaders);

            foreach (List<IPercept> row in rowHeaders)
            {
                table.Set(row, Action, perceptSequenceActions[row]);
            }
        }

        // function TABLE-DRIVEN-AGENT(percept) returns an action
        public IAction Execute(IPercept percept) {
            // append percept to end of percepts
            this.percepts.Add(percept);

            // action <- LOOKUP(percepts, table)
            // return action
            return LookupCurrentAction();
        }

        private IAction LookupCurrentAction() {
            IAction action = null;

            action = this.table.Get(this.percepts, Action);
            if (null == action) {
                action = NoOpAction.NoOp;
            }

            return action;
        }
    }
}
