using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;

    public class SearchAgent : AbstractAgent 
    {
        protected IList<IAction> ActionList;

        private IEnumerator<IAction> actionEnumerator;

        private Metrics searchMetrics;

        public SearchAgent(Problem p, ISearch search) 
        {
            ActionList = search.Search(p);
            this.actionEnumerator = ActionList.GetEnumerator();
            searchMetrics = search.GetMetrics();
        }

        public override IAction Execute(IPercept p)
        {
            if (this.actionEnumerator.MoveNext())
            {
                return this.actionEnumerator.Current;
            }
            return NoOpAction.NoOp;
        }

        public bool IsDone() {
            //TODO: MoveNext works differently than java's HasNext - make sure that it's ok
            return !this.actionEnumerator.MoveNext();
        }

        public IList<IAction> GetActions() 
        {
            return this.ActionList;
        }

        public Dictionary<string, string> GetInstrumentation() 
        {
            var retVal = new Dictionary<string, string>();
            foreach (var key in this.searchMetrics.KeySet())
            {
                string value = this.searchMetrics.Get(key);
                retVal[key] = value;
                
            }
            return retVal;
        }
    }
}
