using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;
    using Aima.Core.Search.Framework;

    public class MapAgent : SimpleProblemSolvingAgent 
    {
        private IMap map;

        private INotifyEnvironmentViews notifier;

        private DynamicState state = new DynamicState();

        private ISearch search;

        private String[] goalTests;

        private int goalTestPos;

        public MapAgent(IMap map, INotifyEnvironmentViews notifier, ISearch search) 
        {
            this.map = map;
            this.notifier = notifier;
            this.search = search;
        }

        public MapAgent(IMap map, INotifyEnvironmentViews notifier, ISearch search,
                int maxGoalsToFormulate) : base(maxGoalsToFormulate)
        {
            
            this.map = map;
            this.notifier = notifier;
            this.search = search;
        }

        public MapAgent(IMap map, INotifyEnvironmentViews notifier, ISearch search,
                string[] goalTests) : base(goalTests.Length)
        {
            ;
            this.map = map;
            this.notifier = notifier;
            this.search = search;
            this.goalTests = new String[goalTests.Length];
            goalTests.CopyTo(this.goalTests,0);
        }

        protected override IState UpdateState(IPercept p) 
        {
            var dp = (DynamicPercept) p;

            state.SetAttribute(DynAttributeNames.AgentLocation, dp
                    .GetAttribute(DynAttributeNames.PerceptIn));

            return state;
        }

        protected override object FormulateGoal()
        {
            object goal;
            if (null == goalTests) 
            {
                goal = map.RandomlyGenerateDestination();
            } 
            else 
            {
                goal = goalTests[goalTestPos];
                goalTestPos++;
            }
            notifier.NotifyViews("CurrentLocation=In("
                    + state.GetAttribute(DynAttributeNames.AgentLocation)
                    + "), Goal=In(" + goal + ")");

            return goal;
        }

        
        protected override Problem FormulateProblem(object goal) 
        {
            return new BidirectionalMapProblem(map, (string) state
                    .GetAttribute(DynAttributeNames.AgentLocation), (string) goal);
        }

        
        protected override IList<IAction> Search(Problem problem) 
        {
            var actions = new List<IAction>();
            
            var sactions = search.Search(problem);
            actions.AddRange(sactions);

            return actions;
        }

        protected override void NotifyViewOfMetrics()
        {
            var keys = this.search.GetMetrics().KeySet();
            foreach (var key in keys)
            {
                this.notifier.NotifyViews(String.Format("METRIC[{0}]={1}", key, this.search.GetMetrics().Get(key)));
            }
        }
    }
}
