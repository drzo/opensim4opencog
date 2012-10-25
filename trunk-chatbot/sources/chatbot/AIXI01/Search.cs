using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;



namespace MCAIXI0
{
    using reward_t = System.Double;
    using action_t = System.UInt64;
    using hash_t = System.UInt64;
    using symbol_t = System.Boolean;
    using visit_t = System.UInt64;
    using precept_t = System.UInt64;
    using age_t = System.UInt64;

    class SearchNode
    {
        const visit_t MinVisitsBeforeExpansion = 1;
        const int MaxDistanceFromRoot  = 100;


        bool m_chance_node; // true if this node is a chance node, false otherwise
        double  m_mean;      // the expected reward of this node
        visit_t m_visits;  // number of times the search node has been visited
        hash_t m_hash;
        Object m_mutex = new Object();

        public SearchNode(UInt64 hash, bool is_chance_node)
        {
            m_chance_node = is_chance_node;
            m_mean = 0.0;
            m_visits = 0;
            m_hash = hash;
        }

        // determine the expected reward from this node
        public double expectation()
        {
            return m_mean;
        }

        // number of times the search node has been visited
        public visit_t visits()
        {
            return m_visits;
        }

        // determine the next action to play
        //determine the next child to explore, NULL if no such child exists 
        public action_t selectAction(Agent agent, Random rng) 
        {
            // TODONE: implement
            // higher values encourage more exploration, less exploitation
            double ExploreBias = agent.horizon() * agent.maxReward();
            double UnexploredBias = 1000000000.0;

            //assert(!m_chance_node);

            action_t best_action = 0;
            double best_priority = double.NegativeInfinity;
            //Random rng = new Random ();
            for (UInt64 a=0; a < agent.numActions(); a++) 
            {

                SearchNode n = agent.spc.findNode(agent.hashAfterAction(a));
                //assert(n == NULL || n->m_chance_node);

                double priority, noise = rng.NextDouble() * 0.0001;

                // use UCB formula to compute priority
                if (n == null || n.visits() == 0) 
                {
                   priority =  UnexploredBias + noise;
                } 
                else 
                {
                    double pvisits = (double)(visits());
                    double cvisits = (double)(n.visits());
                    double bias = ExploreBias * Math.Sqrt(2.0 * Math.Log(pvisits) / cvisits);
                    priority = n.expectation() + bias + noise;
                }

                if (priority > best_priority) 
                {
                    best_action = a;
                    best_priority = priority;
                }
            }

            return best_action;

        }




	    // perform a sample run through this node and it's children,
	    // returning the accumulated reward from this sample run
        public double sample(Agent agent,Random rng, int dfr)
        {
            // TODO: implement
            if (dfr == (int) agent.horizon() * 2) return 0.0;

            ModelUndo undo = new ModelUndo (agent);
            double reward = 0.0;

            if (m_chance_node) 
            {  // handle chance nodes

                // generate a hypothetical percept
                symbol_list_t percept = new symbol_list_t(0); //(UInt64)(agent.m_obs_bits + agent.m_rew_bits));
                agent.genPerceptAndUpdate(rng, percept);

                // extract the reward for this transition, and
                // update the agent model
                reward = agent.rewardFromPercept(percept);

                SearchNode n = agent.spc.findOrCreateNode(agent.hash(), false);
                reward += n.sample(agent, rng, dfr + 1);
                agent.modelRevert(undo);

            } 
            else 
            {  // handle decision nodes

                lock(m_mutex)
                {
                
                // if we need to do a playout
                bool do_playout =
                visits() < MinVisitsBeforeExpansion           ||
                dfr >= MaxDistanceFromRoot                    ||
                agent.spc.search_node_pool.Count >= (int) agent.spc.MaxSearchNodes;

                if (do_playout) 
                {

                    //m_mutex.unlock();

                    reward = playout(agent, rng, (int)agent.horizon() - dfr / 2);

                } 
                else 
                {

                    // pick an action
                    UInt64  a = selectAction(agent, rng);
                    //m_mutex.unlock();

                    // update model, and recurse
                    agent.modelUpdate(a);
                    SearchNode n = agent.spc.findOrCreateNode(agent.hash(), true);
                    reward = n.sample(agent, rng, dfr + 1);
                    agent.modelRevert(undo);
                }
              }
            }

            { // update our statistics for this node
                lock(m_mutex)
                {
                double vc = (double)(m_visits);
                m_mean = (m_mean * vc + reward) / (vc + 1.0);
                m_visits++;
                }
            }

        return reward;
        }

        /* simulate a path through a hypothetical future for the agent
        within it's internal model of the world, returning the
        accumulated reward. */
        double  playout(Agent agent, Random rng, int playout_len) 
        {

            double start_reward = agent.reward();

            //ptr_vector<ModelUndo> undos;
            Stack undos = new Stack();

            for ( int i=0; i < playout_len; i++) 
            {

                undos.Push(new ModelUndo(agent));

                // generate action
                UInt64 a = agent.useSelfModel() ?
                agent.genAction(rng) : agent.selectRandomAction(rng);
                agent.modelUpdate(a);

                // generate percept
                symbol_list_t percept = new symbol_list_t (0);//agent.preceptBits ());
                undos.Push(new ModelUndo(agent));
                agent.genPerceptAndUpdate(rng, percept);
            }

            double rval = agent.reward() - start_reward;

            //boost::ptr_vector<ModelUndo>::reverse_iterator it = undos.rbegin();
            //int it = 0;
            //for (; it != undos.rend(); ++it) 
            //{
                //agent.modelRevert(it);
               
            //}

            // POP ALL
            while (undos.Count > 0)
            {
                agent.modelRevert((ModelUndo)undos.Pop());
            }
            return rval;
        }


    } // SearchNode


     class SearchSpace
     {
         public Hashtable search_node_pool = new Hashtable();
         public UInt64 MaxSearchNodes =1024 ;
        public SearchNode gbl_search_root = null;
        public SearchSpace()
         {
         }

        public SearchNode findNode(UInt64 hash)
        {
            if (search_node_pool.ContainsKey(hash)) return (SearchNode)search_node_pool[hash];
            return null;
        }
        public SearchNode findOrCreateNode(UInt64 hash, bool is_chance_node)
        {

            if (search_node_pool.ContainsKey(hash)) return (SearchNode)search_node_pool[hash];
            SearchNode rval = new SearchNode(hash,is_chance_node);
            search_node_pool[hash] = rval;
            return rval;

        }
 
     }

    class Search
    {
        public UInt64  MinVisitsBeforeExpansion = (UInt64)1;
        public int     MaxDistanceFromRoot  = 100;
        public UInt64  MaxSearchNodes=1;
 
        /* simulate a path through a hypothetical future for the agent
           within it's internal model of the world, returning the
           accumulated reward. */
         reward_t playout(Agent agent, Random rng,  UInt64 playout_len) {

            double start_reward = agent.reward();

            //boost::ptr_vector<ModelUndo> undos;

            Stack undos = new Stack();

            for ( int i=0; i < (int)playout_len; i++) {

                undos.Push(new ModelUndo(agent));

                // generate action
                UInt64  a = agent.useSelfModel() ?
                    agent.genAction(rng) : agent.selectRandomAction(rng);
                agent.modelUpdate(a);

                // generate percept
                symbol_list_t percept = new symbol_list_t (0);//agent.preceptBits());
                undos.Push(new ModelUndo(agent));
                agent.genPerceptAndUpdate(rng, percept);
            }

            double rval = agent.reward() - start_reward;

           // boost::ptr_vector<ModelUndo>::reverse_iterator it = undos.rbegin();
           // for (; it != undos.rend(); ++it) {
           //     agent.modelRevert(*it);
           // }
            while (undos.Count > 0) agent.modelRevert((ModelUndo)undos.Pop());


            return rval;
        }

         public action_t naiveMonteCarlo(Agent agent) 
                {

                    DateTime ti = DateTime.Now;
                    TimeSpan elapsed = ti - ti;

                    // determine the depth and number of seconds to search
                    double time_limit_ms = double.Parse((string)agent.options["cycle-length-ms"]);
                    double time_limit = time_limit_ms / 1000.0;

                    // sufficient statistics to compute the sample mean for each action
                    // std::vector<std::pair<reward_t, double> > r(agent.numActions());
                    double [] rfirst = new double [agent.numActions ()];
                    double [] rsecond = new double[agent.numActions()];

                    for ( int i = 0; i < (int) agent.numActions(); i++)
                    {
                       rfirst[i] = rsecond[i] = 0.0;
                    }

                    ModelUndo mu  =  new ModelUndo (agent);
                    UInt64 total_samples = 0;
                    UInt64 start_hist_len = agent.historySize();

                    do {  // we ensure each action always has one estimate
                         for (UInt64 i = 0; i < agent.numActions(); i++)
                            {

                                // make action
                                agent.modelUpdate(i);

                                // grab percept and determine immediate reward
                                symbol_list_t percept=new symbol_list_t (0); //agent.preceptBits ());
                                agent.genPerceptAndUpdate(agent.rng, percept);
                                double reward = agent.rewardFromPercept(percept);

                                // playout the remainder of the sequence
                                reward += playout(agent, agent.rng, agent.horizon() - 1);

                                rfirst[i]  += reward;
                                rsecond[i] += 1.0;

                                agent.modelRevert(mu);
                                //assert(start_hist_len == agent.historySize());

                                total_samples++;
                            }
                        elapsed = DateTime.Now - ti;    
                    } while (Math.Abs(elapsed.TotalMilliseconds) < time_limit_ms);

                    // determine best arm, breaking ties arbitrarily
                    double best = double.NegativeInfinity ;
                    action_t best_action = 0;
                    for (int i = 0; i < (int)agent.numActions(); i++) 
                    {

                        // assert(r[i].second > 0.0);
                        double noise = agent.rng.NextDouble() * 0.0001;

                        double x = rfirst[i] / rsecond[i] + noise;

                        if (x > best)
                        {
                            best = x;
                            best_action = (UInt64)i;
                        }
                    }

                    //agent.logbox.Items.Add( "naive monte-carlo decision based on " + total_samples + " samples.");

                    for ( int i=0; i < (int)agent.numActions(); i++)
                    {
                       //agent.logbox.Items.Add("action " + i + ": " +( rfirst[i] / rsecond[i]));
                    }
                    //agent.logbox.Items.Add(" best_action:" + best_action);
                    return best_action;
                    }
        
                /* initialise the monte-carlo tree search */
        public void initMCTS(Agent agent) 
                {
                    agent.spc.search_node_pool.Clear();
                }

            /* perform a monte-carlo tree search to determine the best action */
       public action_t mcts(Agent agent)
            {
                initMCTS(agent);

                // determine number of seconds to search for
                double time_limit = double.MaxValue;
                double time_limit_ms = 1000;
                if (agent.options.ContainsKey("cycle-length-ms")) 
                {
                     time_limit_ms = double.Parse((string)agent.options["cycle-length-ms"]);
                    time_limit = time_limit_ms / 1000.0;
                }

                UInt64  sims = UInt64.MaxValue;
                if (agent.options.ContainsKey("mc-simulations"))
                {
                   sims = UInt64.Parse ((string)agent.options["mc-simulations"]);
                }

                DateTime timer = DateTime.Now;
                TimeSpan elapsed = timer - timer;
                agent.spc.gbl_search_root = agent.spc.findOrCreateNode(agent.hash(), false);
                if (agent.spc.gbl_search_root == null)
                {
                    //throw SearchNodeAllocFailException();
                }

                TreeSampler Prober = new    TreeSampler(agent, timer, time_limit_ms, sims);
                do {  // we ensure each action always has one estimate
                    Prober.doTask();

                    elapsed = DateTime.Now - timer;    
                } while (Math.Abs(elapsed.TotalMilliseconds) < time_limit_ms);


                return selectBestMCTSAction(agent);
            }

            /* selects the best action determined by the MCTS statistics */
                public action_t selectBestMCTSAction(Agent agent)
            {
                ModelUndo mu = new ModelUndo(agent);
                action_t best_action = agent.selectRandomAction(agent.rng);
                double best_exp = double.NegativeInfinity;
                bool found = false;
                for (UInt64 a = 0; a < agent.numActions(); a++)
                {
                    SearchNode n = agent.spc.findNode(agent.hashAfterAction(a));
                    if (n != null)
                    {
                        double noise = agent.rng.NextDouble() * 0.0001;
                        double  exp = n.expectation() + noise;
                        if (exp > best_exp)
                        {
                            best_exp = n.expectation();
                            best_action = a;
                            found = true;
                        }
                       // agent.logbox.Items.Add("action " + a + ":" + exp + " visits " + n.visits() + " self-predicted probability :" + agent.getPredictedActionProb(a));
                    }
                }

                //agent.logbox.Items.Add(" selectBestMCTSAction=" + best_action +"found ="+found );


                return best_action;
            }


           public action_t search(Agent agent)
                {
                    action_t best = 0;
                    string controller = (string)agent.options["controller"];
                    if (controller == "mc")
                    {
                        best = naiveMonteCarlo(agent);
                    }
                    else if (controller == "mcts")
                    {
                        best = mcts(agent);
                    }
                    else
                    {
                        best = agent.selectRandomAction(agent.rng);
                    }
                return best;// TODO
                }
 
    }

    class TreeSampler
    {
        public Agent m_agent;
        public Random m_rng;
        public double m_time_limit;
        public UInt64 m_num_simulations;
        public UInt64 m_samples;
        public Random m_stat_mutex=new Random();
        public DateTime m_timer;

        public TreeSampler(Agent agent, DateTime timer, double time_limit_ms, UInt64 sims)
        {
            m_agent = agent;
            m_timer = timer;
            m_time_limit = time_limit_ms;
            m_num_simulations = sims;
            m_rng = new Random();
        }
        public void init()
        {
            lock (m_stat_mutex)
            {
                m_samples = 0;
            }
        }
        public UInt64 samples()
        {
            return m_samples;
        }

        public void doTask()
        {
            ModelUndo mu = new ModelUndo (m_agent);
            TimeSpan elapsed = DateTime.Now - m_timer;
            try
            {
                do
                {
                    lock (m_stat_mutex)
                    {
                        if (m_samples >= m_num_simulations) break;
                        m_samples++;
                    }
                    m_agent.spc.gbl_search_root.sample(m_agent, m_rng, 0);
                    elapsed = DateTime.Now - m_timer;
                } while (Math.Abs(elapsed.TotalMilliseconds) < m_time_limit);
            }
            catch (Exception e)
            {
                //m_agent.logbox.Items.Add(e.Message);
               // m_agent.logbox.Items.Add(e.StackTrace);
               // m_agent.logbox.Items.Add(e.InnerException);
            }
        }
    }
}
