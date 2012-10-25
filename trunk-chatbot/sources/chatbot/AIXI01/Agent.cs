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

    //typedefs
    // age_t 	UInt64 age();

    // reward_t double
    // action UInt64
    // precept_t UInt64
    // symbol_t bool

    // the program's keyword/value option pairs
//typedef std::map<std::string, std::string> options_t;
    // HASHTABLE

    // a list of symbols
//typedef std::vector<symbol_t> symbol_list_t;
    // BitArray

    // Are all the BitArrays of the right size ???

        class Agent
        {

            // agent properties
            public UInt64 m_actions;      // number of actions
            public int m_actions_bits; // number of bits to represent an action
            public int m_obs_bits;     // number of bits to represent an observation
            public int m_rew_bits;     // number of bits to represent a reward
            public UInt64 m_horizon;            // length of the search horizon

            // Context Tree representing the agent's beliefs
            public ContextTree m_ct;
            public ContextTree m_self_model;

            // How many time cycles the agent has been alive
            public age_t m_time_cycle;

            // The total reward received by the agent
            public reward_t m_total_reward;

            // True if the last update was a percept update
            public bool m_last_update_percept;

            // Context Tree depth
            public UInt64 ct_depth;

            public hash_t m_hash;

            public SearchSpace spc;

            public bool m_use_self_model = true;

            public bool m_base2_reward_encoding = false;

            // External stuffs
            public Hashtable  options = new Hashtable();
            public Random rng = new Random();
            //public ListBox logbox = null;


	        // construct a learning agent from the command line arguments
	        public Agent(Hashtable in_options)
            {
                options = in_options;
                m_actions = UInt64.Parse((string)options["agent-actions"]);
                m_horizon = UInt64.Parse((string)options["agent-horizon"]);
                m_obs_bits = int.Parse((string)options["observation-bits"]);
                m_rew_bits = int.Parse((string)options["reward-bits"]);
                // calculate the number of bits needed to represent the action
                for ( UInt64 i = 1, c = 1; i < m_actions; i *= 2, c++) 
                {
                    m_actions_bits = (int)c;
                }
                ct_depth = UInt64.Parse((string)options["ct-depth"]);
                m_ct = new ContextTree(ct_depth);
                m_self_model = null; // new ContextTree(ct_depth);
                spc = new SearchSpace();
                if (m_use_self_model)
                {
                    m_self_model = new ContextTree(ct_depth);
                }
                reset();
          }


	    // current age of the agent in cycles
        public UInt64 age() 
        {
            return m_time_cycle; 
        }

        public bool useSelfModel()
        {
            return m_use_self_model;
        }
	    // the total accumulated reward across an agents lifespan
        public reward_t reward()
        {
            return m_total_reward;
        }
	    // the average reward received by the agent at each time step
        public reward_t averageReward()
        {
            return age() > 0 ? reward() / (double)(age()) : 0.0;
        }
	    // maximum reward in a single time instant
        public reward_t maxReward()
        {
            return (double)((1 << m_rew_bits) - 1);
        }
	    // minimum reward in a single time instant
        public reward_t minReward()
        {
            return 0.0;
        }
	    // number of distinct actions
        public UInt64 numActions()
        {
            return m_actions;
        }

        public UInt64 preceptBits()
        {
            return (UInt64) (m_obs_bits + m_rew_bits);
        }
	    // the length of the stored history for an agent
	    public UInt64 historySize() 
        {
            return m_ct.historySize();
        }

	    // length of the search horizon used by the agent
        public UInt64 horizon()
        {
            return m_horizon;
        }

        /* convert a list of symbols to an action, false on failure */
        bool symsToAction( symbol_list_t symlist, action_t action)  
        {

            action = 0;

            //symbol_list_t::const_reverse_iterator it = symlist.rbegin();
            //for (UInt64 c = 0; it != symlist.rend(); ++it, c++) {
            //    if (*it == On) action |= (1 << c);
            //}
            UInt16 c = 0;
            foreach (bool bit in  symlist.reverseIterator())
            {
                if (bit == true) action = action | ((UInt64)1 << c);
                c++;
            }

            return isActionOk(action);
        }


        // generate an action uniformly at random
        public action_t genRandomAction()
        {
            Random r = new Random();
            return (UInt64) r.Next((int) m_actions);
        }

        /* select an action uniformly at random */
        public action_t selectRandomAction(Random rng)
        {

            return (UInt64)rng.Next((int)m_actions);
        }

	    // generate an action distributed according
	    // to our history statistics
        public action_t genAction(Random rng)
        {
             // TODONE: implement
            symbol_list_t syms = new symbol_list_t (0); //(UInt64)m_actions_bits);
            UInt64 action =0;

            // use rejection sampling to pick an action according
            // to our historical distribution
            do
            {
                m_self_model.genRandomSymbols(rng, syms,(UInt64 ) m_actions_bits);
            } while (!symsToAction(syms, action));

            return action;
        }

	    // generate a percept distributed according
	    // to our history statistics
        public void genPercept(Random rng,symbol_list_t symlist)
        {
            m_ct.genRandomSymbols(rng, symlist, (UInt64)m_obs_bits + (UInt64)m_rew_bits);
            //return 0; // TODONE: implement
        }

	    // generate a percept distributed to our history statistics, and
	    // update our mixture environment model with it
        /* generate a percept distributed to our history statistics, and
        update our internal agent state. this is more efficient than calling
        genPercept and modelUpdate separately. */
        public void genPerceptAndUpdate(Random rng, symbol_list_t precept)
        {
            m_ct.genRandomSymbolsAndUpdate(rng, precept,(UInt64)( m_obs_bits + m_rew_bits));
            nonCTModelUpdate(precept);

            //return 0; // TODONE: implement
        }

	    // update the internal agent's model of the world
	    // due to receiving a percept or performing an action
	    public void modelUpdate(UInt64 observation, UInt64 reward)
        {
            // Update internal model
            symbol_list_t percept = new symbol_list_t(0); //(UInt64)m_obs_bits + (UInt64)m_rew_bits);
            encodePercept(percept, observation, reward);

            m_ct.update(percept);
            m_ct.updateHistory(percept);


            // Update other properties
            //m_total_reward += reward;
            //m_last_update_percept = true;
            nonCTModelUpdate(percept);
        }

        /* update the non-context tree part of an internal agent after receiving a percept */
        public void nonCTModelUpdate(symbol_list_t percept)
        {
            if (m_use_self_model) m_self_model.updateHistory(percept);
            m_hash = hashAfterSymbols(percept);
            m_total_reward += rewardFromPercept(percept);
            m_last_update_percept = true;
        }



        public void modelUpdate(action_t action)
        {
            if (!isActionOk(action)) return; // should be assert
            if(!m_last_update_percept == true) return; // should be assert

            // Update internal model
            symbol_list_t action_syms = new symbol_list_t(0); //(UInt64) m_actions_bits);
            encodeAction(action_syms, action);

            m_ct.update(action_syms);
            m_ct.updateHistory(action_syms);
            if (m_use_self_model) m_self_model.update(action_syms);
            m_hash = hashAfterSymbols(action_syms);

            m_time_cycle++;
            m_last_update_percept = false;
        }

	    // revert the agent's internal model of the world
	    // to that of a previous time cycle, false on failure
	    public bool modelRevert( ModelUndo mu)
        {
            // return false; // TODONE: implement
            // assert(m_ct->historySize() > mu.historySize());
           // assert(!m_use_self_model || m_self_model->historySize() > mu.historySize());

            if (m_time_cycle < mu.age()) return false;

            // agent properties must be reverted before context update,
            // since the predicates that depend on the context may
            // depend on them
            m_time_cycle = mu.age();
            m_hash = mu.hash();
            m_total_reward = mu.reward();
            m_last_update_percept = mu.lastUpdatePercept();

            // revert the context tree and history back to it's previous state

            if (mu.lastUpdatePercept())
            { // if we are undoing an action
                m_ct.revertHistory(mu.historySize());
                if (m_use_self_model)
                {
                    Int64 end_size = (Int64) m_self_model.historySize();
                    for (Int64 i = 0; i < (Int64)end_size - (Int64)mu.historySize(); i++)
                    {
                        m_self_model.revert();
                    }
                }
            }
            else
            { 
                // if we are undoing an observation / reward
                Int64 end_size = (Int64)m_ct.historySize();
                Int64 percept_bits = (Int64)(m_obs_bits + m_rew_bits);
                Int64 lim = (Int64) end_size - (Int64) mu.historySize();
                for (Int64 i = 0; i <(Int64) end_size - (Int64) mu.historySize(); i++)
                {
                   //ORIGINAL ::  m_ct.revert(percept_bits - i - 1);
                    Int64 offset = percept_bits - i - 1;
                    m_ct.revert();
                    for (Int64 ix = 0; ix <(Int64) m_ct.size(); ix++)
                    {
                        if (ix != offset) m_ct.m_history.pop_back();
                    }
                }
                if (m_use_self_model)
                {
                    m_self_model.revertHistory(mu.historySize());
                }
            }

            //assert(!m_use_self_model || m_self_model.historySize() == m_ct.historySize());

            return true;
        }

	    // resets the agent
	    public void reset()
        {
            m_ct.clear();
            if (m_use_self_model) m_self_model.clear ();

	        m_time_cycle = 0;
	        m_total_reward = 0.0;
            m_last_update_percept = false;
            m_hash = 5381 << 32;
        }

	    // probability of selecting an action according to the
	    // agent's internal model of it's own behaviour
        public double getPredictedActionProb(action_t action)
        {
            //return 0; // TODONE: implement
 
            // actions are equally likely if no internal model is used
            if (!m_use_self_model) return 1.0 / (double)(m_actions);

            // compute normalisation term, since some
            // actions may be illegal
            double tot = 0.0;
            symbol_list_t symlist = new symbol_list_t (0);//(UInt64)m_actions_bits);

            for (UInt64  a=0; a < m_actions; a++) 
            {
                encodeAction(symlist,a );
                tot += m_self_model.predict(symlist);
                }

            //assert(tot != 0.0);
            encodeAction(symlist,action );
            return m_self_model.predict(symlist) / tot;
       }

	    // get the agent's probability of receiving a particular percept
        //public double perceptProbability(UInt64 observation, UInt64 reward)
        public double perceptProbability( symbol_list_t percept)
        {
            //return 0; // TODONE: implement
           // assert(percept.size() == m_obs_bits_c + m_rew_bits_c);
            return m_ct.predict(percept);
        }

        public UInt64 hash()
        {
            return m_hash;
        }

	    // action sanity check
        public bool isActionOk(action_t action)
        {
            return action < m_actions;
        }

	    // reward sanity check
        public  bool isRewardOk(double reward)
        {
            return reward >= minReward() && reward <= maxReward();
        }

	    // encoding/decoding actions and percepts to/from symbol lists
        void encodeAction(symbol_list_t symlist, action_t action)
        {
            symlist.clear();

            encode(symlist, (int) action, m_actions_bits);
        }

        public void encodePercept(symbol_list_t symlist, UInt64 observation, UInt64 reward)
        {
            symlist.clear();

            encode(symlist, (int) observation, m_obs_bits);
            encode(symlist, (int) reward, m_rew_bits);
        }

        UInt64 decodeAction(symbol_list_t symlist)
        {
	        return (UInt64) decode(symlist, (int) m_actions_bits);
        }

        public UInt64 decodeReward(symbol_list_t symlist)
        {
            return (UInt64) decode(symlist, (int) m_rew_bits);
        }

        /* interprets a list of symbols as a reward */
        public reward_t  rewardFromPercept( symbol_list_t percept)  
        {

            // assert(percept.size() == m_obs_bits_c + m_rew_bits_c);

            // symbol_list_t::const_reverse_iterator it = percept.rbegin();
            //int it = 0;
            IEnumerator it = percept.reverseIterator().GetEnumerator ();
            if (m_base2_reward_encoding) 
                { // base2 reward encoding

                    int r = 0;
                    for ( int c = 0; c < m_rew_bits; it.MoveNext()) 
                        {
                            //assert(it != percept.rend());
                            if ((bool)it.Current== true) r |= (1 << c);
                            c++;
                        }
                    return (double)(r);
                }

            // assume the reward is the number of on bits
            double reward = 0.0;
            it.MoveNext();
            for ( int c = 0 ; c < m_rew_bits; it.MoveNext()) {
                //assert(it != percept.rend());
                if ((bool)it.Current == true)
                {
                    reward += 1.0;
                }
                c++;
            }

            return reward;
        }


        // Decodes the value encoded on the end of a list of symbols
         int decode( symbol_list_t symlist,  int bits) {
	        //assert(bits <= symlist.size());

	         int value = 0;
             //IEnumerator it = symlist.reverseIterator().GetEnumerator();

	        //symbol_list_t::const_reverse_iterator it = symlist.rbegin();
	        //symbol_list_t::const_reverse_iterator end = it + bits;
	        //for( ; it != end; ++it) {
	        //	value = (*it ? 1 : 0) + 2 * value;
	        //}
             int it = 0;
             int end = it +bits;
             for (; it != end; ++it)
             {
                 value = (symlist.bits[it] ? 1 : 0) + 2 * value;
             }

	        return value;
        }


        // Encodes a value onto the end of a symbol list using "bits" symbols
         public void encode(symbol_list_t symlist, int value, int  bits)
         {
	        for ( int i = 0; i < bits; i++, value /= 2) {
		        bool sym = ((value & 1) !=0);
		        symlist.push_back(sym);
	        }
        }

        /* computes the resultant history hash after processing a single symbol */
        hash_t hashAfterSymbol(symbol_t sym, hash_t hash)  {

           UInt64 c = (sym == true) ? '1' : '0';

           // update with a single iteration of the SDBM hash
           hash_t low = (hash << 32) >> 32;
           low = c + (low << 6) + (low << 16) - low;

           // update with a single iteration of the DJB2 hash
           hash_t high = hash >> 32;
           high = ((high << 5) + high) + c;

           // combine
           return (high << 32) | low;
        }


        /* computes the resultant history hash after processing a set of symbols */
        hash_t hashAfterSymbols(symbol_list_t new_syms)
        {

            hash_t rval = m_hash;

            // update the hash of the history
            //symbol_list_t::const_iterator it = new_syms.begin();
            int it = 0;
            for (; it != new_syms.bits.Length ; ++it) {
                rval = hashAfterSymbol(new_syms.bits[it], rval);
            }

            return rval;
        }

        /* hash of history if we were to make a particular action */
        public hash_t hashAfterAction(action_t action)
        {

            //assert(isActionOk(action));

            symbol_list_t action_syms = new symbol_list_t (0);//(UInt64)m_actions_bits);
            encodeAction(action_syms,action );

            return hashAfterSymbols(action_syms);
        }


    }


    // used to store sufficient information to revert an agent
    // to a copy of itself from a previous time cycle
    class ModelUndo {

           public UInt64 m_age;
           public hash_t m_hash;
           public reward_t m_reward;
           public UInt64 m_history_size;
           public bool m_last_update_percept;



            // construct a save point
            public ModelUndo(Agent agent)
            {

                m_age = agent.age();
                m_hash = agent.hash();
                m_reward = agent.reward();
                m_history_size = agent.historySize();
                m_last_update_percept = agent.m_last_update_percept;
            }

            // saved state age accessor
            public UInt64 age()  { return m_age; }

            // saved state reward accessor
            public reward_t reward()  { return m_reward; }

            public hash_t hash() { return m_hash; }

            // saved state history size accessor
             public UInt64 historySize()  { return m_history_size; }

            public bool lastUpdate()  { return m_last_update_percept; }
            public bool lastUpdatePercept() { return m_last_update_percept; }


    }

}
