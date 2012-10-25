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

    // stores symbol occurrence counts
//typedef unsigned int count_t;

// holds context weights
//typedef double weight_t;

// stores the agent's history in terms of primitive symbols
//typedef std::deque<symbol_t> history_t;

    class CTNode
    {

 
        public double m_log_prob_est;      // log KT estimated probability
        public double m_log_prob_weighted; // log weighted block probability

        // one slot for each symbol
        public UInt64 [] m_count;  // a,b in CTW literature
        public CTNode [] m_child;

        public CTNode ()
          {
      	        m_log_prob_est=0.0;
	        m_log_prob_weighted=0.0;

              m_count = new UInt64[2];
              m_child = new CTNode[2];
              m_count[0] = 0;
              m_count[1] = 0;
              m_child[0] = null;
              m_child[1] = null;

          }

        // log weighted blocked probability
        /* logarithmic weighted probability estimate accessor */
        public double logProbWeighted() { return m_log_prob_weighted; }

        // log KT estimated probability
        /* Krichevski-Trofimov estimated log probability accessor */
        public double logProbEstimated() { return m_log_prob_est; }

        // the number of times this context has been visited
        public UInt64 visits() { return m_count[0] + m_count[1]; }

        // child corresponding to a particular symbol
        public CTNode child(bool sym)  
        { 
           return m_child[(sym ? 1:0)]; 
        }

        // number of descendants
        public UInt64 size() 
        {
            UInt64 rval = 1;
            rval += child(false)!=null ? child(false).size() : 0;
            rval += child(true)!=null  ? child(true).size()  : 0;
            return rval;

        }

        // compute the logarithm of the KT-estimator update multiplier
        public double logKTMul(bool sym)
        {
            //return 0; // TODONE: implement in predict.cpp
            double kt_mul_numer = (double)(m_count[(sym? 1:0)]) + 0.5;
            double kt_mul_denom = (double)(visits() + 1);

            return Math.Log(kt_mul_numer / kt_mul_denom);
        }

    }

    class history_t
    {
        // stores the agent's history in terms of primitive symbols
        //typedef std::deque<symbol_t> history_t;
        public ArrayList mem = new ArrayList();

        public void clear()
        {
           mem.Clear();
        }
        public UInt64 size() { return (UInt64)mem.Count; }
        // deQueue on the back acts like a stack
        public void push_back(bool b)
        {
            // TODO
            mem.Add(b);
        }
        public void pop_back()
        {
          if (mem.Count > 0) mem.RemoveAt(mem.Count - 1);
        }
        public bool back()
        {
            return (bool) mem[mem.Count-1]; // TODO
        }
    }

    // a representation of a context and a list of symbols
    class symbol_list_t
    {
        public BitArray bits;
        
 
        public symbol_list_t(UInt64 N) 
        {
            bits= new BitArray((int)N);
            
        }
        public UInt64 size()
        { 
            return (UInt64) bits.Length ;
        }
        public void clear ()
        {
            bits.SetAll(false);
        }
        public void push_back(symbol_t b)
        {
            bits.Length = (bits.Count + 1);
            bits.Set(bits.Length-1, b);
        }
        public int ibits(int n)
        {
            return bits[n] ? 1 : 0;
        }

        public int ibits(UInt64 n)
        {
            return bits[(int) n] ? 1 : 0;
        }

        public void reserve(int n)
        {
            if (bits.Length < (int)n) {bits.Length = (int)n; }

        }

        public System.Collections.IEnumerator GetEnumerator()
        {
            for (int i = 0; i < bits.Count; i++)
            {
                yield return bits[i];
            }
        }
        public IEnumerable<bool> reverseIterator()
        {
            for (int i = bits.Count-1; i >=0; i--)
            {
                yield return bits[i];
            }
        }
    }

    class ContextTree
    {
        public history_t m_history; // the agents history
        public CTNode m_root;      // the root node of the context tree
        public UInt64 m_depth;      // the maximum depth of the context tree
        const double log_point_five = -0.69314718055994530941723212145818; //Math.Log(0.5);

        public ContextTree(UInt64 depth)
        {
            	m_root=new CTNode();
                m_depth = depth;
                m_history = new history_t();
        }

        // clear the entire context tree
        public void clear() 
        {
	        m_history.clear();

	        m_root = new CTNode();
        }

        /* reports the most frequently occuring symbol */
        public bool mostFrequentSym()
        {
            return m_root.m_count[1] > m_root.m_count[0] ? true : false;
        }

        /* the depth of the context tree */
        public UInt64 depth()
        {
            return m_depth;
        }

        /* the size of the stored history */
        public UInt64 historySize()
        {
            return m_history.size();
        }

        /* number of nodes in the context tree */
        public UInt64 size()
        {
            return m_root.size();
        }

        // the logarithm of the block probability of the whole sequence
        public double logBlockProbability()
        {
            return m_root.logProbWeighted();
        }


        // get the n'th most recent history symbol, NULL if doesn't exist
        public bool nthHistorySymbol(UInt64 n)
        {
            return n < (UInt64)m_history.mem.Count ? (bool)m_history.mem[(int)n] : false;
        }

        /* create (if necessary) all of the nodes in the current context */
        void createNodesInCurrentContext( symbol_list_t context) 
        {

            CTNode ctn = m_root;

            for (UInt64 i = 0; i < context.size(); i++) {
                // scan context and make up new nodes as we go along 
                // and insert in tree as necessary
                int lp = context.ibits((int) i);
                CTNode nxt_ctn =ctn.m_child[lp];
                if (nxt_ctn == null)
                {
                    //void *p = m_ctnode_pool.malloc();
                    //assert(p != NULL);  // TODO: make more robust
                    CTNode p = new CTNode ();
                    ctn.m_child[lp]=p;
                    nxt_ctn = p;
                }
                ctn = nxt_ctn;
            }
        }



        /* updates the context tree with a single symbol */
        void update(symbol_t sym) 
        {
	    // TODONE: implement

            // compute the current context
            symbol_list_t context = new symbol_list_t(0); //m_depth);
                //context.reserve((int) m_depth);
                getContext(context);
            // if we have not seen enough context, append the symbol
            // to the history buffer and skip updating the context tree
                if (context.size() < m_depth)
                {
                    m_history.push_back(sym);
                    return;
                }
            // 1. create new nodes in the context tree (if necessary)

                createNodesInCurrentContext(context);
            // 2. walk down the tree to the relevant leaf context, saving the path as we go
                Stack path = new Stack();
                path.Push(m_root); // add the empty context

            // add the path to the leaf nodes
                CTNode ctn = m_root;
                for (UInt64 i = 0; i < context.size(); i++)
                {
                    ctn = ctn.m_child[context.ibits((int)i)];
                    path.Push(ctn);
                }
            // 3. update the probability estimates from the leaf node back up to the root

               for (; path.Count!=0; path.Pop()) {

                    CTNode n = (CTNode)path.Peek(); // .Top();

                    // update the KT estimate and counts
                    double log_kt_mul = n.logKTMul(sym);
                    n.m_log_prob_est += log_kt_mul;
                   n.m_count[(sym ? 1 : 0 )]++;

                    // update the weighted probabilities
                    if (path.Count == (int)m_depth + 1) 
                    {
                        n.m_log_prob_weighted = n.logProbEstimated();
                    } 
                    else 
                    {
                        // computes P_w = log{0.5 * [P_kt + P_w0*P_w1]}
                        double log_prob_on  = n.child( true)!=null  ? n.child(true).logProbWeighted() : 0.0;
                        double log_prob_off = n.child(false)!=null ? n.child(false).logProbWeighted() : 0.0;
                        double log_one_plus_exp = log_prob_off + log_prob_on - n.logProbEstimated();

                        // NOTE: no need to compute the log(1+e^x) if x is large, plus it avoids overflows
                        if (log_one_plus_exp < 100.0) log_one_plus_exp = Math.Log(1.0 + Math.Exp(log_one_plus_exp));

                        n.m_log_prob_weighted = log_point_five + n.logProbEstimated() + log_one_plus_exp;
                    }
                }



            // 4. save the new symbol to the context buffer
             m_history.push_back(sym);

        }


        public void update(symbol_list_t symlist) 
        {
        // TODONE: implement
           int it = 0;
           for (; it != symlist.bits.Count; ++it)
           {
               update(symlist.bits[it]);
           }
        }

        void getContext(symbol_list_t context)
        {
           // if (!m_context_functor.empty())
           // {
           //     m_context_functor(context);
           //     return;
           // }

            context.clear();

           // history_t::const_reverse_iterator ri = m_history.rbegin();
            int ri = m_history.mem.Count-1;
            for (UInt64 c = 0; ri>=0 && c < m_depth; --ri, c++)
            {
                context.push_back((bool)m_history.mem[(int)ri]);
            }

        }


        // updates the history statistics, without touching the context tree
        public void updateHistory(symbol_list_t symlist)
        {

        for (UInt64 i=0; i < symlist.size(); i++) 
            {
                m_history.push_back( symlist.bits[(int)i] );
            }
        }

/* removes the most recently observed symbol from the context tree */
        /*
void revert(UInt64  offset) {

    m_cts[offset].revert();
    for (size_t i=0; i < m_cts.size(); i++) {
        if (i != offset) m_cts[i].m_history.pop_back();
    }
}*/
        // removes the most recently observed symbol from the context tree
        public void revert() 
        {
	        // TODONE: implement
                if (m_history.size() == 0) return;

            // 1. remove the most recent symbol from the context buffer
                symbol_t sym = m_history.back();
                m_history.pop_back();
            // compute the current context
                symbol_list_t context = new symbol_list_t (0);//m_depth); 
               // context.reserve((int)m_depth); 
                getContext(context);

            // no need to undo a context tree update if there was
            // not enough context to begin with
                if (context.size() < m_depth) return;
            // 2. determine the path to the leaf nodes
                Stack path = new Stack();
                path.Push(m_root);
            // add the path to the leaf nodes
                CTNode ctn = m_root;
                for (UInt64 i = 0; i < context.size() && ctn !=null ; i++)
                {
                    ctn = ctn.m_child[context.ibits(i)];
                    path.Push(ctn);
                }

        // 3. update the probability estimates from the leaf node back up to the root,
        //    deleting any superfluous nodes as we go
            for (; path.Count !=0; path.Pop()) 
            {

                ctn = (CTNode) path.Peek(); //top();
                if (ctn == null) break;
                // undo the previous KT estimate update
                ctn.m_count[sym? 1:0 ]--;
                double log_kt_mul = ctn.logKTMul(sym);
                ctn.m_log_prob_est -= log_kt_mul;

                // reclaim memory for any children nodes that now have seen no data
                for (UInt64 i = 0; i < 2; i++) 
                {
                   // bool sym = symbols[i];
                    bool my_sym = (i == 1);
                    if (ctn.m_child[my_sym ? 1 : 0] != null && ctn.m_child[my_sym ? 1 : 0].visits() == 0)
                    {
                        //m_ctnode_pool.free(ctn.m_child[sym]);
                        ctn.m_child[my_sym ? 1 : 0] = null;
                    }
                }

          // update the weighted probabilities
                if (path.Count == (int)m_depth + 1) 
                {
                    ctn.m_log_prob_weighted = ctn.logProbEstimated();
                } 
                else 
                {
                    // computes P_w = log{0.5 * [P_kt + P_w0*P_w1]}
                    double log_prob_on  = ctn.child(true)!=null  ? ctn.child(true).logProbWeighted() : 0.0;
                    double log_prob_off = ctn.child(false)!=null ? ctn.child(false).logProbWeighted() : 0.0;
                    double log_one_plus_exp = log_prob_off + log_prob_on - ctn.logProbEstimated();

                    // NOTE: no need to compute the log(1+e^x) if x is large, plus it avoids overflows
                    if (log_one_plus_exp < 100.0) log_one_plus_exp = Math.Log(1.0 + Math.Exp(log_one_plus_exp));

                    ctn.m_log_prob_weighted = log_point_five + ctn.logProbEstimated() + log_one_plus_exp;
                }
            }


        }


        /* gives the estimated probability of observing a particular symbol */
        public double predict(bool sym)
        {

            // if we haven't sufficient context to make an informed
            // prediction then guess uniformly randomly
            if (m_history.size() + 1 <= m_depth) return 0.5;

            // prob(sym | history) = prob(sym and history) / prob(history)
            double log_prob_history = m_root.logProbWeighted();
            update(sym);
            double log_prob_sym_and_history = m_root.logProbWeighted();
            revert();
            double df = log_prob_sym_and_history - log_prob_history;
            return Math.Exp(df);
        }

        /* gives the estimated probability of observing a particular sequence */
        public double predict(symbol_list_t symlist) 
        {

            // if we haven't enough context to make an informed
            // prediction then guess uniformly randomly
            if (m_history.size() + symlist.size() <= m_depth) 
            {
                double exp = -(double)(symlist.size());
                return Math.Pow(2.0, exp);
            }

            // prob(sym1 ^ sym2 ^ ... | history) = prob(sym1 ^ sym2 ^ ... and history) / prob(history)
            double log_prob_history = logBlockProbability();
            update(symlist);
            double log_prob_syms_and_history = logBlockProbability();

            int it = 0;
            for (; it != symlist.bits.Count; ++it) 
            {
                revert();
            }

            return Math.Exp(log_prob_syms_and_history - log_prob_history);
        }



        // shrinks the history down to a former size
        public void revertHistory(UInt64 newsize)
        {

            //assert(newsize <= m_history.size());
            while (m_history.size() > newsize) m_history.pop_back();
        }



        // generate a specified number of random symbols
        // distributed according to the context tree statistics
        public void genRandomSymbols(Random rng,symbol_list_t symbols, UInt64 bits) 
        {

            //Random rng = new Random();
            genRandomSymbolsAndUpdate(rng,symbols, bits);

	        // restore the context tree to it's original state
	        for (UInt64 i=0; i < bits; i++) revert();
        }


        // generate a specified number of random symbols distributed according to
        // the context tree statistics and update the context tree with the newly
        // generated bits
        public void genRandomSymbolsAndUpdate(Random rng, symbol_list_t symbols, UInt64 bits) 
        {
	        // TODONE: implement
            symbols.clear();
            //Random rng = new Random();
            for (UInt64 i = 0; i < bits; i++)
            {
                // flip a biased coin for each bit
                double prediction = predict(false);
                bool rand_sym = rng.NextDouble() < prediction ? false : true;
                symbols.push_back(rand_sym);
                update(rand_sym); // TODO: optimise this loop
            }

        }


 
    }
}
 