using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CAMeRAVUEmotion
{
    // This code was taken from Silicon Coppelia,
    // developed within the Center for Advanced Media Research Amsterdam 
    // at the VU University Amsterdam (CAMeRA@VU) 
    // and written by Matthijs Aart Pontier and Ghazanfar Farooq Siddiqui. 
    // More information and publications can be found here:
    // http://camera-vu.nl/matthijs/
    // http://www.linkedin.com/profile/view?id=19933074 
    // http://www.few.vu.nl/~mpr210/
    // http://www.few.vu.nl/~mpr210/DissertationMAPontier.pdf
    // http://camera-vu.nl/matthijs/IAT-2009_Coppelia.pdf
    //Hoorn, J.F., Pontier, M.A., & Siddiqui, G.F., (2011).
    //Coppélius’ Concoction: Similarity and Complementarity
    //Among Three Affect-related Agent Models. Cognitive
    //Systems Research Journal, in press.

    /// <summary> 
    /// Agents are AI participants in the simulation, and their reactions are calculated through the Model class
    /// They can be given "personalities" through variables upon their creation, and can receive settings through their public functions.
    /// </summary>
    public class Agent
    {
        int _agentId = -1;
        public int AgentID
        {
            get
            {
                return _agentId;
            }
        }
        internal int ID
        {
            set
            {
                _agentId = value;
            }
            get
            {
                return _agentId;
            }
        }

        #region variables

        internal int queuedAction = -1, queuedTarget = -1, receivedAction = -1, receivedAgent = -1;

        public int ReceivedAction
        {
            get
            {
                return receivedAction;
            }
        }

        public int ReceivedAgent
        {
            get
            {
                return receivedAgent;
            }
        }

        #region weights

        //*******************************************************************//****************************************
        //Weights for positivity / negativity in calculating GEU's regarding features and actions, GAT's, and UI
        //*******************************************************************//****************************************
        public float wGEUpf = 1;
        public float wGEUnf = 1;
        public float wGATp = 1;
        public float wGATn = 1;
        public float wGEUpa = 1;
        public float wGEUna = 1;
        public float wUIp = 1;
        public float wUIn = 1;

        public float wGPATp = 1;
        public float wGPATn = 1;
        public float wGNATp = 1;
        public float wGNATn = 1;

        public float trade_off_gamma = 0.5f;
        //*******************************************************************
        //******* weight of good on similarity***************
        //*******************************************************************
        public float wsg = 0.3f;
        public float wsb = 0.2f;
        public float wsbea = 0.2f;
        public float wsu = 0.1f;
        public float wsr = 0.1f;
        public float wsunr = 0.1f;
        //*******************************************************************

        //******* weight of good on dissimilarity************
        //*******************************************************************
        public float wdsg = 0.2f;
        public float wdsb = 0.3f;
        public float wdsbea = 0.1f;
        public float wdsu = 0.2f;
        public float wdsr = 0.1f;
        public float wdsunr = 0.1f;
        //*******************************************************************
        //*********weights on relevance***************
        //*******************************************************************
        public float wrg = 0.25f;
        public float wrb = 0.20f;
        public float wrpos = 0.30f;
        public float wrneg = 0.25f;

        //*******************************************************************
        //********weights on irrelevance************
        //*******************************************************************
        public float wirrg = 0.20f;
        public float wirrb = 0.25f;
        public float wipos = 0.25f;
        public float wineg = 0.30f;

        //***************************************************
        //**********weights on positive valence*******
        //***************************************************
        public float wpvg = 0.4f;
        public float wpvb = 0.02f;
        public float wpvpos = 0.55f;
        public float wpvneg = 0.03f;

        //*******************************************************************
        //**weights on negative valence***************
        //*******************************************************************
        public float wnvg = 0.02f;
        public float wnvb = 0.4f;
        public float wnvpos = 0.03f;
        public float wnvneg = 0.55f;
        //*******************************************************************
        //**weights on Involvement**********
        //*******************************************************************
        public float wibe = 0.15f;
        public float wiu = 0.05f;
        public float wireal = 0.10f;
        public float wiunr = 0.05f;
        public float wiaid = 0.15f;
        public float wiobstacle = -0.10f;
        public float wipv = 0.55f;
        public float winv = -0.15f;
        public float wipvs = 0.12f;
        public float winvs = -0.15f;
        public float wipvd = -0.10f;
        public float winvd = 0.05f;
        public float wipvbe = 0.07f;
        public float winvbe = -0.10f;
        public float wipvu = 0.07f;
        public float winvu = -0.04f;
        public float wir = 0.15f;
        public float wiirr = -0.01f;
        public float wirs = 0.10f;
        public float wiirrs = 0.04f;
        public float wird = 0.03f;
        public float wiirrd = -0.02f;
        public float wirbe = 0.10f;
        public float wiirrbe = 0.03f;
        public float wiru = 0.03f;
        public float wiirru = 0.01f;
        //******************************************************
        //**weights on Distance
        //******************************************************
        public float wdbe = -0.05f;
        public float wdu = 0.15f;
        public float wdreal = 0.05f;
        public float wdunr = 0.10f;
        public float wdaid = -0.10f;
        public float wdobstacle = 0.25f;
        public float wdpv = -0.35f;
        public float wdnv = 0.4f;
        public float wdpvs = -0.15f;
        public float wdnvs = 0.20f;
        public float wdpvd = 0.08f;
        public float wdnvd = -0.05f;
        public float wdpvbe = 0.08f;
        public float wdnvbe = 0.22f;
        public float wdpvu = -0.05f;
        public float wdnvu = -0.04f;
        public float wdr = 0.15f;
        public float wdirr = 0.05f;
        public float wdrs = -0.08f;

        public float wdirrs = 0.05f;
        public float wdrd = 0.05f;
        public float wdirrd = 0.02f;
        public float wdrbe = -0.08f;
        public float wdirrbe = -0.05f;
        public float wdru = 0.10f;
        public float wdirru = 0.05f;
        //******************************************************
        //*****weight on expected satisfaction*********************
        //******************************************************
        public float wesidt = 0.8f;
        public float wesui = 0.2f;
        public float wesaeu = .1f;		//weight of expected utility on expected satisfaction action
        public float wesapos = .2f;		//weight of positivity action on expected satisfaction action
        public float wesaneg = .2f;		//weight of negativity action on expected satisfaction action
        public float wesmor = .5f;      //weight of morality action on expected satisfaction action

        //WEIGHT EMOTIONS///////////////

        public float beta_hope = 0.15f;
        public float beta_fear = 0.15f;
        public float beta_joy = 0.15f;
        public float beta_distress = 0.15f;
        public float beta_surprise = 0.1f;
        public float beta_anger = 0.15f;
        public float beta_guilt = 0.15f;

        //other weights

        internal float biasinv = 1;			//bias for desired involvement to show in behavior
        internal float biasdis = 1;			//bias for desired distance to show in behavior

        internal float w_likelihood_p = 1;			//weight of positive beliefs that states facilitate goal-states on likelihood of goal-state
        internal float w_likelihood_n = 1;			//weight of negative beliefs that states facilitate goal-states on likelihood of goal-state

        internal float mf_aid = 1;
        internal float mf_obst = 1;

        internal float mf_respbel = 0.25f;			//Modification Factor for belief someone is responsible for something
        internal float mf_blame = 0.6f;			//Modification Factor for belief someone is blameworthy / praiseworthy

        internal float fatalism = 0.5f;			//Fatalism (or pessimism) for calculating hope and fear

        internal float mf_joy = 1;			//Modification factor for joy (speed with which joy is updated)
        internal float mf_distress = 1;			//Modification factor for distress (speed with which distress is updated)

        internal float p_surprise = 0.7f;			//Persistency for surprise
        internal float mf_surprise = 0.25f;			//modification factor of surprise
        internal float decay_surprise = 0.95f;			//Decay of surprise

        internal float mf_anger = 1;			//Persistency for anger
        internal float decay_anger = 0.95f;

        internal float p_guilt = 0.25f;			//Persistency for guilt
        internal float decay_guilt = 0.95f;
        internal float thr_guilt = -0.2f;			// threshold for guilt

        internal float alpha_guilt_bc = 0.8f;			// belief change of guilt

        internal float p_att = 0.9f;			// Persistency for attention to feature

        internal float mf_bias_ethics = 0.1f;

        //EDITED alpha_bf
        internal float alpha_bf = 0.25f;

        internal float d_emotion = 0.2f;			//desired emotion

        internal float beta_ethics = 0.5f;
        internal float beta_aesth = 0.4f;
        internal float beta_aff = 0.4f;
        internal float beta_bel_state = 0.5f;
        internal float beta_bel_state_AC = 0.9f;

        internal float mf_agent_bel_state = 0.5f;
        internal float mf_agent_bel_state_AC = 0.1f;

        #endregion weights
        
        //ambitions
        internal Dictionary<int, float> AMBITIONS = new Dictionary<int, float>();
        /// <summary>
        /// Adds an Ambition to an Agent.
        /// </summary>
        /// <param name="state"></param>
        /// <param name="value"></param>
        public void AddAmbition(int state, float value)
        {
            AMBITIONS[state] = value;
        }

        //moral ambitions
        internal Dictionary<int, float> MORALAMBITIONS = new Dictionary<int, float>();
        /// <summary>
        /// Adds a Moral Ambition to an Agent.
        /// </summary>
        /// <param name="state"></param>
        /// <param name="value"></param>
        public void AddMoralAmbition(int moralprinciple, float value)
        {
            MORALAMBITIONS[moralprinciple] = value;
        }

        /// <summary>
        /// Gets an ambition from an Agent.
        /// </summary>
        /// <param name="state"></param>
        /// <returns>The Agent's Ambition for that state, or 0 if no ambition exists.</returns>
        public float GetAmbition(int state)
        {
            if (AMBITIONS.Keys.Contains(state))
            {
                return AMBITIONS[state];
            }
            else
                return 0;
        }

        /// <summary>
        /// Gets a moral ambition from an Agent.
        /// </summary>
        /// <param name="state"></param>
        /// <returns>The Agent's Ambition for that state, or 0 if no ambition exists.</returns>
        public float GetMoralAmbition(int moralprinciple)
        {
            if (MORALAMBITIONS.Keys.Contains(moralprinciple))
            {
                return MORALAMBITIONS[moralprinciple];
            }
            else
                return 0;
        }

        //beliefs about features
        internal List<FeatureStateBelief> featureBeliefs = new List<FeatureStateBelief>();
        /// <summary>
        /// Sets an Agent's belief that another Agent's feature facilitates a state.
        /// </summary>
        /// <param name="feature"></param>
        /// <param name="state"></param>
        /// <param name="targetAgent"></param>
        /// <param name="value"></param>
        /// 
        public void SetFeatureBelief(int feature, int state, int targetAgent,float value)
        {
            int index = -1;
            foreach (FeatureStateBelief fsb in featureBeliefs)
            {
                if (fsb._feature == feature && fsb._state == state && fsb._target == targetAgent )
                {
                    //replace this
                    index = featureBeliefs.IndexOf(fsb);
                }
            }

            if (index == -1)
                featureBeliefs.Add(new FeatureStateBelief(feature, state, targetAgent, value));
            else
            {
                FeatureStateBelief temp = featureBeliefs[index];
                temp._value = value;
                featureBeliefs[index] = temp;
            }
        }

        public void SetFeatureBelief(int feature, int state, int targetAgent)
        {
            SetFeatureBelief(feature,state,targetAgent ,0);
        }

        /// <summary>
        /// Gets an Agent's belief that another Agent's feature facilitates a state.
        /// </summary>
        /// <param name="feature"></param>
        /// <param name="state"></param>
        /// <param name="targetAgent"></param>
        /// <returns></returns>
        public float GetFeatureBelief(int feature, int state, int targetAgent )
        {
            foreach (FeatureStateBelief fsb in featureBeliefs)
            {
                if (fsb._feature == feature && fsb._state == state && fsb._target == targetAgent)
                {
                    //replace this
                    return fsb.Value;
                }
            }

            return 0;
        }

        internal List<StateStateBelief> stateFacilitatesStateBeliefs = new List<StateStateBelief>();
        /// <summary>
        /// Sets an Agent's belief that a state facilitates another state.
        /// </summary>
        /// <param name="state1"></param>
        /// <param name="state2"></param>
        /// <param name="value"></param>
        public void SetStateFacStateBelief(int state1, int state2, float value )
        {
            int index = -1;
            StateStateBelief temp;
            foreach (StateStateBelief ssb in stateFacilitatesStateBeliefs)
            {
                if (state1 == ssb._state1 && state2 == ssb._state2)
                {
                    index = stateFacilitatesStateBeliefs.IndexOf(ssb);
                    break;
                }
            }

            if (index != -1)
            {
                temp = stateFacilitatesStateBeliefs[index];
                temp._value = value;
                stateFacilitatesStateBeliefs[index] = temp;
            }
            else
            {
                temp = new StateStateBelief(state1, state2, value);
                stateFacilitatesStateBeliefs.Add(temp);
            }
        }
        public void SetStateFacStateBelief(int state1, int state2)
        {
           SetStateFacStateBelief( state1,  state2,  0);
        }
        /// <summary>
        /// Gets an Agent's belief that a state facilitates another state.
        /// </summary>
        /// <param name="state1"></param>
        /// <param name="state2"></param>
        /// <returns></returns>
        public float GetStateFacStateBelief(int state1, int state2 )
        {
            foreach (StateStateBelief ssb in stateFacilitatesStateBeliefs)
            {
                if (state1 == ssb._state1 && state2 == ssb._state2)
                {
                    return ssb._value;
                }
            }

            return 0;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="state1"></param>
        /// <param name="state2"></param>
        /// <param name="value"></param>
        public void AddStateFacStateBelief(int state1, int state2, float value)
        {
            stateFacilitatesStateBeliefs.Add(new StateStateBelief(state1, state2, value));
        }
        public void AddStateFacStateBelief(int state1, int state2 )
        {
            stateFacilitatesStateBeliefs.Add(new StateStateBelief(state1, state2, 0));
        }


        /// <summary>
        /// 
        /// </summary>
        /// <param name="ssb"></param>
        public void AddStateFacStateBelief(StateStateBelief ssb)
        {
            stateFacilitatesStateBeliefs.Add(ssb);
        }

        internal Dictionary<int, float> stateLikelihoodBelief = new Dictionary<int, float>();
        /// <summary>
        /// Returns the Agent's belief that a state is likely.
        /// </summary>
        /// <param name="state"></param>
        /// <returns></returns>
        public float GetStateLikelihood(int state)
        {
            if (stateLikelihoodBelief.Keys.Contains(state))
                return stateLikelihoodBelief[state];
            else
                return 0;
        }
        /// <summary>
        /// Sets the Agent's belief that a state is likely.
        /// </summary>
        /// <param name="state"></param>
        /// <param name="value"></param>
        public void SetStateLikelihood(int state, float value)
        {
            stateLikelihoodBelief[state] = value;
        }

        //beliefs about actions
        internal List<AgentAction> actionBeliefs = new List<AgentAction>();
        /// <summary>
        /// Sets the Agent's belief that an action facilitates a state
        /// </summary>
        /// <param name="action"></param>
        /// <param name="state"></param>
        /// <param name="value"></param>
        public void SetActionStateBelief(int action, int state, float value )
        {
            int index = -1;
            ActionStateBelief temp;
            foreach (ActionStateBelief asb in actionStateBeliefs)
            {
                if (action == asb._action && state == asb._state)
                {
                    index = actionStateBeliefs.IndexOf(asb);
                    break;
                }
            }

            if (index != -1)
            {
                temp = actionStateBeliefs[index];
                temp._value = value;
                actionStateBeliefs[index] = temp;
            }
            else
            {
                temp = new ActionStateBelief(action, state, value);
                actionStateBeliefs.Add(temp);
            }
        }
    public void SetActionStateBelief(int action, int state)
        {
           SetActionStateBelief( action,  state,  0);
        }
        /// <summary>
        /// Gets the Agent's belief that an action facilitates a state
        /// </summary>
        /// <param name="action"></param>
        /// <param name="state"></param>
        /// <returns></returns>
        public float GetActionStateBelief(int action, int state)
        {
            foreach (ActionStateBelief asb in actionStateBeliefs)
            {
                if (action == asb._action && state == asb._state)
                {
                    return asb._value;
                }
            }

            return 0;
        }

        /// <summary>
        /// Sets the Agent's belief that an action facilitates a moral principle
        /// </summary>
        /// <param name="action"></param>
        /// <param name="state"></param>
        /// <param name="value"></param>
        public void SetActionMoralPrincipleBelief(int action, int moralprinciple, float value)
        {
            int index = -1;
            ActionMoralPrincipleBelief temp;
            foreach (ActionMoralPrincipleBelief ampb in actionMoralPrincipleBeliefs)
            {
                if (action == ampb._action && moralprinciple == ampb._moralprinciple)
                {
                    index = actionMoralPrincipleBeliefs.IndexOf(ampb);
                    break;
                }
            }

            if (index != -1)
            {
                temp = actionMoralPrincipleBeliefs[index];
                temp._value = value;
                actionMoralPrincipleBeliefs[index] = temp;
            }
            else
            {
                temp = new ActionMoralPrincipleBelief(action, moralprinciple, value);
                actionMoralPrincipleBeliefs.Add(temp);
            }
        }
         public void SetActionMoralPrincipleBelief(int action, int moralprinciple)
         {
             SetActionMoralPrincipleBelief( action,  moralprinciple,0);
         }
       /// <summary>
        /// Gets the Agent's belief that an action facilitates a moral principle
        /// </summary>
        /// <param name="action"></param>
        /// <param name="state"></param>
        /// <returns></returns>
        public float GetActionMoralPrincipleBelief(int action, int moralprinciple)
        {
            foreach (ActionMoralPrincipleBelief ampb in actionMoralPrincipleBeliefs)
            {
                if (action == ampb._action && moralprinciple == ampb._moralprinciple)
                {
                    return ampb._value;
                }
            }

            return 0;
        }

        //general beliefs about features leading to emotions
        internal float[,] featureEmotionBeliefs =
        {
            //  HOPE    FEAR    JOY     DISTR.  SURPR.  ANGER   GUILT    
            {   0,      0,      0,      0,      0,      0,      0 },    //GOOD
            
            {   0,      0,      0,      0,      0,      0,      0 },    //BAD
            
            {   0,      0,      0,      0,      0,      0,      0 },    //BEAUTIFUL
            
            {   0,      0,      0,      0,      0,      0,      0 },    //UGLY
            
            {   0,      0,      0,      0,      0,      0,      0 },    //REALISTIC
            
            {   0,      0,      0,      0,      0,      0,      0 },    //UNREALISTIC
            
            {   0,      0,      0,      0,      0,      0,      0 },    //AID
            
            {   0,      0,      0,      0,      0,      0,      0 },    //OBSTACLE
        };
        /// <summary>
        /// Sets the Agent's belief that a feature leads to an emotion. See the AgentFeatures and AgentEmotions classes for their indices.
        /// </summary>
        /// <param name="feature"></param>
        /// <param name="emotion"></param>
        /// <param name="value"></param>
        public void SetFeatureEmotionBelief(int feature, int emotion, float value)
        {
            featureEmotionBeliefs[feature, emotion] = value;
        }
        /// <summary>
        /// Gets the Agent's belief that a feature leads to an emotion. See the AgentFeatures and AgentEmotions classes for their indices.
        /// </summary>
        /// <param name="feature"></param>
        /// <param name="emotion"></param>
        /// <returns></returns>
        public float GetFeatureEmotionBelief(int feature, int emotion)
        {
            return featureEmotionBeliefs[feature, emotion];
        }

        //possible actions
        List<AgentAction> ACTIONS = new List<AgentAction>();
        /// <summary>
        /// Adds an action to the Agent. Currently is not to be used directly.
        /// </summary>
        /// <param name="toAdd"></param>
        internal void AddAction(AgentAction toAdd)
        {
            ACTIONS.Add(toAdd);
        }
        /// <summary>
        /// Removes an action from the Agent. Currently is not to be used directly.
        /// </summary>
        /// <param name="toRemove"></param>
        internal void RemoveAction(AgentAction toRemove)
        {
            if (ACTIONS.Contains(toRemove))
                ACTIONS.Add(toRemove);
        }

        //const	float desired_emotion[EMOTIONS] = {
        internal AgentEmotions desired_emotion;// = new AgentEmotions();
        /// <summary>
        /// Sets the Agent's desired emotional level for a specific emotion.
        /// </summary>
        /// <param name="emotion"></param>
        /// <param name="value"></param>
        public void SetDesired(int emotion, float value)
        {
            desired_emotion[emotion] = value;
        }
        /// <summary>
        /// Sets the Agent's desired emotional level for all emotions.
        /// </summary>
        /// <param name="e"></param>
        public void SetDesired(AgentEmotions e)
        {
            desired_emotion = e;
        }
        /// <summary>
        /// Gets an Agent's desired emotional level for a specific emotion.
        /// </summary>
        /// <param name="emotion"></param>
        /// <returns></returns>
        public float GetDesired(int emotion)
        {
            return desired_emotion[emotion];
        }

        //emotions
        internal AgentEmotions EMOTIONS;// = new AgentEmotions();
        internal AgentEmotions OLD_EMOTIONS;// = new AgentEmotions();
        /// <summary>
        /// Gets an Agent's emotional level for a specific emotion.
        /// </summary>
        /// <param name="emotion"></param>
        /// <returns></returns>
        public float GetEmotion(int emotion)
        {
            return EMOTIONS[emotion];
        }
        internal float GetOldEmotion(int emotion)
        {
            return OLD_EMOTIONS[emotion];
        }
        /// <summary>
        /// Sets an Agent's desired emotional level for a specific emotion.
        /// </summary>
        /// <param name="emotion"></param>
        /// <param name="value"></param>
        public void SetEmotion(int emotion, float value)
        {
            EMOTIONS[emotion] = value;
        }
        /// <summary>
        /// Gets an Agent's desired emotional level for all emotions.
        /// </summary>
        /// <param name="e"></param>
        public void SetEmotions(AgentEmotions e)
        {
            EMOTIONS = e;
        }

        //action state influence beliefs
        List<ActionStateBelief> actionStateBeliefs = new List<ActionStateBelief>();

        //action moralprinciple influence beliefs
        List<ActionMoralPrincipleBelief> actionMoralPrincipleBeliefs = new List<ActionMoralPrincipleBelief>();

        //anger towards other agents
        internal Dictionary<int, float> anger = new Dictionary<int, float>();
        /// <summary>
        /// Sets the Agent's anger towards another Agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="value"></param>
        public void SetAnger(int AgentID, float value)
        {
            anger[AgentID] = value;
        }
        /// <summary>
        /// Gets the Agent's anger towards another Agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <returns></returns>
        public float GetAnger(int AgentID)
        {
            if (anger.Keys.Contains(AgentID))
                return anger[AgentID];
            else
                return 0;
        }

        internal Dictionary<int, AgentFeatures> designed_bias = new Dictionary<int, AgentFeatures>();
        /// <summary>
        /// Sets the Agents designed bias towards the features of another Agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="feature"></param>
        /// <param name="value"></param>
        public void SetDesignedBias(int AgentID, int feature, float value)
        {
            AgentFeatures tmp;
            if ( designed_bias.Keys.Contains( AgentID ) )
            {
                tmp = designed_bias[AgentID];
            }
            else
            {
                tmp = new AgentFeatures();
            }
           
            tmp[feature] = value;
            designed_bias[AgentID] = tmp;
        }
        /// <summary>
        /// Gets the Agents designed bias towards the features of another Agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="feature"></param>
        /// <returns></returns>
        public float GetDesignedBias(int AgentID, int feature)
        {
            AgentFeatures returnValue;
            if (designed_bias.TryGetValue(AgentID, out returnValue))
            {
                return returnValue[feature];
            }
            else
                return 0;
        }

        internal Dictionary<int, float> STATE_BELIEFS = new Dictionary<int, float>();
        internal Dictionary<int, float> STATE_BELIEVED = new Dictionary<int, float>();
        /// <summary>
        /// Sets the Agents belief in a state.
        /// </summary>
        /// <param name="state"></param>
        /// <param name="value"></param>
        public void SetStateBelief(int state, float value)
        {
            STATE_BELIEFS[state] = value;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="state"></param>
        /// <param name="value"></param>
        internal void SetStateBelieved(int state, float value)
        {
            STATE_BELIEVED[state] = value;
        }

        /// <summary>
        /// Gets the Agents belief in a state.
        /// </summary>
        /// <param name="state"></param>
        /// <returns></returns>
        public float GetStateBelief(int state)
        {
            if (STATE_BELIEFS.Keys.Contains(state))
                return STATE_BELIEFS[state];
            else
                return 0;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="state"></param>
        /// <returns></returns>
        public float GetStateBelieved(int state)
        {
            if (STATE_BELIEVED.Keys.Contains(state))
                return STATE_BELIEVED[state];
            else
                return 0;
        }

        internal List<AgentStateBelief> agentResponsibleBeliefs = new List<AgentStateBelief>();
        /// <summary>
        /// Sets the Agent's belief that another Agent is responsible for a state.
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="state"></param>
        /// <param name="value"></param>
        public void SetAgentResponsibleBelief(int agentID, int state, float value )
        {
            int index = -1;
            foreach (AgentStateBelief asb in agentResponsibleBeliefs)
            {
                if (asb._agentID == agentID && asb._state == state)
                {
                    index = agentResponsibleBeliefs.IndexOf(asb);
                    break;
                }
            }

            if (index != -1)
            {
                AgentStateBelief tmp = agentResponsibleBeliefs[index];
                tmp._value = value;
                agentResponsibleBeliefs[index] = tmp;
            }
            else
                agentResponsibleBeliefs.Add(new AgentStateBelief(agentID, state, value));
        }
       public void SetAgentResponsibleBelief(int agentID, int state)
        {
            SetAgentResponsibleBelief( agentID,  state,0);
        }
        /// <summary>
        /// Gets the Agent's belief that another Agent is responsible for a state.
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="state"></param>
        /// <returns></returns>
        public float GetAgentResponsibleBelief(int agentID, int state)
        {
            foreach (AgentStateBelief asb in agentResponsibleBeliefs)
            {
                if (asb._agentID == agentID && asb._state == state)
                {
                    return asb._value;
                }
            }

            return 0;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="state"></param>
        /// <param name="value"></param>
        public void AddAgentResponsibleBelief(int agentID, int state, float value)
        {
            agentResponsibleBeliefs.Add(new AgentStateBelief(agentID, state, value));
        }
        public void AddAgentResponsibleBelief(int agentID, int state)
        {
            agentResponsibleBeliefs.Add(new AgentStateBelief(agentID, state, 0));
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="asb"></param>
        public void AddAgentResponsibleBelief(AgentStateBelief asb)
        {
            agentResponsibleBeliefs.Add(asb);
        }

        //praiseworthiness of other agents
        internal Dictionary<int, float> praiseworthy = new Dictionary<int, float>();
        /// <summary>
        /// Sets an Agent's praise of another agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="value"></param>
        public void SetPraiseworthy(int AgentID, float value)
        {
            praiseworthy[AgentID] = value;
        }
        /// <summary>
        /// Gets an Agent's praise of another agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <returns></returns>
        public float GetPraiseworthy(int AgentID)
        {
            if (praiseworthy.Keys.Contains(AgentID))
                return praiseworthy[AgentID];
            else
                return 0;
        }

        internal Dictionary<int, AgentFeatures> perceivedFeatures = new Dictionary<int, AgentFeatures>();
        /// <summary>
        /// Sets the Agent's perceived features of another Agent. This is done automatically when an agent is set to perceive another Agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="feature"></param>
        /// <param name="value"></param>
        public void SetPerceivedFeature(int AgentID, int feature, float value)
        {
            AgentFeatures tmp = perceivedFeatures[AgentID];
            tmp[feature] = value;
            perceivedFeatures[AgentID] = tmp;
        }
        /// <summary>
        /// Gets the Agent's perceived features of another Agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="feature"></param>
        /// <returns></returns>
        public float GetPerceivedFeature(int AgentID, int feature)
        {
            if (!perceivedFeatures.Keys.Contains(AgentID))
            {
                perceivedFeatures.Add(AgentID, new AgentFeatures());
            }

            return perceivedFeatures[AgentID][feature];
        }
        /// <summary>
        /// Sets the Agent's perceived features of another Agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <returns></returns>
        public AgentFeatures GetPerceivedFeatures(int AgentID)
        {
            AgentFeatures returnValue;
            if (perceivedFeatures.TryGetValue(AgentID, out returnValue))
            {
                return returnValue;
            }
            else
                return new AgentFeatures();
        }
        /// <summary>
        /// Sets the Agent's perceived features of another Agent. This is done automatically when an agent is set to perceive another Agent.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="features"></param>
        public void SetPerceivedFeatures(int AgentID, AgentFeatures features )
        {
            perceivedFeatures[AgentID] = features;
        }

        //attentionlevels to features of other agents (access this with an agent id, and you receive a class containing floats with the value per feature at the appropriate index )
        internal Dictionary<int, AgentFeatures> featureAttention = new Dictionary<int, AgentFeatures>();
        /// <summary>
        /// Sets the Agent's attention level to another agent's features.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="feature"></param>
        /// <param name="value"></param>
        public void SetAttention(int AgentID, int feature, float value)
        {
            AgentFeatures tmp = featureAttention[AgentID];
            tmp[feature] = value;
            featureAttention[AgentID] = tmp;
        }
        /// <summary>
        /// Gets the Agent's attention level to another agent's features.
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="feature"></param>
        /// <returns></returns>
        public float GetAttention(int AgentID, int feature)
        {
            if (!featureAttention.Keys.Contains(AgentID))
            {
                featureAttention.Add(AgentID, new AgentFeatures());
            }

            return featureAttention[AgentID][feature];
        }

        //expected utility of agent's features towards states
        internal List<ExpectedUtility> expectedUtilityFeatures = new List<ExpectedUtility>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="state"></param>
        /// <param name="agentID"></param>
        /// <param name="feature"></param>
        /// <param name="value"></param>
        public void SetExpectedUtilityFeature(int state, int agentID, int feature, float value)
        {
            int index = -1;
            foreach (ExpectedUtility eu in expectedUtilityFeatures)
            {
                if (eu._agentID == agentID && eu._state == state && eu._feature == feature)
                {
                    //replace this
                    index = expectedUtilityFeatures.IndexOf(eu);
                }
            }

            if (index == -1)
                expectedUtilityFeatures.Add(new ExpectedUtility(agentID, state, feature, value));
            else
                expectedUtilityFeatures[index] = new ExpectedUtility(agentID, state, feature, value);
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="state"></param>
        /// <param name="agentID"></param>
        /// <param name="feature"></param>
        /// <returns></returns>
        public float GetExpectedUtilityFeature(int state, int agentID, int feature)
        {
            foreach (ExpectedUtility eu in expectedUtilityFeatures)
            {
                if (eu._agentID == agentID && eu._state == state && eu._feature == feature)
                {
                    //replace this
                    return eu.Value;
                }
            }

            return 0;
        }

        //expected utility of agent's features towards states
        internal List<ExpectedUtility> expectedUtilityActions = new List<ExpectedUtility>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="state"></param>
        /// <param name="agentID"></param>
        /// <param name="action"></param>
        /// <param name="value"></param>
        public void SetExpectedUtilityAction(int state, int agentID, int action, float value)
        {
            int index = -1;
            foreach (ExpectedUtility eu in expectedUtilityActions)
            {
                if (eu._agentID == agentID && eu._state == state && eu._feature == action)
                {
                    //replace this
                    index = expectedUtilityActions.IndexOf(eu);
                }
            }

            if (index == -1)
                expectedUtilityActions.Add(new ExpectedUtility(agentID, state, action, value));
            else
                expectedUtilityActions[index] = new ExpectedUtility(agentID, state, action, value);
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="state"></param>
        /// <param name="agentID"></param>
        /// <param name="action"></param>
        /// <returns></returns>
        public float GetExpectedUtilityAction(int state, int agentID, int action )
        {
            foreach (ExpectedUtility eu in expectedUtilityActions)
            {
                if (eu._agentID == agentID && eu._state == state && eu._feature == action)
                {
                    //replace this
                    return eu.Value;
                }
            }

            return 0;
        }

        //MORAL:
        //expected morality of agent's features towards states
        internal List<Morality> moralityActions = new List<Morality>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="moralprinciple"></param>
        /// <param name="agentID"></param>
        /// <param name="action"></param>
        /// <param name="value"></param>
        public void SetMoralityAction(int moralprinciple, int agentID, int action, float value)
        {
            int index = -1;
            foreach (Morality mu in moralityActions)
            {
                if (mu._agentID == agentID && mu._moralprinciple == moralprinciple && mu._feature == action)
                {
                    //replace this
                    index = moralityActions.IndexOf(mu);
                }
            }

            if (index == -1)
                moralityActions.Add(new Morality(agentID, moralprinciple, action, value));
            else
                moralityActions[index] = new Morality(agentID, moralprinciple, action, value);
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="moralprinciple"></param>
        /// <param name="agentID"></param>
        /// <param name="action"></param>
        /// <returns></returns>
        public float GetMoralityAction(int moralprinciple, int agentID, int action)
        {
            foreach (Morality mu in moralityActions)
            {
                if (mu._agentID == agentID && mu._moralprinciple == moralprinciple && mu._feature == action)
                {
                    //replace this
                    return mu.Value;
                }
            }

            return 0;
        }

        //general expected utility features
        internal Dictionary<int, AgentFeatures> generalExpectedUtilityFeature = new Dictionary<int, AgentFeatures>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="feature"></param>
        /// <param name="value"></param>
        public void SetGEUFeature(int agentID, int feature, float value)
        {
            AgentFeatures tmp;
            if (generalExpectedUtilityFeature.Keys.Contains(agentID))
            {
                tmp = generalExpectedUtilityFeature[agentID];
            }
            else
                tmp = new AgentFeatures();

            tmp[feature] = value;

            generalExpectedUtilityFeature[agentID] = tmp;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="feature"></param>
        /// <returns></returns>
        public float GetGEUFeature(int agentID, int feature)
        {
            if (!generalExpectedUtilityFeature.Keys.Contains(agentID))
                return 0;
            else return generalExpectedUtilityFeature[agentID][feature];
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="value"></param>
        public void SetGEUFeatures(int agentID, AgentFeatures value)
        {
            generalExpectedUtilityFeature[agentID] = value;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <returns></returns>
        public AgentFeatures GetGEUFeatures(int agentID)
        {
            if (generalExpectedUtilityFeature.Keys.Contains(agentID))
                return generalExpectedUtilityFeature[agentID];
            else
            {
                return new AgentFeatures();
            }
        }

        //TODO: What is this?
        internal Dictionary<int, AgentFeatures> Rel_features = new Dictionary<int, AgentFeatures>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="value"></param>
        public void SetRelFeatures(int agentID, AgentFeatures value)
        {
            Rel_features[agentID] = value;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="feature"></param>
        /// <param name="value"></param>
        public void SetRelFeature(int agentID, int feature, float value)
        {
            AgentFeatures tmp = new AgentFeatures();
            if (Rel_features.Keys.Contains(agentID))
                tmp = Rel_features[agentID];

            tmp[feature] = value;

            Rel_features[agentID] = tmp;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <returns></returns>
        public AgentFeatures GetRelFeatures(int agentID)
        {
            if (Rel_features.Keys.Contains(agentID))
                return Rel_features[agentID];
            else
            {
                return new AgentFeatures();
            }
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="feature"></param>
        /// <returns></returns>
        public float GetRelFeature(int agentID, int feature)
        {
            if (Rel_features.Keys.Contains(agentID))
                return Rel_features[agentID][feature];
            else
            {
                return 0;
            }
        }

        //general expected utility actions
        internal Dictionary<int, Dictionary<int, float>> generalExpectedUtilityActions = new Dictionary<int, Dictionary<int, float>>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="actionID"></param>
        /// <param name="value"></param>
        public void SetGEUAction(int agentID, int actionID, float value)
        {
            Dictionary<int, float> tmp = new Dictionary<int,float>();
            if (generalExpectedUtilityActions.Keys.Contains(agentID))
                //retrieve this dictionary
                tmp = generalExpectedUtilityActions[agentID];
            
            tmp[actionID] = value;

            generalExpectedUtilityActions[agentID] = tmp;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="actionID"></param>
        /// <returns></returns>
        public float GetGEUAction(int agentID, int actionID)
        {
            if (generalExpectedUtilityActions.Keys.Contains(agentID))
            {
                if (generalExpectedUtilityActions[agentID].Keys.Contains(actionID))
                    return generalExpectedUtilityActions[agentID][actionID];
                else
                    return 0;
            }
            else
                return 0;
        }

        //general morality actions
        internal Dictionary<int, Dictionary<int, float>> generalMoralityActions = new Dictionary<int, Dictionary<int, float>>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="actionID"></param>
        /// <param name="value"></param>
        public void SetGMoralityAction(int agentID, int actionID, float value)
        {
            Dictionary<int, float> tmp = new Dictionary<int, float>();
            if (generalMoralityActions.Keys.Contains(agentID))
                //retrieve this dictionary
                tmp = generalMoralityActions[agentID];

            tmp[actionID] = value;

            generalMoralityActions[agentID] = tmp;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="actionID"></param>
        /// <returns></returns>
        public float GetGMoralityAction(int agentID, int actionID)
        {
            if (generalMoralityActions.Keys.Contains(agentID))
            {
                if (generalMoralityActions[agentID].Keys.Contains(actionID))
                    return generalMoralityActions[agentID][actionID];
                else
                    return 0;
            }
            else
                return 0;
        }

        //Action Tendancy
        internal Dictionary<int, Dictionary<int, float>> actionTendancy = new Dictionary<int, Dictionary<int, float>>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="actionID"></param>
        /// <param name="value"></param>
        public void SetAT(int agentID, int actionID, float value)
        {
            Dictionary<int, float> tmp = new Dictionary<int, float>();
            if (actionTendancy.Keys.Contains(agentID))
                //retrieve this dictionary
                tmp = actionTendancy[agentID];

            tmp[actionID] = value;

            actionTendancy[agentID] = tmp;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="actionID"></param>
        /// <returns></returns>
        public float GetAT(int agentID, int actionID)
        {
            if (actionTendancy.Keys.Contains(agentID))
            {
                if (actionTendancy[agentID].Keys.Contains(actionID))
                    return actionTendancy[agentID][actionID];
                else
                    return 0;
            }
            else
                return 0;
        }

        internal Dictionary<int, float> GNAT = new Dictionary<int, float>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="value"></param>
        public void SetGNAT(int AgentID, float value)
        {
            GNAT[AgentID] = value;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="AgentID"></param>
        /// <returns></returns>
        public float GetGNAT(int AgentID)
        {
            if (GNAT.Keys.Contains(AgentID))
                return GNAT[AgentID];
            else
                return 0;
        }

        internal Dictionary<int, float> GPAT = new Dictionary<int, float>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="AgentID"></param>
        /// <param name="value"></param>
        public void SetGPAT(int AgentID, float value)
        {
            GPAT[AgentID] = value;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="AgentID"></param>
        /// <returns></returns>
        public float GetGPAT(int AgentID)
        {
            if (GPAT.Keys.Contains(AgentID))
                return GPAT[AgentID];
            else
                return 0;
        }

        //Exp.Sat.Actions
        internal Dictionary<int, Dictionary<int, float>> ESAT = new Dictionary<int, Dictionary<int, float>>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="actionID"></param>
        /// <param name="value"></param>
        public void SetExpectedSatisfaction(int agentID, int actionID, float value)
        {
            Dictionary<int, float> tmp = new Dictionary<int, float>();
            if (ESAT.Keys.Contains(agentID))
                //retrieve this dictionary
                tmp = ESAT[agentID];

            tmp[actionID] = value;

            ESAT[agentID] = tmp;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="actionID"></param>
        /// <returns></returns>
        public float GetExpectedSatisfaction(int agentID, int actionID)
        {
            if (ESAT.Keys.Contains(agentID))
            {
                if (ESAT[agentID].Keys.Contains(actionID))
                    return ESAT[agentID][actionID];
                else
                    return 0;
            }
            else
                return 0;
        }

        //features
        internal AgentFeatures FEATURES = new AgentFeatures();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="feature"></param>
        /// <returns></returns>
        public float GetFeature(int feature)
        {
            return FEATURES[feature];
        }

        public void SetFeature(int feature, float value)
        {
            FEATURES[feature] = value;
        }
        //activity means whether or not the agent updates within the simulation
        internal bool bActive = true;

        public float Mood
        {
            get
            {
                return mood;
            }
        }
        internal float mood = 0;

        //dynamic array of other agents that are perceived by this agent
        internal List<int> perceivedAgents = new List<int>();
        /// <summary>
        /// Adds an Agent to this Agent's perception list. Agents will only consider performing actions towards perceived agents.
        /// </summary>
        /// <param name="toPerceive"></param>
        public void AddAgent(int toPerceive)
        {
            perceivedAgents.Add(toPerceive);
            //Model.GetAgentByID(toPerceive).actionBroadcast += new ActionListener(PerceiveAction);
            CalculatePerceivedFeatures(toPerceive);
        }
        /// <summary>
        /// Removes an Agent from this Agent's perception list. Agents will only consider performing actions towards perceived agents.
        /// </summary>
        /// <param name="toForget"></param>
        public void RemoveAgent(int toForget)
        {
            //Model.GetAgentByID(toForget).actionBroadcast -= new ActionListener(PerceiveAction);
            perceivedAgents.Remove(toForget);
        }

        //general expected utility features
        internal Dictionary<int, AgentRelations> agentRelations = new Dictionary<int, AgentRelations>();
        /// <summary>
        /// Sets the relational values towards another Agent.
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="value"></param>
        public void SetRelations(int agentID, AgentRelations value)
        {
            agentRelations[agentID] = value;
        }
        /// <summary>
        ///  Sets the relational values towards another Agent.
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="relationType"></param>
        /// <param name="value"></param>
        public void SetRelation(int agentID, int relationType, float value)
        {
            AgentRelations tmp;
            if (agentRelations.Keys.Contains(agentID))
                tmp = agentRelations[agentID];
            else
                tmp = new AgentRelations();  
            
            tmp[relationType] = value;

            agentRelations[agentID] = tmp;
        }
        /// <summary>
        /// Gets the relational values towards another Agent.
        /// </summary>
        /// <param name="agentID"></param>
        /// <returns></returns>
        public AgentRelations GetRelations(int agentID)
        {
            if (agentRelations.Keys.Contains(agentID))
                return agentRelations[agentID];
            else
            {
                return new AgentRelations();
            }
        }
        /// <summary>
        /// Gets the relational values towards another Agent.
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="relationType"></param>
        /// <returns></returns>
        public float GetRelation(int agentID, int relationType)
        {
            if (agentRelations.Keys.Contains(agentID))
                return agentRelations[agentID][relationType];
            else
                return 0;
        }
        
        const int POSSIBLE_RESPONSES = 20;

        internal List<int> possibleResponses = new List<int>();
        /// <summary>
        /// The list of possible responses (in ActionID's) available to an agent, based on the action that he has received.
        /// Note that this only contains valid data when an Agent has received an action.
        /// 
        /// The HumanAgent class can access this during the InputRequest callback, to allow users or the program itself to determine the course of action.
        /// </summary>
        public List<int> PossibleResponses
        {
            get
            {
                return possibleResponses;
            }
        }

        #endregion variables
        /// <summary>
        /// Creates a new agent with the indicated parameter values.
        /// </summary>
        /// <param name="good"></param>
        /// <param name="bad"></param>
        /// <param name="beautiful"></param>
        /// <param name="ugly"></param>
        /// <param name="realistic"></param>
        /// <param name="unrealistic"></param>
        /// <param name="aid"></param>
        /// <param name="obstacle"></param>
        public Agent( float good, float bad, float beautiful, float ugly, float realistic, float unrealistic, float aid, float obstacle )
        {
            FEATURES[AgentFeatures.GOOD]            = good;
            FEATURES[AgentFeatures.BAD]             = bad;
            FEATURES[AgentFeatures.BEAUTIFUL]       = beautiful;
            FEATURES[AgentFeatures.UGLY]            = ugly;
            FEATURES[AgentFeatures.REALISTIC]       = realistic;
            FEATURES[AgentFeatures.UNREALISTIC]     = unrealistic;
            FEATURES[AgentFeatures.AID]             = aid;
            FEATURES[AgentFeatures.OBSTACLE]        = obstacle;

            _agentId = Model.RegisterAgent(this);
        }
        /// <summary>
        /// Creates a new agent with all personality parameters at 0.
        /// </summary>
        public Agent()
        {
            _agentId = Model.RegisterAgent(this);
        }

        void CalculatePerceivedFeatures(int agentID)
        {
            AgentFeatures tmp;
            for (int agent = 0; agent < perceivedAgents.Count; ++agent)
            {
                tmp = GetPerceivedFeatures(agent);

                for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                {
                    tmp[feature] = Model.GetAgentByID(agent).GetFeature(feature) * GetDesignedBias(agent, feature);
                }

                SetPerceivedFeatures(agent, tmp);
            }

            //for my self
            tmp = GetPerceivedFeatures(ID);
            for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
            {
                tmp[feature] = GetFeature(feature) * GetDesignedBias(ID, feature);
            }
            SetPerceivedFeatures(ID, tmp);

        }

        /// <summary>
        /// This function is called by the Model when an action is received.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="performed"></param>
        protected internal virtual void PerceiveAction(Agent sender, AgentAction performed)
        {
            //sanity check
            if (!perceivedAgents.Contains(sender.ID))
            {
                return;
                throw new Exception("The method of unregistering agents doesn't appear to work... Receiving action: "+performed.Name+" | from agent: "+sender.ID);
            }

            //update ourselves based on this action

            //if (HC[J1][2] != 0)
            //{
            //    perceived_feature[0][1][0] = beta_ethics * perceived_feature[0][1][0] + (1 - beta_ethics) * HC[J1][2];				//good
            //    //alert("Updated good towards "+HC[J1][2]);
            //}
            if (performed._positivity != 0)
            {
                SetPerceivedFeature( sender.ID, AgentFeatures.GOOD, beta_ethics * GetPerceivedFeature( sender.ID, AgentFeatures.GOOD ) + ( 1 - beta_ethics ) * performed._positivity );
            }

            //if (HC[J1][3] != 0)
            //{
            //    perceived_feature[0][1][1] = beta_ethics * perceived_feature[0][1][1] + (1 - beta_ethics) * HC[J1][3];				//bad
            //    //alert("Updated bad towards "+HC[J1][3]);
            //}
            if ( performed._negativity != 0 )
            {
                SetPerceivedFeature( sender.ID, AgentFeatures.BAD, beta_ethics * GetPerceivedFeature( sender.ID, AgentFeatures.BAD ) + ( 1 - beta_ethics ) * performed._negativity );
            }
            
            //TODO: Hoe vertaal ik dit ( HC[J1][8] ) naar een variabele die ik ken?      
            //if (HC[J1][8] != 0)
            //{
            //    perceived_feature[0][1][2] = beta_aesth * perceived_feature[0][1][2] + (1 - beta_aesth) * ((HC[J1][8] + 1) / 2.0);		//beautiful
            //    perceived_feature[0][1][3] = beta_aesth * perceived_feature[0][1][3] + (1 - beta_aesth) * (1 - ((HC[J1][8] + 1) / 2.0));	//ugly
            //    //alert("Updated beautiful using "+HC[J1][8]);
            //    //alert("Updated ugly using "+HC[J1][2]);
            //}
            if (performed._aesthetic != 0)
            {
                SetPerceivedFeature(sender.ID, AgentFeatures.BEAUTIFUL, beta_aesth * GetPerceivedFeature(sender.ID, AgentFeatures.BEAUTIFUL) + (1 - beta_aesth) * ((performed._aesthetic + 1) * 0.5f));
                SetPerceivedFeature(sender.ID, AgentFeatures.UGLY, beta_aesth * GetPerceivedFeature(sender.ID, AgentFeatures.UGLY) + (1 - beta_aesth) * (1 - ((performed._aesthetic + 1) * 0.5f)));
            }
            
            //Calculating GEU perceived by the agent in the action of the human
            //var p1=0; var n1=0;
            //var p_eu_a = new Array(STATES);
            //var n_eu_a = new Array(STATES);
            //for ( x=0;x<STATES;x++)
            //{
            //    var Goal = x + 4; //First four indexes of the AC array are not goals
            //    EU_action[0][J1][1][x] = HC[J1][Goal] * agent_ambition[0][x]; //Multiply agent's belief that action facilitates goal with ambition for goal
            //    //alert("EU_action[0]["+J1+"][1]["+x+"] = "+EU_action[0][J1][1][x]);
							
            //    if (EU_action[0][J1][1][x] > 0 )
            //    {	
            //        p_eu_a[p1]=EU_action[0][J1][1][x];
            //        p1++;
            //        //alert("p1 = "+p1);
            //    }
            //    if (EU_action[0][J1][1][x] < 0 )
            //    {
            //        n_eu_a[n1]=EU_action[0][J1][1][x];
            //        n1++;
            //        //alert("\nn1 = "+n1);
            //    }
            //}

            int p1 = 0, n1 = 0;
            List<float> p_eu_a = new List<float>();
            List<float> n_eu_a = new List<float>();

            for (int i = 0; i < Global.StateCount; ++i)
            {
                SetExpectedUtilityAction(i, sender.ID, performed.GlobalIndex, GetActionStateBelief(performed.GlobalIndex, i) * GetAmbition(i));

                float temp_eu_a = GetExpectedUtilityAction(i, sender.ID, performed.GlobalIndex);

                if (temp_eu_a > 0)
                {
                    p_eu_a.Add(temp_eu_a);
                    p1++;
                }
                else if (temp_eu_a < 0)
                {
                    n_eu_a.Add(temp_eu_a);
                    n1++;
                }
            }
                
            //p_eu_a.sort(sortNumber);
            //n_eu_a.sort(sortNumberInv);
            //var pos_eu1 = 0;
            //var neg_eu1 = 0;
            //for (x = 0; x < p1; x++)
            //{
            //    pos_eu1 = (pos_eu1 + p_eu_a[x]) / 2;
            //}
            //for (x = n1 - 1; x >= 0; --x)
            //{
            //    neg_eu1 = ((neg_eu1) + (n_eu_a[x])) / 2;
            //}
            //if ((p1 + n1) != 0) { wp = wGEUpa * p1 / (p1 + n1); } else wp = 0.5;
            //if ((p1 + n1) != 0) { wn = wGEUna * n1 / (p1 + n1); } else wn = 0.5;
            //GEU_action[0][J1][1] = wp * pos_eu1 + wn * neg_eu1;
            ////alert("GEU_action[0]["+J1+"][1] = "+GEU_action[0][J1][1]);

            //if (GEU_action[0][J1][1] > 0)
            //{
            //    perceived_feature[0][1][6] = perceived_feature[0][1][6] + mf_aid * GEU_action[0][J1][1] * (1 - perceived_feature[0][1][6]);	//aid
            //    perceived_feature[0][1][7] = perceived_feature[0][1][7] - mf_obst * GEU_action[0][J1][1] * perceived_feature[0][1][7];		//obstacle
            //}
            //if (GEU_action[0][J1][1] < 0)
            //{
            //    perceived_feature[0][1][6] = perceived_feature[0][1][6] + mf_aid * GEU_action[0][J1][1] * perceived_feature[0][1][6];		//aid
            //    perceived_feature[0][1][7] = perceived_feature[0][1][7] - mf_obst * GEU_action[0][J1][1] * (1 - perceived_feature[0][1][7]);	//obstacle
            //} 

            p_eu_a.Sort(Global.SortAscending);
            n_eu_a.Sort(Global.SortDescending);

            float pos_eu1 = 0, neg_eu1 = 0;

            for (int x = 0; x < p1; ++x)
            {
                pos_eu1 = (pos_eu1 + p_eu_a[x]) / 2;
            }
            for (int x = n1 - 1; x >= 0; --x)
            {
                neg_eu1 = ((neg_eu1) + (n_eu_a[x])) / 2;
            }
            float wp, wn;
            if ((p1 + n1) != 0) { wp = wGEUpa * p1 / (p1 + n1); } else wp = 0.5f;
            if ((p1 + n1) != 0) { wn = wGEUna * n1 / (p1 + n1); } else wn = 0.5f;

            SetGEUAction(sender.ID, performed.GlobalIndex, wp * pos_eu1 + wn * neg_eu1);

            float temp_geu_a = GetGEUAction(sender.ID, performed.GlobalIndex);

            if (temp_geu_a > 0)
            {
                SetPerceivedFeature(sender.ID, AgentFeatures.AID, GetPerceivedFeature(sender.ID, AgentFeatures.AID) + mf_aid * temp_geu_a * (1 - GetPerceivedFeature(sender.ID, AgentFeatures.AID)));
                SetPerceivedFeature(sender.ID, AgentFeatures.OBSTACLE, GetPerceivedFeature(sender.ID, AgentFeatures.OBSTACLE) - mf_obst * temp_geu_a * GetPerceivedFeature(sender.ID, AgentFeatures.OBSTACLE));
            }
            else if (temp_geu_a < 0)
            {
                SetPerceivedFeature(sender.ID, AgentFeatures.AID, GetPerceivedFeature(sender.ID, AgentFeatures.AID) + mf_aid * temp_geu_a * GetPerceivedFeature(sender.ID, AgentFeatures.AID));
                SetPerceivedFeature(sender.ID, AgentFeatures.OBSTACLE, GetPerceivedFeature(sender.ID, AgentFeatures.OBSTACLE) - mf_obst * temp_geu_a * ( 1 - GetPerceivedFeature(sender.ID, AgentFeatures.OBSTACLE)));
            }

            for (int i = 0; i < performed.responseList.Count; ++i)
            {
                possibleResponses.Add(performed.responseList[i]);
            }

            receivedAction = performed.GlobalIndex;
            receivedAgent = sender.ID;
        }

        /// <summary>
        /// This function is called every tick by the Model. Any Agents that have actions queued will perform these.
        /// This means all Agents will wait until the current cycle has completed, before performing their actions.
        /// </summary>
        protected internal virtual bool Perform()
        {
            //calculate highestTendencyAction
            
            //actionBroadcast(this, ACTIONS[0]);

            if ( queuedAction != -1 )
            {
                Console.WriteLine("Agent " + AgentID + " performing action: " + Global.GetActionByID(queuedAction).Name);

                Global.GetActionByID(queuedAction).Perform();

                Model.GetAgentByID( queuedTarget ).PerceiveAction( this, Global.GetActionByID(queuedAction ) );
                possibleResponses.Clear();
                Global.Broadcast(ID, queuedAction, queuedTarget);

                queuedAction = -1;
                queuedTarget = -1;

                return true;
            }

            return false;
        }

        /// <summary>
        /// Allows end-users to manually start an action, for instance to kick-start the Model.
        /// This can also be used to have AI Agents perform actions based on data from outside of the Model.
        /// </summary>
        /// <param name="toPerform">The action that is to be performed.</param>
        /// <param name="target">The Agent who is to be the target of the action.</param>
        public void ManualPerform(AgentAction toPerform, Agent target)
        {
            queuedAction = toPerform.GlobalIndex;
            queuedTarget = target.ID;
        }
    }
}