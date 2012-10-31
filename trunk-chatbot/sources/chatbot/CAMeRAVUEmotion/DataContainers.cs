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
    /// Features of Agents
    /// </summary>
    public struct AgentFeatures
    {
        public const int GOOD = 0;
        public const int BAD = 1;
        public const int BEAUTIFUL = 2;
        public const int UGLY = 3;
        public const int REALISTIC = 4;
        public const int UNREALISTIC = 5;
        public const int AID = 6;
        public const int OBSTACLE = 7;

        public const int NUM_VALUES = 8;

        float[] values;

        public float this[int index]
        {
            get
            {
                if (index > NUM_VALUES-1)
                    throw new IndexOutOfRangeException("AgentRelations indices cannot exceed " + (NUM_VALUES - 1));

                if (values == null)
                    values = new float[NUM_VALUES];

                return values[index];
            }
            set
            {
                if (index > NUM_VALUES-1)
                    throw new IndexOutOfRangeException("AgentRelations indices cannot exceed " + (NUM_VALUES-1));

                if (values == null)
                    values = new float[NUM_VALUES];

                values[index] = value;
            }
        }

        public AgentFeatures(float good, float bad, float beautiful, float ugly, float realistic, float unrealistic, float aid, float obstacle)
        {
            values = new float[8];

            values[GOOD] = good;
            values[BAD] = bad;
            values[BEAUTIFUL] = beautiful;
            values[UGLY] = ugly;
            values[REALISTIC] = realistic;
            values[UNREALISTIC] = unrealistic;
            values[AID] = aid;
            values[OBSTACLE] = obstacle;
        }
        public static int Parse(string name)
        {
            name = name.ToLower();
            switch (name.ToLower())
            {
                case "good": return GOOD;
                case "bad": return BAD;
                case "beatutiful": return BEAUTIFUL;
                case "realistic": return REALISTIC;
                case "unrealistic": return UNREALISTIC;
                case "ugly": return UGLY;
                case "aid": return AID;
                case "obstacle": return OBSTACLE;
            }
            return -1;
        }
    }

    /// <summary>
    /// Emotions of Agents
    /// </summary>
    public struct AgentEmotions
    {
        public const int HOPE = 0;
        public const int FEAR = 1;
        public const int JOY = 2;
        public const int DISTRESS = 3;
        public const int SURPRISE = 4;
        public const int ANGER = 5;
        public const int GUILT = 6;

        public const int NUM_VALUES = 7;

        float[] values;

        public int Count
        {
            get
            {
                if (values == null)
                    values = new float[NUM_VALUES];

                return values.Length;
            }
        }

        public float this[int index]
        {
            get
            {
                if (index > NUM_VALUES-1)
                    throw new IndexOutOfRangeException("AgentEmotions indices cannot exceed " + (NUM_VALUES-1));

                if (values == null)
                    values = new float[NUM_VALUES];

                return values[index];
            }
            set
            {
                if (index > NUM_VALUES-1)
                    throw new IndexOutOfRangeException("AgentEmotions indices cannot exceed " + (NUM_VALUES - 1));

                if (values == null)
                    values = new float[NUM_VALUES];

                values[index] = value;
            }
        }
        public static int Parse(string name)
        {
            name = name.ToLower();
            switch (name.ToLower())
            {
                case "hope": return HOPE;
                case "fear": return FEAR;
                case "joy": return JOY;
                case "distress": return DISTRESS;
                case "surprise": return SURPRISE;
                case "anger": return ANGER;
                case "guilt": return GUILT;
            }
            return -1;
        }

        public static string StringFor(int emotion)
        {
            switch (emotion)
            {
                case HOPE: return "hope";
                case FEAR: return "fear";
                case JOY: return "joy";
                case DISTRESS: return "distress";
                case SURPRISE: return "surprise";
                case ANGER: return "anger";
                case GUILT: return "guilt";
            }
            return "unknown";
        }

    }

    /// <summary>
    /// Relationships between Agents
    /// </summary>
    public struct AgentRelations
    {
        public const int SIMILARITY = 0;
        public const int DISSIMILARITY = 1;
        public const int RELEVANCE = 2;
        public const int IRRELEVANCE = 3;
        public const int P_VALENCE = 4;
        public const int N_VALENCE = 5;
        public const int USE_INTENTION = 6;
        public const int INVOLVEMENT = 7;
        public const int DISTANCE = 8;
        public const int INV_DIST_TRADEOFF = 9;
        public const int EXP_SATISFACTION = 10;

        public const int NUM_VALUES = 11;

        float[] values;

        public float this[int index]
        {
            get
            {
                if (index > NUM_VALUES-1)
                    throw new IndexOutOfRangeException("AgentRelations indices cannot exceed "+(NUM_VALUES-1));

                if (values == null)
                    values = new float[NUM_VALUES];

                return values[index];
            }
            set
            {
                if (index > NUM_VALUES-1)
                    throw new IndexOutOfRangeException("AgentRelations indices cannot exceed " + (NUM_VALUES - 1));

                if (values == null)
                    values = new float[NUM_VALUES];

                values[index] = value;
            }
        }

        public static int Parse(string name)
        {
            name = name.ToLower();
            switch (name.ToLower())
            {
                case "similarity": return SIMILARITY;
                case "dissimilarity": return DISSIMILARITY;
                case "relevance": return RELEVANCE;
                case "irrelevance": return IRRELEVANCE;
                case "p_valence": return P_VALENCE;
                case "n_valence": return N_VALENCE;
                case "use_intention": return USE_INTENTION;
                case "involvement": return INVOLVEMENT;
                case "distance": return DISTANCE;
                case "inv_dist_tradeoff": return INV_DIST_TRADEOFF;
                case "exp_Satisfaction": return EXP_SATISFACTION;
            }
            return -1;
        }

    }

    /// <summary>
    /// Links a Feature of a specific Agent and a State, to a Value
    /// </summary>
    public struct FeatureStateBelief
    {
        internal int _target, _feature, _state;

        internal float _value;
        public float Value
        {
            get
            {
                return _value;
            }
            set
            {
                _value = value;
            }
        }

        //It may be useful to set Agents to null in order to support
        //general beliefs about the influence of features on goal states
        public FeatureStateBelief(int feature, int state, int targetAgent, float value)
        {
            _target = targetAgent;
            _feature = feature;
            _state = state;
            _value = value;
        }
        public FeatureStateBelief(int feature, int state, int targetAgent)
        {
            _target = targetAgent;
            _feature = feature;
            _state = state;
            _value = 0;
        }

    }

    /// <summary>
    /// Links two States, to a Value
    /// </summary>
    public struct StateStateBelief
    {
        internal int _state1, _state2;

        internal float _value;
        public float Value
        {
            get
            {
                return _value;
            }
            set
            {
                _value = value;
            }
        }

        public StateStateBelief(int state1, int state2, float value )
        {
            _state1 = state1;
            _state2 = state2;
            _value = value;
        }
        public StateStateBelief(int state1, int state2)
        {
            _state1 = state1;
            _state2 = state2;
            _value = 0;
        }

    }

    /// <summary>
    /// Links an Agent and a State, to a Value
    /// </summary>
    public struct AgentStateBelief
    {
        internal int _agentID, _state;

        internal float _value;
        public float Value
        {
            get
            {
                return _value;
            }
            set
            {
                _value = value;
            }
        }
        
        public AgentStateBelief(int agentID, int state, float value )
        {
            _state = state;
            _agentID = agentID;
            _value = value;
        }
        public AgentStateBelief(int agentID, int state)
        {
            _state = state;
            _agentID = agentID;
            _value = 0;
        }
    }

    /// <summary>
    /// Links an Action and a State, to a Value
    /// </summary>
    public struct ActionStateBelief
    {
        internal int _action, _state;

        internal float _value;
        public float Value
        {
            get
            {
                return _value;
            }
            set
            {
                _value = value;
            }
        }
        
        public ActionStateBelief(int action, int state, float value)
        {
            _state = state;
            _action = action;
            _value = value;
        }
        public ActionStateBelief(int action, int state)
        {
            _state = state;
            _action = action;
            _value = 0;
        }
    }

    
    /// <summary>
    /// Links an Action and a MoralPrinciple, to a Value
    /// </summary>
    public struct ActionMoralPrincipleBelief
    {
        internal int _action, _moralprinciple;

        internal float _value;
        public float Value
        {
            get
            {
                return _value;
            }
            set
            {
                _value = value;
            }
        }

        public ActionMoralPrincipleBelief(int action, int moralprinciple, float value)
        {
            _moralprinciple = moralprinciple;
            _action = action;
            _value = value;
        }
        public ActionMoralPrincipleBelief(int action, int moralprinciple)
        {
            _moralprinciple = moralprinciple;
            _action = action;
            _value = 0;
        }
    }

    /// <summary>
    /// Links a Feature of a specific Agent and a State, to a Value
    /// </summary>
    public struct ExpectedUtility
    {
        internal int _agentID, _state, _feature;

        internal float _value;
        public float Value
        {
            get
            {
                return _value;
            }
            internal set
            {
                _value = Value;
            }
        }
        
        public ExpectedUtility(int agentID, int state, int feature, float value )
        {
            _state = state;
            _agentID = agentID;
            _feature = feature;
            _value = value;
        }
        public ExpectedUtility(int agentID, int state, int feature)
        {
            _state = state;
            _agentID = agentID;
            _feature = feature;
            _value = 0;
        }
    }



    /// <summary>
    /// Links a Feature of a specific Agent and a MoralPrinciple, to a Value
    /// </summary>
    public struct Morality
    {
        internal int _agentID, _moralprinciple, _feature;

        internal float _value;
        public float Value
        {
            get
            {
                return _value;
            }
            internal set
            {
                _value = value;
            }
        }

        public Morality(int agentID, int moralprinciple, int feature, float value)
        {
            _moralprinciple = moralprinciple;
            _agentID = agentID;
            _feature = feature;
            _value = value;
        }
        public Morality(int agentID, int moralprinciple, int feature)
        {
            _moralprinciple = moralprinciple;
            _agentID = agentID;
            _feature = feature;
            _value = 0;
        }
    }
}