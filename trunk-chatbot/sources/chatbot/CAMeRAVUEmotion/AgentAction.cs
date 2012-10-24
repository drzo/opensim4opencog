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
    /// A class representing the actions agents (both human and AI) can undertake during the simulation.
    /// </summary>
    public class AgentAction
    {
        int _globalIndex;
        public int GlobalIndex
        {
            get
            {
                return _globalIndex;

            }
        }

        int _groupNumber;
        public int GroupNumber
        {
            get
            {
                return _groupNumber;
            }
        }
        int _groupIndex;
        public int GroupIndex
        {
            get
            {
                return _groupIndex;
            }
        }

        internal List<int> responseList = new List<int>();
        /// <summary>
        /// Added responses will be available when this action is received by an Agent.
        /// </summary>
        /// <param name="toAdd"></param>
        public void AddResponse(int toAdd)
        {
            responseList.Add(toAdd);
        }
        public void RemoveResponse(int toRemove)
        {
            responseList.Remove(toRemove);
        }
        public AgentAction(string name, float positivity, float negativity)
        {
            AgentAction0(name, positivity, negativity, -1);
        }
        public AgentAction(string name, float positivity, float negativity,int actionGroup)
        {
            AgentAction0(name, positivity, negativity, actionGroup);
        }
        public void AgentAction0(string name, float positivity, float negativity, int actionGroup)
        {
            _name = name;
            _positivity = positivity;
            _negativity = negativity;

            if (actionGroup != -1)
            {
                if (actionGroup == 0)
                    throw new Exception("Action Group 0 is reserved for unassigned actions");
            }
            else
                actionGroup = 0;

            _globalIndex = Global.NextActionID();
                
            //add this action to the end of the group
            List<AgentAction> tmp;
            _groupNumber = actionGroup;

            if (Global.ACTIONS.Keys.Contains(actionGroup))
                tmp = Global.ACTIONS[actionGroup];
            else
                tmp = new List<AgentAction>();

            _groupIndex = tmp.Count;

            tmp.Add(this);

            Global.ACTIONS[actionGroup] = tmp;
        }

        string _name ="uninitialized";
        public string Name
        {
            get
            {
                return _name;
            }
        }

        internal float _positivity = 0, _negativity = 0, _aesthetic = 0;

        public delegate void ActionPerformEvent();

        public ActionPerformEvent performed;

        internal void Perform()
        {
            if (performed != null)
                performed();
        }

        public void SetValence(float pos, float neg)
        {
            _positivity = pos;
            _negativity = neg;
        }

        //internal Dictionary<int, float> stateInfluences = new Dictionary<int, float>();
        //public void SetInfluence(int state, float influence)
        //{
        //    stateInfluences[state] = influence;
        //}
        //public float GetInfluence(int state)
        //{
        //    if (stateInfluences.Keys.Contains(state))
        //        return stateInfluences[state];
        //    else
        //        return 0;
        //}
    }
}
