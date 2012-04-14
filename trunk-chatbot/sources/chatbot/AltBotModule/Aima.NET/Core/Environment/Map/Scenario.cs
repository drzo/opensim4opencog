using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    /// <summary>
    /// A scenario specifies an environment, the agent's knowledge about the
    /// environment, and the agents initial location. It can be used to specify
    /// settings for route planning agent applications.
    /// </summary>
    public class Scenario 
    {

        /// <summary>
        /// A map-based environment. Note that the contained map must be of type <see cref="ExtendableMap"/>
        /// </summary>
        private readonly MapEnvironment env;
        
        /// <summary>
        /// A map reflecting the knowledge of the agent about the environment.
        /// </summary>
        private readonly IMap agentMap;

        /// <summary>
        /// Initial location of the agent.
        /// </summary>
        private readonly string initAgentLoc;

        /// <summary>
        /// Initializes a new instance of the <see cref="Scenario"/> class.
        /// </summary>
        /// <param name="env">
        /// a map-based environment. Note that the contained map must be
        /// of type <see cref="ExtendableMap"/>
        /// </param>
        /// <param name="agentMap">
        /// a map reflecting the knowledge of the agent about the
        /// environment
        /// </param>
        /// <param name="agentLoc">
        /// initial location of the agent
        /// </param>
        public Scenario(MapEnvironment env, IMap agentMap, string agentLoc) 
        {
            this.agentMap = agentMap;
            this.env = env;
            this.initAgentLoc = agentLoc;
        }

        public MapEnvironment GetEnv() 
        {
            return env;
        }

        public IMap GetEnvMap() 
        {
            return env.GetMap();
        }

        public IMap GetAgentMap() 
        {
            return agentMap;
        }

        public String GetInitAgentLocation() 
        {
            return initAgentLoc;
        }
    }

}
