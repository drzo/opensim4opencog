using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

namespace RTParser.Utils
{
    /// <summary>
    /// Encapsulates information about a custom tag class
    /// </summary>
    public class TagHandler
    {
        /// <summary>
        /// The assembly this class is found in
        /// </summary>
        public Unifiable AssemblyName;

        /// <summary>
        /// The class name for the assembly
        /// </summary>
        public Unifiable ClassName;

        /// <summary>
        /// The name of the tag this class will deal with
        /// </summary>
        public Unifiable TagName;

        /// <summary>
        /// Provides an instantiation of the class represented by this tag-handler
        /// </summary>
        /// <param name="Assemblies">All the assemblies the bot knows about</param>
        /// <returns>The instantiated class</returns>
        public AIMLTagHandler Instantiate(Dictionary<Unifiable, Assembly> Assemblies)
        {
            if (Assemblies.ContainsKey(this.AssemblyName))
            {
                Assembly tagDLL = (Assembly)Assemblies[this.AssemblyName]; 
                Type[] tagDLLTypes = tagDLL.GetTypes();
                return (AIMLTagHandler)tagDLL.CreateInstance(this.ClassName);
            }
            else
            {
                return null;
            }
        }
    }
}
