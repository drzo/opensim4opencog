using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Xml;

namespace RTParser.Utils
{
    /// <summary>
    /// Encapsulates information about a custom tag class
    /// </summary>
    public class TagHandler
    {
        public TagHandler(Type type)
        {
            this.type = type;
            ClassName = type.Name;
            TagName = ClassName.ToLower();
            AssemblyName = type.Assembly.FullName;
        }
        /// <summary>
        /// The assembly this class is found in
        /// </summary>
        public string AssemblyName;

        /// <summary>
        /// The class name for the assembly
        /// </summary>
        public string ClassName;

        /// <summary>
        /// The name of the tag this class will deal with
        /// </summary>
        public string TagName;

        private ConstructorInfo info;

        public Type type;

        /// <summary>
        /// Provides an instantiation of the class represented by this tag-handler
        /// </summary>
        /// <param name="Assemblies">All the assemblies the bot knows about</param>
        /// <returns>The instantiated class</returns>
        public AIMLTagHandler Instantiate(Dictionary<string, Assembly> Assemblies, User user, SubQuery query, Request request, Result result, XmlNode node, RTPBot bot)
        {
            if (Assemblies.ContainsKey(this.AssemblyName))
            {
                if (type!=null)
                {
                    if (info == null) info = type.GetConstructor(CONSTRUCTOR_TYPES);
                    if (info != null) return (AIMLTagHandler)info.Invoke(new object[] { bot, user, query, request, result, node });
                }
                Assembly tagDLL = (Assembly)Assemblies[this.AssemblyName]; 
                AIMLTagHandler newCustomTag = (AIMLTagHandler)tagDLL.CreateInstance(ClassName);
                newCustomTag.user = user;
                newCustomTag.query = query;
                newCustomTag.request = request;
                newCustomTag.result = result;
                newCustomTag.templateNode = node;
                newCustomTag.Proc = bot;
                return newCustomTag;
            }
            else
            {
                return null;
            }
        }

        protected static Type[] CONSTRUCTOR_TYPES = new[]
                                                        {
                                                            typeof (RTPBot), typeof (User), typeof (SubQuery),
                                                            typeof (Request), typeof (Result), typeof (XmlNode)
                                                        };
    }
}
