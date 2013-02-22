using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Xml;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using MushDLR223.Virtualization;

namespace AltAIMLbot
{
    /// <summary>
    /// Encapsulates an AIML Custom Tag Proccessor.
    /// </summary>
    public partial class TagHandlerProcessor // : StaticAIMLUtils
    {
        /// <summary>
        /// Holds information about the available custom tag handling classes (if loaded)
        /// Key = class name
        /// Value = TagHandler class that provides information about the class
        /// </summary>
        static internal Dictionary<string, TagHandler> CustomTags;

        /// <summary>
        /// Holds references to the assemblies that hold the custom tag handling code.
        /// </summary>
        static readonly Dictionary<string, Assembly> LateBindingAssemblies = new Dictionary<string, Assembly>();

        internal static void InitTagHandlers()
        {
            CustomTags = new Dictionary<string, TagHandler>();
            //this.GraphMaster = new GraphMaster();
            //this.HeardSelfSayGraph = new GraphMaster();
            if (HostSystem.FileExists("AIMLbot.dll")) loadCustomTagHandlers("AIMLbot.dll");
            if (HostSystem.FileExists("AIMLbot.exe")) loadCustomTagHandlers("AIMLbot.exe");
        }

        /// <summary>
        /// Searches the CustomTag collection and processes the AIML if an appropriate tag handler is found
        /// </summary>
        /// <param name="user">the user who originated the request</param>
        /// <param name="query">the query that produced targetBot node</param>
        /// <param name="request">the request from the user</param>
        /// <param name="result">the result to be sent to the user</param>
        /// <param name="node">the node to evaluate</param>
        /// <returns>the output Unifiable</returns>
        static public AIMLTagHandler getBespokeTags(User user, SubQuery query, Request request, Result result, XmlNode node)
        {
            AltBot targetBot = query.TargetBot;
            string nodename = node.Name.ToLower();
            if (CustomTags != null)
            {
                //return null;
                try
                {
                    lock (CustomTags)


                        if (CustomTags.ContainsKey(nodename))
                        {
                            TagHandler customTagHandler = CustomTags[node.Name.ToLower()];

                            AIMLTagHandler newCustomTag = customTagHandler.Instantiate(LateBindingAssemblies, user,
                                                                                       query,
                                                                                       request, result, node, targetBot);
                            if (Equals(null, newCustomTag))
                            {
                                return null;
                            }
                            else
                            {
                                return newCustomTag;
                            }
                        }
                }
                catch (Exception e)
                {
                    writeToLog("WARNING IN GET BESPOKE TAGS: " + e);
                }
            }
            {
                try
                {
                    if (nodename.StartsWith("#")) return null;
                    String typeName = typeof(TagHandlerProcessor).Namespace + ".AIMLTagHandlers." + nodename;
                    Type t = Type.GetType(typeName);
                    if (t == null) return null;
                    ConstructorInfo c = t.GetConstructor(TagHandler.CONSTRUCTOR_TYPES);
                    return (AIMLTagHandler)c.Invoke(new object[] { targetBot, user, query, request, result, node });
                }
                catch (Exception e)
                {
                    writeToLog("ERROR getBespokeTags: " + e);
                    return null;
                }
            }
        }


        #region Latebinding custom-tag dll handlers

        /// <summary>
        /// Loads any custom tag handlers found in the dll referenced in the argument
        /// </summary>
        /// <param name="pathToDLL">the path to the dll containing the custom tag handling code</param>
        static public void loadCustomTagHandlers(string pathToDLL)
        {
            // return;
            string hostSystemResolveToExistingPath = HostSystem.ResolveToExistingPath(pathToDLL);
            if (hostSystemResolveToExistingPath==null)
            {
                throw new FileNotFoundException(pathToDLL);
            }
            Assembly tagDLL = Assembly.LoadFrom(hostSystemResolveToExistingPath);
            var tagDLLTypes = tagDLL.GetTypes();
            for (int i = 0; i < tagDLLTypes.Length; i++)
            {
                Type type = tagDLLTypes[i];
                try
                {
                    var typeCustomAttributes = type.GetCustomAttributes(false);
                    if (typeCustomAttributes.Length == 0 && typeof (AIMLTagHandler).IsAssignableFrom(type) &&
                        !type.IsAbstract && !type.IsInterface)
                    {
                        try
                        {
                            AddTagHandler(type);
                        }
                        catch (Exception e)
                        {
                            AltBot.writeException(e);

                        }
                        continue;
                    }
                    for (int j = 0; j < typeCustomAttributes.Length; j++)
                    {
                        if (typeCustomAttributes[j] is CustomTagAttribute)
                        {
                            // We've found a custom tag handling class
                            // so store the assembly and store it away in the Dictionary<,> as a TagHandler class for 
                            // later usage
                            try
                            {
                                AddTagHandler(type);
                            }
                            catch (Exception e)
                            {
                                AltBot.writeException(e);

                            }
                        }
                    }
                }
                catch (Exception ee)
                {
                    AltBot.writeException(ee);
                }
            }
        }

        static public void AddTagHandler(Type type)
        {
            Assembly tagDLL = type.Assembly;
            // store Assembly
            if (!LateBindingAssemblies.ContainsKey(tagDLL.FullName))
            {
                LateBindingAssemblies.Add(tagDLL.FullName, tagDLL);
            }
            // create the TagHandler representation
            TagHandler newTagHandler = new TagHandler(type);
            newTagHandler.AssemblyName = tagDLL.FullName;
            newTagHandler.ClassName = type.FullName;
            newTagHandler.TagName = type.Name.ToLower();
            if (CustomTags.ContainsKey(newTagHandler.TagName))
            {
                throw new Exception("ERROR! Unable to add the custom tag: <" + newTagHandler.TagName + ">, found in: " +
                                    tagDLL.Location + " as a handler for this tag already exists.");
            }
            else
            {
                CustomTags.Add(newTagHandler.TagName, newTagHandler);
            }
        }
        #endregion

    }
}