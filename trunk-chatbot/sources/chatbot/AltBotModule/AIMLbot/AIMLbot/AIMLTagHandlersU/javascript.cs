// #define USEVSAHOST

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Xml;
using AltAIMLbot.Utils;
using Microsoft.JScript;
using Microsoft.JScript.Vsa;

#if USEVSAHOST
using Evaluator;
#endif

namespace AltAIMLbot.AIMLTagHandlers
{

    /// <summary>
    /// NOT IMPLEMENTED FOR SECURITY REASONS
    /// </summary>
    public class javascript : AIMLTagHandlerU
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public javascript(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChangeU()
        {
            string innerText = SafelyGetInnerText(false);
            if (IsNullOrEmpty(innerText))
            {
                return templateNode.OuterXml;
            }
            bool onClient;
            if (TryParseBool(templateNode, "onClient", out onClient))
            {
                if (onClient)
                {
                    writeToLog("javascript tag is happeming on client: " + innerText);
                    return templateNode.OuterXml;
                }
            }
#if NOJAVASCRIPT
            writeToLog("The javascript tag is not implemented in this Proc");
#else
            try
            {
                innerText = "" + EvalJScript(innerText);
            }
            catch (ChatSignal ex)
            {
                throw;
            }
            catch (Exception e)
            {
                writeToLogWarn("ERROR: " + e + "\nCaused by \"" + innerText + "\"");
            }
#endif
            return innerText;
        }

        public string SafelyGetInnerText(bool saveForNexTime)
        {
            var r = Recurse();
            string innerText = r;
            if (!IsNullOrEmpty(innerText))
            {
                if (saveForNexTime)
                {
                    templateNodeInnerText = innerText;
                }
                return innerText;
            }
            return innerText;
        }

        /// <summary>
        /// Returns a string array containing the names of the assemblies added as references to the engine by default.
        /// </summary>
        public static string[] DefaultAssemblyReferences = new string[] 
			{
				@"mscorlib.dll",
				@"System.dll",
				@"System.Data.dll",
				@"System.Drawing.dll",
				@"System.Windows.Forms.dll",
				@"System.Xml.dll",
				@"Microsoft.Vsa.dll"
			};
        /// <summary>
        /// Returns a string array containing the names of the assemblies added as references to the engine by default.
        /// </summary>
        public static string[] DefaultNamespaceReferences = new string[]
                                                               {
                                                                   "System",
                                                                   "System.Collections",
                                                                   "System.ComponentModel",                                                               
                                                                   "System.Data",
                                                                   "System.Drawing",
                                                                   "System.Windows.Forms",
                                                                   "System.Xml",
                                                               };
#if !(_MonoCS__)		
        public static readonly VsaEngine Engine;
#endif

        static javascript()
        {
#if USEVSAHOST
            Engine = Evaluator.Evaluator.Engine;
#else
            List<string> assemblies = new List<string>(DefaultAssemblyReferences);
            Assembly executingAssembly = Assembly.GetExecutingAssembly();
            foreach (var name in executingAssembly.GetReferencedAssemblies())
            {
                string codeBase = name.CodeBase;
                try
                {
                    Assembly assembly = Assembly.Load(name);

                    if (assembly.GlobalAssemblyCache)
                    {
                        codeBase = Path.GetFileName(assembly.Location);
                    }
                    else
                    {
                        codeBase = assembly.Location;
                    }
                }
                catch (ChatSignal ex)
                {
                    throw;
                }
                catch (Exception)
                {
                }
                if (codeBase == null)
                {
                    codeBase = name.Name + ".dll";
                }
                if (!assemblies.Contains(codeBase))
                {
                    assemblies.Add(codeBase);
                }
            }
            var GS = VsaEngine.CreateEngineAndGetGlobalScope(true, assemblies.ToArray());
            Engine = GS.engine;
#endif
            foreach (string reference in DefaultNamespaceReferences)
            {
                Import.JScriptImport(reference, Engine);
            }
            object o = Eval.JScriptEvaluate("this", Engine);
            writeDebugLine("JSciptThis = " + o);
        }

#if (!USEVSAHOST)
        private object EvalJScript(string src)
        {
            var engine = Engine;
            try
            {
                With.JScriptWith(this, Engine);
                return Eval.JScriptEvaluate(src, "unsafe", engine);
            }
            finally
            {
                engine.PopScriptObject( /* with */);
            }
        }
#else
        public static javascript handler;
        private object EvalJScript(string src)
        {
            var engine = Engine;
            var _expression = src;
            var context = this;
            lock (engine)
            {
                handler = this;
                return EvalJScript0(src);
            }
        }
        private object EvalJScript0(string src)
        {
            Evaluator.EvaluationStrategy ft = null;
            if (false) try
                {
                    ft = new PartialTrustEvaluationStrategy(src, this, Evaluator.Evaluator.Engine);
                    object hndlr31 = ft.Eval(this);
                    if (hndlr31 != null) return hndlr31;
                }
                catch (Exception ex)
                {
                    writeToLogWarn("ERROR PartialTrustEvaluationStrategy: " + ex);
                }
            try
            {
                ft = new FullTrustEvaluationStrategy(src, this, Evaluator.Evaluator.Engine);
                try
                {
                    object hndlr31 = ft.Eval(this);
                    if (hndlr31 != null) return hndlr31;

                }
                catch (Exception ex)
                {
                    writeToLogWarn("ERROR FullTrustEvaluationStrategy: " + ex);
                }
            }
            catch (Exception ex)
            {
                writeToLogWarn("ERROR FullTrustEvaluationStrategy: " + ex);
            }
            try
            {
                object hndlr31 = Evaluator.Evaluator.EvalJScript(src);
                if (hndlr31 != null) return hndlr31;
            }
            catch (Exception ex)
            {
                writeToLogWarn("ERROR EvalJScript " + ex);
                return "ERROR EvalJScript: " + ex.Message;
            }
            return null;
        }

#endif
    }
}
