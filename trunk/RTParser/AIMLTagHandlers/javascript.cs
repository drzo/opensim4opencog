using System;
using System.Xml;
using System.Text;
using System;
using System.CodeDom.Compiler;
using System.Reflection;
using Microsoft.JScript;


namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// NOT IMPLEMENTED FOR SECURITY REASONS
    /// </summary>
    public class javascript : RTParser.Utils.AIMLTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public javascript(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChange()
        {
            string s = templateNodeInnerText = Recurse();
            if (GetAttribValue("onClient","False").StartsWith("T"))
            {
                writeToLogWarn("javascript tag is happeming on client: " + s);
                return templateNode.OuterXml;
            }
#if NOJAVASCRIPT
            writeToLog("The javascript tag is not implemented in this Proc");
#else
            try
            {
                s = "" + Evaluator.EvalToObject(templateNodeInnerText);
            }
            catch (Exception e)
            {
                writeToLog("ERROR: " + e + " caused by " + s);
            }
#endif
            return s;
        }
    }
#if !(NOJAVASCRIPT)
    public class Evaluator
    {
        public static int EvalToInteger(string statement)
        {
            string s = EvalToString(statement);
            return int.Parse(s.ToString());
        }

        public static double EvalToDouble(string statement)
        {
            string s = EvalToString(statement);
            return double.Parse(s);
        }

        public static string EvalToString(string statement)
        {
            object o = EvalToObject(statement);
            return o.ToString();
        }

        public static object EvalToObject(string statement)
        {
            return _evaluatorType.InvokeMember(
                        "Eval",
                        BindingFlags.InvokeMethod,
                        null,
                        _evaluator,
                        new object[] { statement }
                     );
        }

        static Evaluator()
        {
            ICodeCompiler compiler;
            compiler = new JScriptCodeProvider().CreateCompiler();

            CompilerParameters parameters;
            parameters = new CompilerParameters();
            parameters.GenerateInMemory = true;

            CompilerResults results;
            results = compiler.CompileAssemblyFromSource(parameters, _jscriptSource);

            Assembly assembly = results.CompiledAssembly;
            _evaluatorType = assembly.GetType("Evaluator.Evaluator");

            _evaluator = Activator.CreateInstance(_evaluatorType);
        }

        private static object _evaluator = null;
        private static Type _evaluatorType = null;
        private static readonly string _jscriptSource =

            @"package Evaluator
            {
               class Evaluator
               {
                  public function Eval(expr : String) : String 
                  { 
                     return eval(expr); 
                  }
               }
            }";
    }
#endif
}
