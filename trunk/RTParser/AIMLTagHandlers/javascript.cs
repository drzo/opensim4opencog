using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Security;
using System.Security.Permissions;
using System.Xml;
using System.Text;
using System;
using System.CodeDom.Compiler;
using System.Reflection;
using Evaluator;
using Microsoft.JScript;
using Microsoft.JScript.Vsa;
using Microsoft.Vsa;
using System.Diagnostics;
using RTParser.AIMLTagHandlers;
using Convert = Microsoft.JScript.Convert;


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
                CurrentHandler = this;
                Evaluator.Evaluator.AddGlobalItem("query", query);
                Evaluator.Evaluator.AddGlobalItem("handler", this);
                innerText = "" + EvalJScript(innerText);
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
                    this.templateNodeInnerText = innerText;
                }
                return innerText;
            }
            return innerText;
        }

        private object EvalJScript(string src)
        {
            EvaluationStrategy ft = null;
            if (false)try
            {
                ft = new PartialTrustEvaluationStrategy(src, Evaluator.Evaluator.Engine);
                object hndlr31 = ft.Eval(this);
                if (hndlr31 != null) return hndlr31;
            }
            catch (Exception ex)
            {
                RTParser.RTPBot.writeDebugLine("ERROR PartialTrustEvaluationStrategy: " + ex);
            }
            try
            {
                ft = new FullTrustEvaluationStrategy(src, Evaluator.Evaluator.Engine);
                try
                {
                    object hndlr31 = ft.Eval(this);
                    if (hndlr31 != null) return hndlr31;

                }
                catch (Exception ex)
                {
                    RTParser.RTPBot.writeDebugLine("ERROR FullTrustEvaluationStrategy: " + ex);
                }

            }
            catch (Exception ex)
            {
                RTParser.RTPBot.writeDebugLine("ERROR FullTrustEvaluationStrategy: " + ex);
            }

            try
            {
                object hndlr31 = Evaluator.Evaluator.EvalJScript(src);
                if (hndlr31 != null) return hndlr31;
            }
            catch (Exception ex)
            {
                RTParser.RTPBot.writeDebugLine("ERROR EvalJScript " + ex);
                return "ERROR EvalJScript: " + ex.Message;
            }
            return null;
        }

        public static javascript CurrentHandler;
    }
}
#if !(NOJAVASCRIPT)
namespace Evaluator
{
    // so extension methods will allow me to pass arround my simple types.. such as "nephrael rae" but have it work as unificable
    // OutputDelegate/ExceptionHandlingThreadController/ 
    static public class Evaluator
    {
        readonly public static Microsoft.JScript.Vsa.VsaEngine Engine;
        readonly public static IVsaScriptScope globalScope;
        public static readonly List<VsaScriptingHost> hosts = new List<VsaScriptingHost>();
        /// <summary>
        /// Create a new host factory
        /// </summary>
        public static VsaScriptingHostFactory HostFactory = new VsaScriptingHostFactory();

        public static readonly VsaScriptingHost _host;
        public static readonly string rootHostName = @"MyScriptingHost";
        public static GlobalScope thisGlobalObj;

        public static object EvalJScript(string srcJScript)
        {
                //Engine.Host.CreateGlobalItem("_scriptableForm", _scriptableForm, false);
               return Microsoft.JScript.Eval.JScriptEvaluate(srcJScript, Engine);

        }

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
            //_evaluatorCtxType = assembly.GetType("Evaluator.Context");
            _evaluator = Activator.CreateInstance(_evaluatorType);

            // create a new form instance
            // _scriptableForm = new ScriptableForm();

            // create a new set of hosts
            lock (hosts)
            {
                string rootMoniker = string.Format("{0}://{1}", rootHostName, Guid.NewGuid().ToString());
                _host = new VsaScriptingHost(VsaScriptingLanguages.JScript, "Evaluator", rootMoniker,
                                             _evaluatorType.Namespace, true);
                Engine = (VsaEngine) _host.Engine;
                globalScope = Engine.GetGlobalScope();
                thisGlobalObj = Microsoft.JScript.Eval.JScriptEvaluate("this;", Engine) as GlobalScope;
                hosts.Add(_host);
                _host.AddType(typeof(System.Object));
                _host.AddType(typeof(System.String));
                AddGlobalItem("$engine", Engine);
                // hosts.AddRange(HostFactory.Create(@"MyScriptingHost", @"Scripting", true, Environment.CurrentDirectory));                
            }

            // wire up to the events of each host
            foreach (VsaScriptingHost host in hosts)
            {
                host.AssemblyReferencesNeeded += new ScriptingHostEventHandler(OnAssemblyReferencesNeeded);
                host.CompilerException += new ScriptingHostCompilerExceptionEventHandler(OnCompilerException);
                host.GlobalItemsNeeded += new ScriptingHostEventHandler(OnGlobalItemsNeeded);
            }

            // execute the hosts
            foreach (VsaScriptingHost host in hosts)
                host.Execute();


            // show the form
            //_scriptableForm.ShowDialog();

        }

        public static object Context;

        private static readonly object _evaluator;
        private static readonly Type _evaluatorType;
        //private static Type _evaluatorCtxType = null;
        private const string _jscriptSource =

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

        public static void AddGlobalItem(string name, object value)
        {
            //VsaItemType vsaItemType = VsaItemType.AppGlobal;
            //var v = globalScope.CreateDynamicItem(name, vsaItemType);
            var fi = thisGlobalObj.AddField(name);
            if (fi != null)
            {
                fi.SetValue(thisGlobalObj, value);
            }
            else
            {
                debugInfo("thisGlobalObj", thisGlobalObj);
                if (globalScope != thisGlobalObj)
                {
                    debugInfo("globalScope", globalScope);
                }
                debugInfo("d:", thisGlobalObj.GetDefaultThisObject());
                foreach (var s in thisGlobalObj.GetMembers(BindingFlags.Public | BindingFlags.Instance| BindingFlags.NonPublic))
                {
                    debugInfo("m", s);
                }
                foreach (var s in thisGlobalObj.GetMembers(BindingFlags.Public | BindingFlags.Static | BindingFlags.NonPublic))
                {
                    debugInfo("ms", s);
                }

                var v = globalScope.GetObject();
            }
            _host.CreateGlobalItem(name, value, false);
        }


        private static void debugInfo(string ms, object o)
        {
            if (o==null)
            {
                Console.Error.WriteLine(ms+" =  NULL");
                return;
            }
            string ss = ms + " = " + o + " " + o.GetType();
            Console.Error.WriteLine(ss);
        }

        /// <summary>
        /// Occurs when assembly references are needed
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static void OnAssemblyReferencesNeeded(object sender, VsaScriptingHostEventArgs e)
        {
            // add the current assembly as a reference
            e.Host.CreateAssemblyReferences(Assembly.GetExecutingAssembly().Location);
        }

        /// <summary>
        /// Occurs when an error occurs during compilation or execution
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static void OnCompilerException(object sender, VsaScriptingHostCompilerExceptionEventArgs e)
        {
            Debug.WriteLine(e.Exception);
        }

        /// <summary>
        /// Occurs when global items are needed
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        static  void OnGlobalItemsNeeded(object sender, VsaScriptingHostEventArgs e)
        {
            // publish the scriptable form so that the scripts may modify it
            e.Host.CreateGlobalItem("_scriptableForm", typeof(Evaluator), false);
        }
    }
#endif


/*
 * Copyright (C) 2004 Mark Belles (Code6)
 * 
 * http://mrbelles.brinkster.net
 * http://www.codereflection.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * */

    #region VsaScriptingHostFactory

    /// <summary>
    /// A factory class used to create instances of the VsaScriptingHost class based upon a file or folder
    /// </summary>
    public class VsaScriptingHostFactory
    {
        /// <summary>
        /// Creates an array of VsaScriptingHost instances from a file or directory containing script files
        /// </summary>
        /// <param name="rootHostName"></param>
        /// <param name="rootNamespace"></param>
        /// <param name="generateDebugInfo"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public VsaScriptingHost[] Create(string rootHostName, string rootNamespace, bool generateDebugInfo, string path)
        {
            // create an array list in which to hold the newly created hosts
            ArrayList array = new ArrayList();

            // determine if path is a directory
            if (Directory.Exists(path))
            {
                // if it is, then search for any script files
                DirectoryInfo di = new DirectoryInfo(path);
                FileInfo[] files = di.GetFiles();

                // and try and create a host for each script file
                foreach (FileInfo info in files)
                {
                    VsaScriptingHost host = this.FromFileInfo(info, rootHostName, rootNamespace, generateDebugInfo);
                    if (host != null)
                    {
                        // set the hosts info
                        host.FileInfo = info;

                        array.Add(host);
                    }
                }
            }
            // otherwise the path is a file
            else if (File.Exists(path))
            {
                // so load the some file info about the path
                FileInfo info = new FileInfo(path);

                // and try and create a host from the file
                VsaScriptingHost host = this.FromFileInfo(info, rootHostName, rootNamespace, generateDebugInfo);
                if (host != null)
                {
                    // set the hosts info
                    host.FileInfo = info;

                    array.Add(host);
                }
            }

            // FileInfo
            return array.ToArray(typeof(VsaScriptingHost)) as VsaScriptingHost[];
        }

        /// <summary>
        /// Creates a VsaScriptingHost from a FileInfo object
        /// </summary>
        /// <param name="info"></param>
        /// <param name="rootHostName"></param>
        /// <param name="rootNamespace"></param>
        /// <param name="generateDebugInfo"></param>
        /// <returns></returns>
        protected VsaScriptingHost FromFileInfo(FileInfo info, string rootHostName, string rootNamespace, bool generateDebugInfo)
        {
            // determine if we can use the file based upon it's extention
            VsaScriptingLanguages language;
            if (this.SelectLanguageBasedOnExtension(info, out language))
            {
                // format the root moniker that will be used by this scripting host
                string rootMoniker = string.Format("{0}://{1}", rootHostName, Guid.NewGuid().ToString());

                // create a host
                VsaScriptingHost host = new VsaScriptingHost(language, rootHostName, rootMoniker, rootNamespace, true);

                // and return it
                return host;
            }

            return null;
        }

        /// <summary>
        /// Selects the appropriate language based upon the file extension
        /// </summary>
        /// <param name="info"></param>
        /// <param name="language"></param>
        /// <returns></returns>
        public bool SelectLanguageBasedOnExtension(FileInfo info, out VsaScriptingLanguages language)
        {
            // default the language type to none
            language = VsaScriptingLanguages.None;

            if (string.Compare(info.Extension, @".js", true) == 0)
            {
                language = VsaScriptingLanguages.JScript;
                return true;
            }
            else if (string.Compare(info.Extension, @".vb", true) == 0)
            {
                language = VsaScriptingLanguages.VBScript;
                return true;
            }

            return false;
        }
    }

    #endregion

    #region VsaScriptingHost

    /// <summary>
    /// Provides a means to execute script code using the IVsaSite and IVsaEngine interfaces
    /// </summary>
    public class VsaScriptingHost : IVsaSite, IDisposable
    {
        #region Vars

        protected bool _disposed;
        protected IVsaEngine _engine;
        protected IVsaCodeItem _codeItem;
        protected Hashtable _globalItemLookupTable;
        protected VsaScriptingLanguages _language;
        protected FileInfo _info;


        /// <summary>
        /// Returns a string array containing the names of the assemblies added as references to the engine by default.
        /// </summary>
        public static string[] DefaultAssemblyReferences = new string[] 
			{
				@"Mscorlib.dll",
				@"System.dll",
				@"System.Data.dll",
				@"System.Drawing.dll",
				@"System.Windows.Forms.dll",
				@"System.Xml.dll",
				@"Microsoft.Vsa.dll"
			};

        private string _script = null;
        string Script
        {

            get
            {

                // read the script file that created this host
                if (_script != null && _info != null)
                {
                    _script = VsaScriptingHost.ReadScriptFile(_info.FullName);
                }
                return _script;
            }
        }

        #endregion

        #region Events

        /// <summary>
        /// Occurs when the VsaScriptingHost needs Assembly references to be added
        /// </summary>
        public event ScriptingHostEventHandler AssemblyReferencesNeeded;

        /// <summary>
        /// Occurs when the VsaScriptingHost needs Global Items to be added
        /// </summary>
        public event ScriptingHostEventHandler GlobalItemsNeeded;

        /// <summary>
        /// Occurs when the engine encounters an IVsaError during compilation
        /// </summary>
        public event ScriptingHostCompilerExceptionEventHandler CompilerException;

        #endregion

        /// <summary>a
        /// Initializes a new instance of the VsaScriptingHost class
        /// </summary>
        /// <param name="language">The language used by this scripting host</param>
        /// <param name="name">The name of the engine used by the scripting host</param>
        /// <param name="rootMoniker">The root moniker of the engine used by the scripting host</param>
        /// <param name="rootNamespace">The root namespace of the engine used by the scripting host</param>
        /// <param name="generateDebugInfo">A flag that indicates whether the engine should generate debugging information</param>
        public VsaScriptingHost(VsaScriptingLanguages language, string name, string rootMoniker, string rootNamespace, bool generateDebugInfo)
        {
            // create a new hashtable for storing global items so that we can look them up as needed
            _globalItemLookupTable = new Hashtable();
            _language = language;
            // create an engine aligned to the language specified (currently only JScript and VBScript are supported)
            _engine = this.CreateLanguageSpecificEngine(language);
            _engine.RootMoniker = rootMoniker;
            SetEngine(_engine);
            SetUpEngine(name, rootMoniker, rootNamespace, generateDebugInfo);           
        }
        public VsaScriptingHost(IVsaEngine _engine0)
        {
            // create a new hashtable for storing global items so that we can look them up as needed
            _globalItemLookupTable = new Hashtable();
            SetEngine( _engine0);
            // from an engine aligned to the language specified (currently only JScript and VBScript are supported)
            GetFromEngine(_engine0);
        }

        public void SetEngine(IVsaEngine _engine0)
        {
            _engine = _engine0;
        }

        private void GetFromEngine(IVsaEngine _engine)
        {
            // set the root moniker used by the engine (ex: "MyApp://MyVsaEngine/Instance#X")
            //rootMoniker = _engine.RootMoniker;

            /*
             * fails when engine is a jscript engine...
             * wtf?
             * it works when it's a vbscript engine?
             * */
            //if (_language == VsaScriptingLanguages.VBScript)
                // set the name used by the engine (ex: "MyVsaEngine")
              //  _engine.Name = name;

            // set the site used by the engine to ourself as we are the host and will need to communicate with the engine
            //_engine.Site = this;

            // readies the engine to add new code items
            //_engine.InitNew();

            // set the root namespace used by the engine
            //_engine.RootNamespace = rootNamespace;

            // set the debug flag used by the engine
            //_engine.GenerateDebugInfo = generateDebugInfo;
        }

        private void SetUpEngine(string name, string rootMoniker, string rootNamespace, bool generateDebugInfo)
        {
            // set the root moniker used by the engine (ex: "MyApp://MyVsaEngine/Instance#X")
            if (_engine.RootMoniker == null)
            {
                _engine.RootMoniker = rootMoniker;
            }

            /*
             * fails when engine is a jscript engine...
             * wtf?
             * it works when it's a vbscript engine?
             * */
            if (_language == VsaScriptingLanguages.VBScript)
                // set the name used by the engine (ex: "MyVsaEngine")
                _engine.Name = name;

            // set the site used by the engine to ourself as we are the host and will need to communicate with the engine
            _engine.Site = this;

            // readies the engine to add new code items
            _engine.InitNew();

            // set the root namespace used by the engine
            _engine.RootNamespace = rootNamespace;

            // set the debug flag used by the engine
            _engine.GenerateDebugInfo = generateDebugInfo;

            AddType(this.GetType());
        }


        public virtual bool Execute()
        {
            string script = Script;
      
            // execute the script
            return this.Execute(script);
        }

        /// <summary>
        /// Complies and runs the specified script code
        /// </summary>
        /// <param name="script">The script code to execute</param>
        public virtual bool Execute(string script)
        {
            try
            {
                // raise the event that assembly references are needed
                this.OnAssemblyReferencesNeeded(this, new VsaScriptingHostEventArgs(this));

                // raise the event that global items are needed
                this.OnGlobalItemsNeeded(this, new VsaScriptingHostEventArgs(this));

                // create the one and only code item that will be executed by this engine
                _codeItem = this.CreateCodeItem(@"Script");

                if (script != null)
                {
                    // set the source text to the script contents
                    _codeItem.SourceText = script;

                    // compile the script
                    _engine.Compile();

                    // run the script
                    _engine.Run();
                }
                return true;
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex);
            }

            return false;
            //			// Call the VB entry point
            //			if( et == EngineType.VBScript )
            //			{
            //				// Execute ClockScript.Script.Main()
            //				Assembly   assem = _engine.Assembly;
            //				Type       type = assem.GetType("ClockScript.Script");
            //				MethodInfo method = type.GetMethod("Main");
            //				method.Invoke(null, null);
            //			}
        }

        /// <summary>
        /// Creates a new language specific engine based upon the language specified
        /// </summary>
        /// <param name="language">The language that the engine should support</param>
        /// <returns></returns>
        protected IVsaEngine CreateLanguageSpecificEngine(VsaScriptingLanguages language)
        {
            IVsaEngine engine = null;

            switch (language)
            {
                //case VsaScriptingLanguages.VBScript:
                // use the vbscript engine provided in the assembly Microsoft.VisualBasic.Vsa.dll
                // engine = new Microsoft.VisualBasic.Vsa.VsaEngine();
                // break;

                case VsaScriptingLanguages.JScript:
                    // use the jscript engine provided in the assembly Microsoft.JScript.dll
                    engine = new Microsoft.JScript.Vsa.VsaEngine();
                    break;

                default:
                    // the language specified is not supported
                    throw new VsaScriptingLanguageNotSupportedException(language);
            };

            return engine;
        }

        /// <summary>
        /// Determines if the specified item exists in the engine's item list
        /// </summary>
        /// <param name="itemName"></param>
        /// <returns></returns>
        public bool ContainsEngineItem(string itemName)
        {
            foreach (IVsaItem item in _engine.Items)
                if (string.Compare(item.Name, itemName, true) == 0)
                    return true;
            return false;
        }

        /// <summary>
        /// Returns the item from the engine's item list if it exists
        /// </summary>
        /// <param name="itemName"></param>
        /// <returns></returns>
        public IVsaItem LookupEngineItem(string itemName)
        {
            foreach (IVsaItem item in _engine.Items)
                if (string.Compare(item.Name, itemName, true) == 0)
                    return item;
            return null;
        }

        /// <summary>
        /// Asserts the the item is unique in the engine's item list. Throws an VsaItemAlreadyExistsException containing the existing item if one already exists with the specified name.
        /// </summary>
        /// <param name="itemName"></param>
        public virtual void AssertEngineItemUnique(string itemName)
        {
            if (this.ContainsEngineItem(itemName))
            {
                IVsaItem item = this.LookupEngineItem(itemName);
                throw new VsaItemAlreadyExistsException(item, itemName);
            }
        }

        /// <summary>
        /// Creates an IVsaReferenceItem item using the current IVsaEngine
        /// </summary>
        /// <param name="itemName"></param>
        /// <returns></returns>
        public virtual IVsaReferenceItem CreateReferenceItem(string itemName, string assemblyName)
        {
            Debug.Assert(itemName != null && itemName != string.Empty);

            // assert the item is unique		
            this.AssertEngineItemUnique(itemName);

            // create a new reference item
            IVsaReferenceItem item = (IVsaReferenceItem)_engine.Items.CreateItem(itemName, VsaItemType.Reference, VsaItemFlag.None);

            // set the item's assembly name
            item.AssemblyName = assemblyName;

            return item;
        }

        /// <summary>
        /// Creates an IVsaGlobalItem item using the current IVsaEngine and adds the object to the VsaScriptingHost's global item lookup table 
        /// </summary>
        /// <param name="itemName"></param>
        /// <param name="instance"></param>
        /// <returns></returns>
        public virtual IVsaGlobalItem CreateGlobalItem(string itemName, object instance, bool isAnEventSource)
        {
            Debug.Assert(itemName != null && itemName != string.Empty);
            Debug.Assert(instance != null);

            IVsaGlobalItem item = this.LookupEngineItem(itemName) as IVsaGlobalItem;

            var e = _engine;

            if (item == null)
            {
                // assert the item is unique
                this.AssertEngineItemUnique(itemName);

                // create a new global item
                item = (IVsaGlobalItem)_engine.Items.CreateItem(itemName, VsaItemType.AppGlobal, VsaItemFlag.None);

                // cash item in globalItemTable
                _globalItemLookupTable.Add(itemName, instance);
            }

            AddType(instance.GetType());
            string typeName = instance.GetType().FullName;

            // set the item's type string to the instance's type's fullname
            item.TypeString = typeName;
            item.ExposeMembers = true;
            // only VBScript supports event sources
            if (_language == VsaScriptingLanguages.VBScript)
                // now determine if this item is supposed to be an event source
                if (isAnEventSource)
                    // add the item as an event source
                    _codeItem.AddEventSource(itemName, typeName);
            
            
            return item;
        }

        static HashSet<String> AssembliesKnown = new HashSet<string>();
        public void AddType(Type type)
        {
            //Add a reference to the ColorPicker assembly
            string dllName = type.Assembly.ManifestModule.Name;
            if (AlreadyAdded(dllName)) return;
            var refItem =
                (IVsaReferenceItem)_engine.Items.CreateItem(
                                       dllName, VsaItemType.Reference, VsaItemFlag.None);
            refItem.AssemblyName = type.Assembly.Location;
        }

        private bool AlreadyAdded(string dllName)
        {
            lock (AssembliesKnown)
            {
                if (AssembliesKnown.Contains(dllName)) return true;
                AssembliesKnown.Add(dllName);
                return false;

            }
        }

        /// <summary>
        /// Creates an IVsaCodeItem using the current IVsaEngine
        /// </summary>
        /// <param name="itemName"></param>
        /// <returns></returns>
        public virtual IVsaCodeItem CreateCodeItem(string itemName)
        {
            Debug.Assert(itemName != null && itemName != string.Empty);

            // assert the item is unique
            this.AssertEngineItemUnique(itemName);

            // add a code item to the engine
            IVsaCodeItem item = (IVsaCodeItem)_engine.Items.CreateItem(itemName, VsaItemType.Code, VsaItemFlag.None);

            return item;
        }

        /// <summary>
        /// Creates a IVsaReferenceItem from the specified Assembly names using the current IVsaEngine
        /// </summary>
        /// <param name="assemblyNames"></param>
        /// <returns></returns>
        public virtual IVsaReferenceItem[] CreateAssemblyReferences(params string[] assemblyNames)
        {
            // confirm we have a valid list of items to reference
            Debug.Assert(assemblyNames != null);

            // create an array list of items referenced
            ArrayList items = new ArrayList();

            // create a reference for each assembly name
            foreach (string assemblyName in assemblyNames)
            {
                // create a reference item
                IVsaReferenceItem item = this.CreateReferenceItem(assemblyName, assemblyName);

                //				// set the assembly name
                //				item.AssemblyName = assemblyName;

                // add the item to the list
                items.Add(item);
            }

            // return the list of items as reference items
            return items.ToArray(typeof(IVsaReferenceItem)) as IVsaReferenceItem[];
        }

        #region My Public Properties

        /// <summary>
        /// Gets or sets the file that will be run as a script when the host is executed
        /// </summary>
        public FileInfo FileInfo
        {
            get
            {
                return _info;
            }
            set
            {
                _info = value;
            }
        }

        /// <summary>
        /// Returns the IVsaEngine interface used by this VsaScriptingHost instance
        /// </summary>
        public virtual IVsaEngine VsaEngine
        {
            get
            {
                return _engine;
            }
        }

        /// <summary>
        /// Returns the IVsaSite interface used by this VsaScriptingHost instance
        /// </summary>
        public virtual IVsaSite VsaSite
        {
            get
            {
                return this;
            }
        }

        public IVsaEngine Engine
        {
            get { return _engine;  }
        }

        #endregion

        /// <summary>
        /// Adds our default assembly references and raises the AssemblyReferencesNeeded event
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        protected virtual void OnAssemblyReferencesNeeded(object sender, VsaScriptingHostEventArgs e)
        {
            try
            {
                /*
                 * NOTE: By default the assemblies reference by this dll are added.
                 * 
                 * create the default assembly references from the scripting host itself
                 * this will cover most of the references that are commonly needed
                 * if other assemblies are needed, responding to this event will provide the 
                 * opportunity to add them
                 * 
                 * */
                this.CreateAssemblyReferences(VsaScriptingHost.DefaultAssemblyReferences);

                // always reference the current scripting host assembly
                this.CreateAssemblyReferences(Assembly.GetExecutingAssembly().Location);

                // raise the event 
                if (this.AssemblyReferencesNeeded != null)
                    this.AssemblyReferencesNeeded(sender, e);
            }
            catch (Exception ex)
            {
                Trace.WriteLine(ex);
            }
        }

        /// <summary>
        /// Adds this instance of the VsaScriptingHost class as a global item named "VsaScriptingHost" and raises the GlobalItemsNeeded
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        protected virtual void OnGlobalItemsNeeded(object sender, VsaScriptingHostEventArgs e)
        {
            try
            {
                // add the scripting host as a global item so that all scripts will have access to the VsaScriptingHost instance
                this.CreateGlobalItem(@"_scriptingHost", this, false);

                // raise the event
                if (this.GlobalItemsNeeded != null)
                    this.GlobalItemsNeeded(sender, e);
            }
            catch (Exception ex)
            {
                Trace.WriteLine(ex);
            }
        }

        /// <summary>
        /// Raises the CompilerException event
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        protected virtual void OnCompilerException(object sender, VsaScriptingHostCompilerExceptionEventArgs e)
        {
            try
            {
                if (this.CompilerException != null)
                    this.CompilerException(sender, e);
            }
            catch (Exception ex)
            {
                Trace.WriteLine(ex);
            }
        }

        #region IVsaSite Members

        public object GetEventSourceInstance(string itemName, string eventSourceName)
        {
            foreach (DictionaryEntry entry in _globalItemLookupTable)
                if (string.Compare(entry.Key.ToString(), eventSourceName, true) == 0)
                    return entry.Value;
            return null;
        }

        /// <summary>
        /// When the engine calls back the IVsaSite to ask for a global item, return the instance if we've cached it previously
        /// </summary>
        /// <param name="name">The name of the global item to which an object instance is requested</param>
        /// <returns></returns>
        public object GetGlobalInstance(string name)
        {
            // enumerate the items in our global item lookup table
            foreach (DictionaryEntry entry in _globalItemLookupTable)
                // carefully comparing the item names to the name of the item requested by the engine
                if (string.Compare(entry.Key.ToString(), name, true) == 0)
                    // and if we find it, return the object instance to the engine
                    return entry.Value;
            // and finally if we couldn't find any instance for that name, just don't worry about it
            return null;
        }

        public void Notify(string notify, object info)
        {
            // TODO:  Add ScriptEngine.Notify implementation
        }

        public bool OnCompilerError(IVsaError error)
        {
            this.OnCompilerException(this, new VsaScriptingHostCompilerExceptionEventArgs(error));
            return true;
        }

        public void GetCompiledState(out byte[] pe, out byte[] debugInfo)
        {
            // TODO:  Add ScriptEngine.GetCompiledState implementation
            pe = null;
            debugInfo = null;
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            this.Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (!_disposed)
            {
                if (_engine != null)
                    _engine.Close();

                _disposed = true;
            }
        }

        #endregion

        /// <summary>
        /// A utility function to read the contents of a file as text (Preferrably used upon a script file)
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static string ReadScriptFile(string path)
        {
            string script = null;
            using (StreamReader reader = new StreamReader(path))
            {
                script = reader.ReadToEnd();
                reader.Close();
            }
            return script;
        }
    }

    #endregion

    #region VsaScriptingHostEventArgs

    /// <summary>
    /// Summary description for VsaScriptingHostEventArgs.
    /// </summary>
    public class VsaScriptingHostEventArgs : EventArgs
    {
        protected VsaScriptingHost _host;

        /// <summary>
        /// Initializes a new instance of the VsaScriptingHostEventArgs class
        /// </summary>
        /// <param name="host"></param>
        public VsaScriptingHostEventArgs(VsaScriptingHost host)
        {
            _host = host;
        }

        /// <summary>
        /// Returns the VsaScriptingHost instance centering around this event
        /// </summary>
        public VsaScriptingHost Host
        {
            get
            {
                return _host;
            }
        }
    }

    public delegate void ScriptingHostEventHandler(object sender, VsaScriptingHostEventArgs e);

    #endregion

    #region VsaScriptingLanguages

    /// <summary>
    /// The various scripting languages supported by the VsaScriptingHost class
    /// </summary>
    public enum VsaScriptingLanguages
    {
        None,

        /// <summary>
        /// Specifies that the language of the script to be executed by the VsaScriptingHost class is VBScript
        /// </summary>
        VBScript,

        /// <summary>
        /// JScript is currently not supported by the current version of the VsaScriptingHost class (NOTE: Use VBScript instead.)
        /// </summary>
        JScript
    }

    #endregion

    #region VsaScriptingLanguageNotSupportedException

    /// <summary>
    /// Summary description for VsaScriptingLanguageNotSupportedException.
    /// </summary>
    public class VsaScriptingLanguageNotSupportedException : Exception
    {
        protected VsaScriptingLanguages _language;

        /// <summary>
        /// Initializes a new instance of the VsaScriptingLanguageNotSupportedException class
        /// </summary>
        /// <param name="language"></param>
        public VsaScriptingLanguageNotSupportedException(VsaScriptingLanguages language)
            : base(string.Format("The language '{0}' is not supported.", language.ToString()))
        {
            _language = language;
        }

        public VsaScriptingLanguages Language
        {
            get
            {
                return _language;
            }
        }
    }

    #endregion

    #region VsaItemAlreadyExistsException

    /// <summary>
    /// Summary description for VsaItemAlreadyExistsException.
    /// </summary>
    public class VsaItemAlreadyExistsException : Exception
    {
        protected IVsaItem _item;

        public VsaItemAlreadyExistsException(IVsaItem item, string itemName)
            : base(string.Format("An item with the name '{0}' already exists.", itemName))
        {
            _item = item;
        }

        public IVsaItem ExistingItem
        {
            get
            {
                return _item;
            }
        }
    }

    #endregion

    #region VsaScriptingHostCompilerExceptionEventArgs

    /// <summary>
    /// Summary description for VsaScriptingHostCompilerExceptionEventArgs.
    /// </summary>
    public class VsaScriptingHostCompilerExceptionEventArgs : EventArgs
    {
        protected IVsaError _exception;

        public VsaScriptingHostCompilerExceptionEventArgs(IVsaError exception)
            : base()
        {
            _exception = exception;
        }

        public IVsaError Exception
        {
            get
            {
                return _exception;
            }
        }
    }

    public delegate void ScriptingHostCompilerExceptionEventHandler(object sender, VsaScriptingHostCompilerExceptionEventArgs e);

    #endregion

    /// <summary>
    /// Compiles the given expression into a JScript function at time of 
    /// construction and then simply invokes it during evaluation, using
    /// the context as a parameter.
    /// </summary>

    internal sealed class FullTrustEvaluationStrategy : EvaluationStrategy
    {
        private readonly object _function;

        public FullTrustEvaluationStrategy(string expression, VsaEngine engine)
            : base(engine)
        {
            //
            // Equivalent to following in JScript:
            // new Function('$context', 'with ($context) return (' + expression + ')');
            //
            // IMPORTANT! Leave the closing parentheses surrounding the 
            // return expression on a separate line. This is to guard
            // against someone using a double-slash (//) to comment out 
            // the remainder of an expression.
            //

            const string context = "$context";
            object Function = null;
            try
            {
                LenientGlobalObject engineLenientGlobalObject = engine.LenientGlobalObject;
                if (engineLenientGlobalObject != null)
                {
                    Function = engineLenientGlobalObject.Function;
                }
            }
            catch (Exception e)
            {
                Console.Error.WriteLine("" + e);
            }
            if (Function == null) Function = Microsoft.JScript.GlobalObject.Function;
            if (expression.EndsWith(";")) expression = expression.TrimEnd(";".ToCharArray());
            string s = @"
                        with (" + context + @") {
                            return (
                                " + expression + @"
                            );
                        }";
            try
            {
                _function = LateBinding.CallValue(DefaultThisObject(engine), Function,
                    new object[] {                        /* parameters */ context, /* body... */s },
                    /* construct */ true, /* brackets */ false, engine);

            }
            catch (Exception e)
            {
                _function = LateBinding.CallValue(DefaultThisObject(engine), Function,
    
                    new object[] {                        /* parameters */ context, /* body... */s },
                    /* construct */ true, /* brackets */ false, engine);

            } var tnpe = _function.GetType();
        }

        public override object Eval(object context)
        {
            //
            // Following is equivalent to calling apply in JScript.
            // See http://msdn.microsoft.com/en-us/library/84ht5z59.aspx.
            //

            object result = LateBinding.CallValue(
                DefaultThisObject(Engine),
                _function, /* args */ new object[] { context },
                /* construct */ false, /* brackets */ false, Engine);

            return result;
        }

        private static object DefaultThisObject(VsaEngine engine)
        {
            Debug.Assert(engine != null);
            return ((IActivationObject)engine.ScriptObjectStackTop()).GetDefaultThisObject();
        }

        public static bool IsApplicable()
        {
            try
            {
                //
                // FullTrustEvaluationStrategy uses Microsoft.JScript.GlobalObject.Function.CreateInstance,
                // which requires unmanaged code permission...
                //

                new SecurityPermission(SecurityPermissionFlag.UnmanagedCode).Demand();
                return true;
            }
            catch (SecurityException)
            {
                return false;
            }
        }
    }

    /// <summary>
    /// Uses the JScript eval function to compile and evaluate the
    /// expression against the context on each evaluation.
    /// </summary>

    internal sealed class PartialTrustEvaluationStrategy : EvaluationStrategy
    {
        private readonly string _expression;
        private readonly GlobalScope _scope;
        private readonly FieldInfo _contextField;

        public PartialTrustEvaluationStrategy(string expression, VsaEngine engine)
            : base(engine)
        {
            //
            // Following is equivalent to declaring a "var" in JScript
            // at the level of the Global object.
            //

            _scope = (GlobalScope)engine.GetGlobalScope().GetObject();
            _contextField = _scope.AddField("$context");
            var tnpe = _contextField.Name;
            _expression = expression;
        }

        public override object Eval(object context)
        {
            VsaEngine engine = Engine;

            //
            // Following is equivalent to calling eval in JScript,
            // with the value of the context variable established at the
            // global scope in order for it to be available to the 
            // expression source.
            //

            _contextField.SetValue(_scope, context);

            try
            {
                With.JScriptWith(context, engine);
                return Microsoft.JScript.Eval.JScriptEvaluate(_expression, engine);
            }
            finally
            {
                engine.PopScriptObject(/* with */);
            }
        }
    }

    internal abstract class EvaluationStrategy
    {
        private readonly VsaEngine _engine;

        protected EvaluationStrategy(VsaEngine engine)
        {
            Debug.Assert(engine != null);
            _engine = engine;
        }

        public VsaEngine Engine { get { return _engine; } }
        public abstract object Eval(object context);
    }

}
