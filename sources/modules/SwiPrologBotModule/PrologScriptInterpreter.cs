using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using Cogbot;
using Cogbot.ScriptEngines;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using SbsSW.SwiPlCs;
using Swicli.Library;

namespace PrologScriptEngine
{
    public class BotVarProvider : ICollectionProvider
    {
        public static BotVarProvider CreateBotVarProvider(PlTerm bot, PlTerm nameSpace, PlTerm getter, PlTerm setter, PlTerm keyGetter)
        {
            BotVarProvider provider = new BotVarProvider("user", nameSpace.Name, getter.Name, setter.Name, keyGetter.Name);
            ScriptManager.AddGroupProvider((ICollectionRequester) PrologCLR.GetInstance(bot), provider);
            return provider;
        }

        private PlQuery AllCallbacks;
        public static bool DiscardFrames;
        private string Getter;
        private string Setter;
        private string KeyGetter;
        private string Module;

        public BotVarProvider(string module, string nameSpace, string getter, string setter, string keyGetter)
        {
            Module = module;
            NameSpace = nameSpace;
            Getter = getter;
            Setter = setter;
            KeyGetter = keyGetter;    
        }

        #region Implementation of ITreeable

        public string NameSpace { get; set;}

        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            return PrologCLR.InvokeFromC(
                () =>
                    {
                        if (IsEmpty(KeyGetter)) return null;
                        List<string> names = new List<string>();
                        var plVar = PlTerm.PlVar();
                        var query = new PlQuery(Module, KeyGetter,
                                                new PlTermV(PrologCLR.ToProlog(requester), PlTerm.PlString(NameSpace),
                                                            plVar));
                        while (query.NextSolution())
                        {
                            string res = (string) query.Args[2];
                            if (!names.Contains(res)) names.Add(res);
                        }
                        return names.ToArray();
                    }, DiscardFrames);
        }        

        #endregion

        #region Implementation of ICollectionProviderSettable

        public void SetValue(ICollectionRequester requester, string name, object value)
        {
            PrologCLR.InvokeFromC(
                () =>
                    {
                        if (IsEmpty(Setter)) return false;
                        var plVar = PlTerm.PlVar();
                        var query = new PlQuery(Module, Setter,
                                                new PlTermV(PrologCLR.ToProlog(requester), PlTerm.PlString(NameSpace), PlTerm.PlString(name),
                                                            PrologCLR.ToProlog(value)));
                        while (query.NextSolution())
                        {
                            
                        }
                        return true;
                    }, DiscardFrames);
        }

        private static bool IsEmpty(string callback)
        {
            return string.IsNullOrEmpty(callback);
        }

        public bool AcceptsNewKeys
        {
            get { return true; }
        }

        public ICollection GetGroup(ICollectionRequester requester, string name)
        {
            return PrologCLR.InvokeFromC(
                () =>
                    {
                        if (IsEmpty(Getter)) return null;
                        var plVar = PlTerm.PlVar();
                        List<object> results = new List<object>();
                        var query = new PlQuery(Module, Getter,
                                                new PlTermV(PrologCLR.ToProlog(requester), PlTerm.PlString(NameSpace), PlTerm.PlString(name),
                                                            plVar));
                        while (query.NextSolution())
                        {
                            object res = PrologCLR.GetInstance(query.Args[3]);
                            if (!results.Contains(res)) results.Add(res);
                        }
                        return results.Count == 0 ? null : results;
                    }, DiscardFrames);
        }

        #endregion
    }
    public class BotVarProviderCallN : ICollectionProvider
    {
        public static ICollectionProvider CreateBotVarProvider(PlTerm bot, PlTerm nameSpace, PlTerm getter, PlTerm setter, PlTerm keyGetter)
        {
            var provider = new BotVarProviderCallN(nameSpace, getter, setter, keyGetter);
            ScriptManager.AddGroupProvider((ICollectionRequester) PrologCLR.GetInstance(bot), provider);
            return provider;
        }

        private PlQuery AllCallbacks;
        public static bool DiscardFrames;

        public BotVarProviderCallN(PlTerm nameSpace, PlTerm getter, PlTerm setter, PlTerm keyGetter)
        {

            uint fid = libpl.PL_open_foreign_frame();
            NameSpace = nameSpace.Name;
            AllCallbacks = new PlQuery("call", new PlTermV(getter, setter, keyGetter));
            AllCallbacks.EnsureQUID();
            GCHandle.Alloc(AllCallbacks);
            GCHandle.Alloc(this);
        }

        #region Implementation of ITreeable

        public string NameSpace { get; set; }

        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            return PrologCLR.InvokeFromC(
                () =>
                {
                    PlTerm callback = AllCallbacks.Args[2];
                    List<string> names = new List<string>();
                    if (IsEmpty(callback)) return null;
                    var plVar = PlTerm.PlVar();
                    var query = new PlQuery("call",
                                            new PlTermV(callback,
                                                        PlTerm.PlString(NameSpace),
                                                        plVar));
                    while (query.NextSolution())
                    {
                        string res = (string)query.Args[2];
                        if (!names.Contains(res)) names.Add(res);
                    }
                    return names.ToArray();
                }, DiscardFrames);
        }

        #endregion

        #region Implementation of ICollectionProviderSettable

        public void SetValue(ICollectionRequester requester, string name, object value)
        {
            PrologCLR.InvokeFromC(
                () =>
                {
                    PlTerm callback = AllCallbacks.Args[1];
                    if (IsEmpty(callback)) return false;
                    var plVar = PlTerm.PlVar();
                    var query = new PlQuery("call",
                                            new PlTermV(callback, PlTerm.PlString(NameSpace), PlTerm.PlString(name),
                                                        PrologCLR.ToProlog(value)));
                    return query.NextSolution();
                }, DiscardFrames);
        }

        private static bool IsEmpty(PlTerm callback)
        {
            return callback.IsVar || callback.Name == "@";
        }

        public bool AcceptsNewKeys
        {
            get { return true; }
        }

        public ICollection GetGroup(ICollectionRequester requester, string name)
        {
            return PrologCLR.InvokeFromC(
                () =>
                {
                    PlTerm callback = AllCallbacks.Args[0];
                    if (IsEmpty(callback)) return null;
                    var plVar = PlTerm.PlVar();
                    List<object> results = new List<object>();
                    var query = new PlQuery("call",
                                            new PlTermV(callback, PlTerm.PlString(NameSpace), PlTerm.PlString(name),
                                                        plVar));
                    while (query.NextSolution())
                    {
                        object res = PrologCLR.GetInstance(query.Args[3]);
                        if (!results.Contains(res)) results.Add(res);
                    }
                    return results.Count == 0 ? null : results;
                }, DiscardFrames);
        }

        #endregion
    }
    public class PrologScriptInterpreter : CommonScriptInterpreter, ScriptInterpreter, ScriptInterpreterFactory
    {
        public static bool AutoInternMethods = false;
        static public void Main(string[] args)
        {
            _reachAllTypes = 2;
            PrologScriptInterpreter pscript = new PrologScriptInterpreter(null);
            pscript.Init(pscript);
            pscript.Intern("pscript", pscript);
            PrologCLR.Main0(args);
        }
        ///<summary>
        ///</summary>
        public PrologCLR prologClient;

        public override ScriptInterpreter GetLoaderOfFiletype(string filenameorext)
        {
            return LoadsFileType(filenameorext) ? this : null;
        }

        public override bool LoadsFileType(string filename)
        {
            return filename.EndsWith("pl") || filename.EndsWith("swi") || filename.EndsWith("prolog") ||
                   base.LoadsFileType(filename);
        }
        static HashSet<Type> _types = new HashSet<Type>();
        private static int _reachAllTypes = 1;

        ///<summary>
        ///</summary>
        ///<param name="type"></param>
        ///<exception cref="NotImplementedException"></exception>
        public override void InternType(Type type)
        {
            InternTypeS(type, _reachAllTypes);
        }

        override public object Impl
        {
            get { return this; }
        }

        static void InternTypeS(Type type, int depth)
        {
            if (depth == 0) return;
            depth--;
            bool deeper = depth != 0;
            {
                lock (_types) if (!_types.Add(type)) return;                          
                string module = type.Namespace;
                foreach (var list0 in type.GetMethods(BindingFlags.Static | BindingFlags.NonPublic))
                {
                    var list = list0;
                    DLRConsole.SafelyRun(() =>
                    {
                        if (AutoInternMethods) PrologCLR.InternMethod(module, PrologCLR.ToPrologCase(list.Name), list);
                        if (deeper)
                        {
                            foreach (var s in list.GetParameters())
                            {
                                InternTypeS(s.ParameterType, depth);
                            }
                            InternTypeS(list.ReturnType, depth);
                        }
                    });
                }
            }
        }

        public override void Dispose()
        {
            prologClient.Dispose();
        }

        public override object GetSymbol(string eventName)
        {
            return PrologCLR.InvokeFromC(() => prologClient.GetSymbol(eventName), false);
        }

        public override bool IsSubscriberOf(string eventName)
        {
            return PrologCLR.InvokeFromC(() => prologClient.IsDefined(eventName), false);
        }

        public PrologScriptInterpreter(object self)
            : base()
        {
            Init(self);
        }

        static private readonly object GroupInitLock = new object();
        public static bool IsInited = false;

        public override sealed void Init(object self)
        {
            lock (GroupInitLock)
            {
                prologClient = prologClient ?? new PrologCLR();
                if (!IsInited)
                {
                    IsInited = true;
                    prologClient.InitFromUser();
                }
            }
        }
        /// <summary>
        /// 
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public override bool LoadFile(string filename, OutputDelegate WriteLine)
        {
            try
            {
                return prologClient.Consult(filename);
            }
            catch (Exception e)
            {
                WriteLine("CSERROR: " + e);
                return false;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context_name"></param>
        /// <param name="stringCodeReader"></param>
        /// <returns></returns>
        public override object Read(string context_name, System.IO.TextReader stringCodeReader, OutputDelegate WriteLine)
        {
            object res = null;
            int line = 0;
            while (stringCodeReader.Peek() != -1)
            {
                line++;
                TextWriter tw = new OutputDelegateWriter(WriteLine);
                res = prologClient.Read(stringCodeReader.ReadLine(), tw);
            }
            return res;
        } // method: Read


        /// <summary>
        /// 
        /// </summary>
        /// <param name="codeTree"></param>
        /// <returns></returns>
        public override bool Eof(object codeTree)
        {
            if (codeTree == null) return true;
            String str = codeTree.ToString().Trim();
            return String.IsNullOrEmpty((String)codeTree);
        } // method: Eof


        /// <summary>
        /// 
        /// </summary>
        /// <param name="varname"></param>
        /// <param name="textForm"></param>
        public override void Intern(string varname, object value)
        {
            if (value != null)
            {
                InternTypeS(value.GetType(),1);
            }
            prologClient.Intern(varname, value);
        } // method: Intern


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public override object Eval(object code)
        {
            return prologClient.Eval(code);
        } // method: Eval


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public override string Str(object code)
        {
            return ScriptEventListener.argString(code);
        } // method: Str


        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public override ScriptInterpreter newInterpreter(object thiz)
        {
            PrologScriptInterpreter si;
            if (prologClient == null || prologClient == thiz) si = this;
            else
                si = new PrologScriptInterpreter(thiz);
            //si.prologClient = thiz as PrologClient;
            return si;
        } // method: newInterpreter
    }
}
