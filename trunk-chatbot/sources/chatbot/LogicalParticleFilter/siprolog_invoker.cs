#define MERGED_RDFSTORE
using System;
using System.Collections.Generic;
using System.Collections;
using System.Data;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.Reflection;
using Mono.CSharp;
using MushDLR223.Utilities;
using VDS.RDF;
using VDS.RDF.Parsing;
using VDS.RDF.Query;
using VDS.RDF.Writing.Formatting;
using VDS.RDF.Writing;
using VDS.RDF.Nodes;
using StringWriter = System.IO.StringWriter;
//using TermList = LogicalParticleFilter1.TermListImpl;
//using TermList = System.Collections.Generic.List<LogicalParticleFilter1.SIProlog.Part>;///LogicalParticleFilter1.SIProlog.PartListImpl;
//using PartList = System.Collections.Generic.List<LogicalParticleFilter1.SIProlog.Part>;///LogicalParticleFilter1.SIProlog.PartListImpl;

using TermList = LogicalParticleFilter1.SIProlog.PartListImpl;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;
#if MERGED_RDFSTORE
using GraphWithDef = LogicalParticleFilter1.SIProlog.PNode;
#endif

using System.Threading;
//using GraphWithDef = LogicalParticleFilter1.SIProlog.;
//using ProveResult = LogicalParticleFilter1.SIProlog.PEnv;

namespace LogicalParticleFilter1
{
    public partial class SIProlog
    {
        private readonly CIDictionary<string, List<object>> NameToObjects = new CIDictionary<string, List<object>>();

        public void RegisterObject(string name, object objWithMethods)
        {
            List<object> objs;
            lock (NameToObjects)
            {
                if (!NameToObjects.TryGetValue(name, out objs))
                {
                    objs = NameToObjects[name] = new List<object>();
                }
            }
            lock (objs)
            {
                objs.Remove(objWithMethods);
                objs.Insert(0, objWithMethods);
            }
        }

        public bool UnregisterObject(string name, object objWithMethods)
        {
            List<object> objs;
            lock (NameToObjects)
            {
                if (!NameToObjects.TryGetValue(name, out objs) || objs.Count == 0)
                {
                    return false;
                }
            }
            lock (objs)
            {
                return objs.Remove(objWithMethods);
            }
        }

        public bool CanInvokeObjectByName(string name, string method, PartList methodArgs, bool searchDownMembers, out object result, TextWriter helpfulWarningsOrNull)
        {
            List<object> objs;
            lock (NameToObjects)
            {
                if (!NameToObjects.TryGetValue(name, out objs) || objs.Count == 0)
                {
                    throw new NotSupportedException("no object named " + name);
                }
            }
            object[] os;
            lock (objs)
            {
                os = objs.ToArray();
            }
            foreach (object o in os)
            {
                try
                {
                    if (CanInvokeObject(o, method, methodArgs, out result, helpfulWarningsOrNull))
                    {
                        return true;
                    }
                }
                catch (NotSupportedException nse)
                {
                    continue;
                }
            }
            if (searchDownMembers)
            {
                HashSet<object> tryPlaces = new HashSet<object>();
                foreach (object o in os)
                {
                    foreach (
                        var membs in
                            o.GetType().GetFields(BindingFlags.NonPublic | BindingFlags.NonPublic | BindingFlags.Static |
                                                  BindingFlags.Instance))
                    {
                        object io = membs.IsStatic ? null : o;
                        var tat = membs.GetValue(io);
                        if (tat is IConvertible || tat is ValueType || tat == null) continue;
                        tryPlaces.Add(tat);
                    }
                    if (false)
                    {
                        foreach (
                            var membs in
                                o.GetType().GetProperties(BindingFlags.NonPublic | BindingFlags.NonPublic |
                                                          BindingFlags.Static | BindingFlags.Instance))
                        {
                            var ggm = membs.GetGetMethod();
                            if (ggm == null) continue;
                            object io = ggm.IsStatic ? null : o;
                            var tat = ggm.Invoke(io, null);
                            if (tat is IConvertible || tat is ValueType || tat == null) continue;
                            tryPlaces.Add(tat);
                        }
                    }
                }
                foreach (object o in os)
                {
                    tryPlaces.Remove(o);
                }
                foreach (object o in tryPlaces)
                {
                    if (CanInvokeObject(o, method, methodArgs, out result, helpfulWarningsOrNull))
                    {
                        return true;
                    }
                }
            }
            if (helpfulWarningsOrNull != null)
            {
                helpfulWarningsOrNull.WriteLine("no such method '" + method + "' on an object named '" + name + "'");
            }
            result = null;
            return false;
        }

        public bool CanInvokeObject(object o, string method, PartList methodArgs, out object result, TextWriter helpfulWarnings)
        {
            object[] invokeargs = new object[methodArgs.Count];
            Type t = o.GetType();
            string extraInfo = "";
            foreach (var s in
                t.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance |
                             BindingFlags.Static))
            {
                if (s.Name.ToLower() == method.ToLower())
                {
                    var ps = s.GetParameters();
                    bool mismatch = false;
                    if (ps.Length == methodArgs.Count)
                    {
                        extraInfo = " except as " + s;
                        for (int i = 0; i < ps.Length; i++)
                        {
                            ParameterInfo parameterInfo = ps[i];
                            var pt = parameterInfo.ParameterType;
                            if (invokeargs[i] != null)
                            {
                                if (pt.IsInstanceOfType(invokeargs[i])) continue;
                            }
                            object op;
                            if (!ConvertPart(methodArgs[i], pt, out op))
                            {
                                mismatch = true;
                                break;
                            }
                            invokeargs[i] = op;
                        }
                    } else
                    {
                        extraInfo = " except with " + s;
                    }
                    if (!mismatch)
                    {
                        object io = s.IsStatic ? null : o;
                        result = s.Invoke(io, invokeargs);
                        if (result == null && s.ReturnType == typeof(void))
                        {
                            result = Atom.PrologNIL;
                        }     
                        return true;
                    }
                }

            }
            result = null;
            if (helpfulWarnings != null && extraInfo != "")
            {
                helpfulWarnings.WriteLine(extraInfo);
            }
            return false;
        }

        private bool ConvertPart(Part arg, Type pt, out object op)
        {
            if (pt == typeof (string))
            {
                op = arg.ToSource(SourceLanguage.Text);
                return true;
            }
            if (arg is Atom)
            {
                op = ((Atom) arg).Functor0;
                return pt.IsInstanceOfType(op) || op == null;
            }
            op = null;
            return false;
        }


        public ProveResult ExecuteSharp(Term thisTerm, PartListImpl goalList, PEnv environment, PDB db, int level,
                                        reportDelegate reportFunction)
        {
            Term collect0 = value((Part) thisTerm.ArgList[1], environment).AsTerm();
            string methodName = collect0.fname;
            var partObj = value((Part)thisTerm.ArgList[0], environment);
            object result;
            TextWriter warns = new StringWriter();
            try
            {
                if (partObj.IsObject)
                {
                    if (!CanInvokeObject(partObj.Functor0, methodName, collect0.ArgList, out result, warns))
                    {
                        Warn(warns);
                        return new ProveResult() { Failed = true };
                    }
                }
                else
                {
                    if (!CanInvokeObjectByName(partObj.Text, methodName, collect0.ArgList, true, out result, warns))
                    {
                        Warn(warns);
                        return new ProveResult() { Failed = true };
                    }
                }
            }
            catch (Exception e2)
            {
                Warn(warns);
                throw e2;
                return new ProveResult() { Failed = true };
            }
            //print("Debug: unifying "); into.print(); print(" with "); answers.print(); print("\n");
            var env2 = unify(thisTerm.ArgList[2], ObjectToPart(result), environment);

            if (env2 == null)
            {
                //print("Debug: bagof cannot unify anslist with "); into.print(); print(", failing\n");
                return null;
            }

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, env2, db, level + 1, reportFunction);
        }

        public Part ObjectToPart(object o)
        {
            if (o is Part) return (Part)o;
            if (o == null) return Atom.PrologNIL;
            if (o is string)
            {
                return Atom.MakeString((string) o);
            }
            return Atom.FromSource(o.ToString());
        }
    }
}
