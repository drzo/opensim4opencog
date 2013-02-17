using System;
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLParser;
using MushDLR223.ScriptEngines;

namespace AltAIMLbot.Utils
{

    public interface XmlNodeEvaluator
    {
        IEnumerable<XmlNodeEval> GetEvaluators(XmlNode node);
    }

    abstract public class XmlNodeEvaluatorImpl : StaticAIMLUtils , XmlNodeEvaluator
    {
        public readonly static IEnumerable<XmlNodeEval> NO_XmlNodeEval = new XmlNodeEval[0];
        public readonly static IEnumerable<XmlNode> NO_XmlNode = new XmlNode[0];

        protected Type ReflectionType;
        protected string prefixE = "Eval_";
        protected string typesufix = "_NodeType";

        protected XmlNodeEvaluatorImpl()
        {
            ReflectionType = GetType(); 
        }

        protected XmlNodeEvaluatorImpl(string prefix0, string suffix)
            : this()
        {
            this.prefixE = prefix0;
            this.typesufix = suffix;
        }

        public virtual IEnumerable<XmlNodeEval> GetEvaluatorsFromReflection(XmlNode node)
        {
            MethodInfo example = typeof (XmlNodeEval).GetMethod("Invoke");
            Type type = GetType();
            Type[] parameters = null;
            MethodInfo mi = GetMethodForNode(node);
            var v = DelegateFromMethod(mi);
            if (v != null && v != DoNothing)
                return new[] {v};

            List<XmlNodeEval> list = new List<XmlNodeEval>();
            foreach (XmlNode xmlNode in node)
            {
                var vs = GetEvaluators(xmlNode);
                if (vs != NO_XmlNodeEval)
                {
                    foreach (var eval in vs)
                    {
                        list.Add(eval);
                    }
                }
            }
            if (list.Count == 0) return NO_XmlNodeEval;
            return list;
        }

        public virtual MethodInfo GetMethodForNode(XmlNode node)
        {
            MethodInfo mi = ReflectionType.GetMethod(prefixE + node.Name);
            if (mi != null) return mi;
            mi = ReflectionType.GetMethod(prefixE + node.NodeType.ToString() + typesufix);
            return mi;
        }

        public virtual XmlNodeEval DelegateFromMethod(MethodInfo mi)
        {
            XmlNodeEval xmlne = DoNothing;
            if (mi != null)
            {
                Object ctx = mi.IsStatic ? null : this;

                try
                {
                    xmlne = (XmlNodeEval)Delegate.CreateDelegate(typeof(XmlNodeEval), ctx, mi, true);
                    return xmlne;
                }
                catch (Exception ex)
                {
                    xmlne =
                        new XmlNodeEval(
                            (src, request0, output)
                            =>
                            (IEnumerable<XmlNode>) mi.Invoke(ctx, new object[] {src, request0, output}));
                    return xmlne;
                }
            }
            return xmlne;
        }

        public virtual IEnumerable<XmlNode> DoNothing(XmlNode src, Request request, OutputDelegate outputdelegate)
        {
            return NO_XmlNode;
        }

        #region Implementation of XmlNodeEvaluator

        public abstract IEnumerable<XmlNodeEval> GetEvaluators(XmlNode node);

        #endregion
    }
}