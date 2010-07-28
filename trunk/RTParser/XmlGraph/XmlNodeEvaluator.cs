using System;
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using MushDLR223.ScriptEngines;

namespace RTParser.Utils
{
    abstract public class XmlNodeEvaluator
    {
        protected Type ReflectionType;
        protected string prefix = "Eval_";
        protected string typesufix = "_NodeType";

        protected XmlNodeEvaluator()
        {
            ReflectionType = GetType(); 
        }

        protected XmlNodeEvaluator(string prefix, string suffix)
            : this()
        {
            this.prefix = prefix;
            this.typesufix = suffix;
        }

        public virtual IEnumerable<XmlNodeEval> GetEvaluators(XmlNode node)
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
                list.AddRange(GetEvaluators(xmlNode));
            }
            return list;
        }

        public virtual MethodInfo GetMethodForNode(XmlNode node)
        {
            MethodInfo mi = ReflectionType.GetMethod(prefix + node.Name);
            if (mi != null) return mi;
            mi = ReflectionType.GetMethod(prefix + node.NodeType.ToString() + typesufix);
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
            throw new NotImplementedException();
        }
    }
}