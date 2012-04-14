using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl
{
    public class DynamicAction: ObjectWithDynamicAttributes, IAction
    {
        public static readonly string AttributeName = "name";

        public DynamicAction(String name) 
        {
            this.SetAttribute(AttributeName, name);
        }

        public String GetName() 
        {
            return (String)GetAttribute(AttributeName);
        }

        public virtual bool IsNoOp() 
        {
            return false;
        }

        public override string DescribeType() 
        {
            return this.GetType().Name;
        }
    }
}
