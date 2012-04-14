using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl
{
    public class DynamicPercept : ObjectWithDynamicAttributes, IPercept
    {
        public override string DescribeType()
        {
            return typeof(IPercept).Name;
        }

        public DynamicPercept()
        {
        }
        public DynamicPercept(object key1, object value1)
        {
            SetAttribute(key1, value1);
        }

        public DynamicPercept(object key1, object value1, object key2, object value2)
        {
            SetAttribute(key1, value1);
            SetAttribute(key2, value2);
        }

        public DynamicPercept(IList<object> keys, IList<object> values)
        {
            if (keys.Count != values.Count)
            {
                throw new ArgumentException("Length of both keys and values must be the same");
            }

            for (var i = 0; i < keys.Count; i++)
            {
                SetAttribute(keys[i], values[i]);
            }
        }
    }
}
