using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public interface IAttributeSpecification
    {
        bool IsValid(string str);

        string GetAttributeName();

        IAttribute CreateAttribute(string rawValue);
    }
}
