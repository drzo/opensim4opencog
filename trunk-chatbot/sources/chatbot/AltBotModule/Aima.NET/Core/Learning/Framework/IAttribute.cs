using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    public interface IAttribute
    {
        string ValueAsString();

        string Name();
    }
}
