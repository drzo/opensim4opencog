using System;

namespace AltAIMLbot.Utils
{
    /// <summary>
    /// A custom attribute to be applied to all custom tags in external "late bound" dlls
    /// </summary>
    [AttributeUsage(AttributeTargets.Class)]
    public class CustomTagAttribute : Attribute
    {
    }
}