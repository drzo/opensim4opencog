using System;
using RTParser.Utils;

namespace RTParser
{
    public class RequestSettings
    {
        public static int DefaultMaxOutputs = 1;
        public static int DefaultMaxPatterns = 1;
        public static int DefaultMaxTemplates = 1;
        public static int DefaultMaxBindings = 20;
        public static int DefaultMaxRecursion = 10;
        public static bool DefualtProcessMultipleTemplates = true;


        public bool ProcessMultipleTemplates = DefualtProcessMultipleTemplates;
        public GraphMaster Graph;

        public bool IsTraced = false;

        public int MaxPatterns = DefaultMaxPatterns;
        public int MaxTemplates = DefaultMaxTemplates;
        public int MaxBindings = DefaultMaxBindings;
        public int MaxOutputs = DefaultMaxOutputs;
        public int MinOutputs = 1;

        public void ApplySettings(RequestSettings user)
        {
            if (user==null) return;
            IsTraced = user.IsTraced || IsTraced;
            Graph = user.Graph ?? Graph;
            MaxTemplates = user.MaxTemplates;
            MaxPatterns = user.MaxPatterns;
            MaxBindings = user.MaxBindings;
            ProcessMultipleTemplates = user.ProcessMultipleTemplates;
        }
    }

}