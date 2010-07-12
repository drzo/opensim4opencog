using System;
using RTParser.Utils;

namespace RTParser
{
    public class RequestSettings
    {
        public static int DefaultMaxOutputs = 2;
        public static int DefaultMaxPatterns = 1;
        public static int DefaultMaxTemplates = 1;
        public static int DefaultMaxRecursion = 10;
        public static bool DefualtProcessMultipleTemplates = true;
        public static bool DefualtProcessMultipleBindings = true;


        public bool ProcessMultipleTemplates = DefualtProcessMultipleTemplates;
        public bool ProcessMultiplePatterns = DefualtProcessMultipleBindings;
        public GraphMaster Graph;

        public bool IsTraced = false;

        public int MaxPatterns = DefaultMaxPatterns;
        public int MaxTemplates = DefaultMaxTemplates;
        public int MaxOutputs = DefaultMaxOutputs;
        public int MinOutputs = 1;

        public void ApplySettings(RequestSettings user)
        {
            if (user==null) return;
            IsTraced = user.IsTraced || IsTraced;
            Graph = user.Graph ?? Graph;
            MaxTemplates = user.MaxTemplates;
            MaxPatterns = user.MaxPatterns;
            MaxOutputs = user.MaxOutputs;
            ProcessMultipleTemplates = user.ProcessMultipleTemplates;
            ProcessMultiplePatterns = user.ProcessMultiplePatterns;
        }
    }

}