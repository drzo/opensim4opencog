using System;
using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{
    public interface QuerySettings
    {
        bool ProcessMultiplePatterns { get; }
        bool ProcessMultipleTemplates { get; }
        int MaxTemplates { get; }
        int MaxPatterns { get; }
        int MaxOutputs { get; }
        GraphMaster Graph { get; }
        bool IsTraced { get; }
        int MinOutputs { get; }
    }

    public abstract class RequestSettingsImpl: QuerySettings
    {
        public static int DefaultMaxOutputs = 2;
        public static int DefaultMaxPatterns = 1;
        public static int DefaultMaxTemplates = 1;
        public static int DefaultMaxRecursion = 10;
        public static bool DefualtProcessMultipleTemplates = true;
        public static bool DefualtProcessMultipleBindings = true;

        protected RequestSettingsImpl()
        {
            ProcessMultipleTemplates = DefualtProcessMultipleTemplates;    
            ProcessMultiplePatterns = DefualtProcessMultipleBindings;
            MaxPatterns = DefaultMaxPatterns;
            MaxTemplates = DefaultMaxTemplates;
            MaxOutputs = DefaultMaxOutputs;
            MinOutputs = 1;
            IsTraced = false;
        }

        public bool ProcessMultiplePatterns { get; set; }
        public bool ProcessMultipleTemplates { get; set; }
        public int MaxTemplates { get; set; }
        public int MaxPatterns { get; set; }
        public int MaxOutputs { get; set; }

        public abstract GraphMaster Graph { get; set; }

        public bool IsTraced { get; set; }

        public int MinOutputs { get; set; }

        public void ApplySettings(QuerySettings user)
        {
            if (user==null) return;
            IsTraced = user.IsTraced || IsTraced;
            //Graph = user.Graph ?? Graph;
            MaxTemplates = user.MaxTemplates;
            MaxPatterns = user.MaxPatterns;
            MaxOutputs = user.MaxOutputs;
            ProcessMultipleTemplates = user.ProcessMultipleTemplates;
            ProcessMultiplePatterns = user.ProcessMultiplePatterns;
        }
    }

}