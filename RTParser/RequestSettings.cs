using System;
using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{

    public class DictionaryOverride<T>
    {
        public T Current { get; set; }
        public T Previous { get; set; }
    }

    public struct SettingMinMaxCurrent<T> where T : IComparable
    {
        private KeyValuePair<string, T> pair;
        public T Min { get; set; }
        public T Max { get; set; }
        public T Current { get; set; }
        public bool IsOverMax
        {
            get { return Current.CompareTo(Max) >= 0; }
        }
    }

    sealed public class QuerySettingsImpl : QuerySettings
    {

        public QuerySettingsImpl(QuerySettingsReadOnly settings)
            : base(settings)
        {
        }

        #region Overrides of QuerySettings

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        sealed public override string GraphName { get; set; }

        #endregion
    }

    abstract public class QuerySettings : StaticAIMLUtils, QuerySettingsSettable
    {
        public void IncreaseLimits(int minsAndMaxes)
        {
            IncreaseLimits(this, minsAndMaxes, minsAndMaxes);
        }
        public static void IncreaseLimits(QuerySettingsSettable request, int mins, int maxs)
        {
            //request.MinOutputs = ((QuerySettingsReadOnly)request).MinOutputs + mins;
            request.MinTemplates = ((QuerySettingsReadOnly)request).MinTemplates + mins;
            request.MinPatterns = ((QuerySettingsReadOnly)request).MinPatterns + mins;
            //request.MaxOutputs = ((QuerySettingsReadOnly)request).MaxOutputs + maxs;
            request.MaxTemplates = ((QuerySettingsReadOnly)request).MaxTemplates + maxs;
            request.MaxPatterns = ((QuerySettingsReadOnly)request).MaxPatterns + maxs;
            request.ProcessMultipleTemplates = true;
            request.ProcessMultiplePatterns = true;
        }

        public static int UNLIMITED = 999;
        public static QuerySettings AIMLDefaults = new QuerySettingsImpl(null)
        {
            ProcessMultipleTemplates = true, // needed to find verbal outputs
            ProcessMultiplePatterns = false,
            MinOutputs = 1,
            MaxOutputs = 1,
            MinPatterns = 1,
            MaxPatterns = 1,
            MinTemplates = 1,
            MaxTemplates = 1,
            GraphName = "default",
            IsTraced = false,
            SraiDepth = new SettingMinMaxCurrent<int>()
            {
                Current = 0,
                Min = 0,
                Max = UNLIMITED,
            },
        };

        public static QuerySettings CogbotDefaults = new QuerySettingsImpl(AIMLDefaults)
        {
            ProcessMultipleTemplates = true, // needed to find verbal outputs
            ProcessMultiplePatterns = true, // needed to find verbal outputs
            MaxTemplates = UNLIMITED,
            SraiDepth = new SettingMinMaxCurrent<int>()
                            {
                                Current = 0,
                                Min = 0,
                                Max = UNLIMITED,
                            },

        };

        public static QuerySettings FindAll = new QuerySettingsImpl(CogbotDefaults)
        {
            ProcessMultipleTemplates = true, // needed to find verbal outputs
            ProcessMultiplePatterns = true, // needed to find verbal outputs
            MaxOutputs = UNLIMITED,
            MaxPatterns = UNLIMITED,
            MaxTemplates = UNLIMITED,
            MinTemplates = UNLIMITED,
            MinOutputs = UNLIMITED,
            MinPatterns = UNLIMITED,
        };

        protected QuerySettings(QuerySettingsReadOnly defaults):base()
        {
            ApplySettings(defaults, this);
        }

        public static void ApplySettings(QuerySettingsReadOnly r, QuerySettingsSettable w)
        {

            if (r == null || w == null || r == w) return;
            if (r.IsTraced) w.IsTraced = true;
            w.MaxTemplates = r.MaxTemplates;
            w.MaxPatterns = r.MaxPatterns;
            w.MaxOutputs = r.MaxOutputs;
            w.MinTemplates = r.MinTemplates;
            w.MinPatterns = r.MinPatterns;
            w.MinOutputs = r.MinOutputs;
            w.ProcessMultipleTemplates = r.ProcessMultipleTemplates;
            w.ProcessMultiplePatterns = r.ProcessMultiplePatterns;
            w.SraiDepth = r.SraiDepth;

            //  GraphMaster gm = r.Graph;
            //  if (gm != null) w.Graph = gm;
            //  string gn = r.GraphName;
            //  if (gn != null)  w.GraphName = gn;

        }

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        public abstract string GraphName { get; set; }

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        //  public abstract GraphMaster Graph { get; set; }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        public virtual bool IsTraced { get; set; }

        /// <summary>
        /// Some patterns implies multiple templates
        /// </summary>
        public bool ProcessMultipleTemplates { get; set; }

        /// <summary>
        /// After the first pattern, if the min/maxes are not satisfied.. 
        /// Try a new pattern
        /// </summary>
        public bool ProcessMultiplePatterns { get; set; }

        /// <summary>
        /// the number of "successfull" (non-empty) templates after "eval"
        /// </summary>
        public int MinOutputs { get; set; }
        public int MaxOutputs { get; set; }

        /// <summary>
        /// The number of templates to harvest in query stage (should be at least one)
        /// </summary>
        public int MinTemplates { get; set; }
        public int MaxTemplates { get; set; }

        /// <summary>
        /// The number of patterns to harvest in query stage (should be at least one)
        /// </summary>
        public int MinPatterns { get; set; }
        public int MaxPatterns { get; set; }

        /// <summary>
        /// The number srai's one can decend generation stage
        /// the "Min" is what is starts out with (defualt 0)
        /// </summary>
        public virtual SettingMinMaxCurrent<int> SraiDepth { get; set; }

        public virtual SettingMinMaxCurrent<bool> CanSrai { get; set; }

        public virtual SettingMinMaxCurrent<TimeSpan> Time { get; set; }


        public virtual int DebugLevel { get; set; }
    }

    public interface QuerySettingsReadOnly
    {
        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        string GraphName { get; }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        bool IsTraced { get; }

        /// <summary>
        /// Some patterns implies multiple templates
        /// </summary>
        bool ProcessMultipleTemplates { get; }
        /// <summary>
        /// After the first pattern, if the min/maxes are not satisfied.. keep going
        /// </summary>
        bool ProcessMultiplePatterns { get; }

        /// <summary>
        /// the number of "successfull" (non-empty) templates after "eval"
        /// </summary>
        int MinOutputs { get; }
        int MaxOutputs { get; }

        /// <summary>
        /// The number of templates to harvest in query stage (should be at least one)
        /// </summary>
        int MinTemplates { get; }
        int MaxTemplates { get; }

        /// <summary>
        /// The number of patterns to harvest in query stage (should be at least one)
        /// </summary>
        int MinPatterns { get; }
        int MaxPatterns { get; }

        SettingMinMaxCurrent<int> SraiDepth { get; set; }

        int DebugLevel { get; }
    }

    public interface QuerySettingsSettable : QuerySettingsReadOnly
    {
        void IncreaseLimits(int minsAndMaxes);
        /// <summary>
        /// Some patterns implies multiple templates
        /// </summary>
        bool ProcessMultipleTemplates { set; }
        /// <summary>
        /// After the first pattern, if the min/maxes are not satisfied.. keep going
        /// </summary>
        bool ProcessMultiplePatterns { set; }

        /// <summary>
        /// the number of "successfull" (non-empty) templates after "eval"
        /// </summary>
        int MinOutputs { set; }
        int MaxOutputs { set; }

        /// <summary>
        /// The number of templates to harvest in query stage (should be at least one)
        /// </summary>
        int MinTemplates { set; }
        int MaxTemplates { set; }

        /// <summary>
        /// The number of patterns to harvest in query stage (should be at least one)
        /// </summary>
        int MinPatterns { set; }
        int MaxPatterns { set; }

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        string GraphName { set; }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        bool IsTraced { set; }

        int DebugLevel { set; }
    }
}