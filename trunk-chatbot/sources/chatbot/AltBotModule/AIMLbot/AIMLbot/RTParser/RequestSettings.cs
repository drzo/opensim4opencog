using System;
using System.Collections.Generic;
using System.Text;
using AltAIMLbot.Utils;
using RTParser.Utils;

namespace RTParser
{

    public class DictionaryOverride<T>
    {
        public T Current { get; set; }
        public T Previous { get; set; }
    }

    public class SettingMinMaxCurrent<T> where T : IComparable
    {
        private KeyValuePair<string, T> pair;
        public T Min { get; set; }
        public T Max { get; set; }
        public T Current { get; set; }
        public bool IsOverMax
        {
            get
            {
                // Max.CompareTo(Min) < 0 means if max is less than min.. it cant timeout
                return Current.CompareTo(Max) >= 0 && Max.CompareTo(Min) >- 0;
            }
        }
        public override string ToString()
        {
            return OrNull(Min) + "=<" + OrNull(Current) + "=<" + OrNull(Max);
        }

        private static string OrNull(T p0)
        {
            if (ReferenceEquals(null, p0)) return "-NULL-";
            return "" + p0;
        }

        public void SetConstraintsFrom(SettingMinMaxCurrent<T> sraiDepth)
        {
            Max = sraiDepth.Max;
            Min = sraiDepth.Min;
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
        public override string StartGraphName { get; set; }

        #endregion

        public static void ApplyImplSettings(QuerySettingsImpl ws, QuerySettingsImpl rs)
        {
            ws.UseLuceneForSetMaxDepth = Math.Min(rs.UseLuceneForSetMaxDepth, ws.UseLuceneForSetMaxDepth);
            ws.UseLuceneForGetMaxDepth = Math.Min(rs.UseLuceneForGetMaxDepth, ws.UseLuceneForGetMaxDepth);
        }
    }

    abstract public class QuerySettings : QuerySettingsSettable
    {
        public static int UNLIMITED = 999;

        public void IncreaseLimits(int minsAndMaxes)
        {
            IncreaseLimits(this, minsAndMaxes, minsAndMaxes);
        }
        public static void IncreaseLimits(QuerySettingsSettable request, int mins, int maxs)
        {
            return;
            //request.MinOutputs = ((QuerySettingsReadOnly)request).MinOutputs + mins;
            request.MinTemplates = ((QuerySettingsReadOnly)request).MinTemplates + mins;
            request.MinPatterns = ((QuerySettingsReadOnly)request).MinPatterns + mins;
            //request.MaxOutputs = ((QuerySettingsReadOnly)request).MaxOutputs + maxs;
            request.MaxTemplates = ((QuerySettingsReadOnly)request).MaxTemplates + maxs;
            request.MaxPatterns = ((QuerySettingsReadOnly)request).MaxPatterns + maxs;
            request.ProcessMultipleTemplates = true;
            request.ProcessMultiplePatterns = true;
        }

        static public String ToSettingsString(QuerySettingsReadOnly qs)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendFormat("Patterns={0}-{1} PM={2}\n", qs.MinPatterns, qs.MaxPatterns, qs.ProcessMultiplePatterns);
            sb.AppendFormat("Templates={0}-{1} PM={2}\n", qs.MinTemplates, qs.MaxTemplates, qs.ProcessMultipleTemplates);
            sb.AppendFormat("Outputs={0}-{1}\n", qs.MinOutputs, qs.MaxOutputs);
            return sb.ToString();
        }

        public static QuerySettings AIMLDefaults = new QuerySettingsImpl(null)
        {
            ProcessMultipleTemplates = true, // needed to find verbal outputs
            ProcessMultiplePatterns = false,
            MinGetVars = 0,
            MaxGetVars = UNLIMITED,
            MinSetVars = 0,
            MaxSetVars = UNLIMITED,
            MinOutputs = 11,
            MaxOutputs = 11,
            MinPatterns = 11,
            MaxPatterns = 11,
            MinTemplates =11,
            MaxTemplates = 11,
            UseDictForSetMaxDepth = UNLIMITED,
            UseLuceneForGetMaxDepth = 112,
            UseLuceneForSetMaxDepth = 112,
            StartGraphName = "default",
            IsTraced = false,
            _sraiDepth = new SettingMinMaxCurrent<int>()
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
            MinOutputs = 14,
            MaxOutputs = 111,
            MinPatterns = 14,
            MaxPatterns = 112,
            MinTemplates = 114,
            MaxTemplates = 114,
            _sraiDepth = new SettingMinMaxCurrent<int>()
                            {

                                Min = 0,
                                Max = AltBot.SraiDepthMax
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

        public static QuerySettings SRAIDefaults = new QuerySettingsImpl(CogbotDefaults)
        {
            ProcessMultipleTemplates = true, // needed to find verbal outputs
            ProcessMultiplePatterns = false,

            /*
            MinOutputs = 1,
            MaxOutputs = 1,
            MinPatterns = 1,
            MaxPatterns = 1,
            MinTemplates = 1,
            MaxTemplates = 1,
             */
            StartGraphName = null,
            IsTraced = false,
            /***
             * undecided!
            UseLuceneForGetMaxDepth = false,
            UseLuceneForGetMaxDepth = false,
            ****/
            _sraiDepth = new SettingMinMaxCurrent<int>()
            {
                Current = 0,
                Min = 0,
                Max = UNLIMITED,
            },
        };

        private SettingMinMaxCurrent<int> _sraiDepth = new SettingMinMaxCurrent<int>();

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
            w.MaxSetVars = r.MaxSetVars;
            w.MaxGetVars = r.MaxGetVars;
            w.UseDictForSetMaxDepth = r.UseDictForSetMaxDepth;
            w.ProcessMultipleTemplates = r.ProcessMultipleTemplates;
            w.ProcessMultiplePatterns = r.ProcessMultiplePatterns;
            w.SraiDepth.SetConstraintsFrom(r.SraiDepth);
            var rs = r as QuerySettingsImpl;
            if (rs != null)
            {
                var ws = w as QuerySettingsImpl;
                if (ws != null)
                {
                    QuerySettingsImpl.ApplyImplSettings(ws, rs);
                }
            }

            //  GraphMaster gm = r.Graph;
            //  if (gm != null) w.Graph = gm;
            //  string gn = r.GraphName;
            //  if (gn != null)  w.GraphName = gn;

        }

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        public abstract string StartGraphName { get; set; }

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
        /// The number of sets before the query is stopped
        /// </summary>
        public int MinSetVars { get; set; }
        public int MaxSetVars { get; set; }

        /// <summary>
        /// The number of gets before the query is stopped
        /// </summary>
        public int MinGetVars { get; set; }
        public int MaxGetVars { get; set; }

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
        public SettingMinMaxCurrent<int> SraiDepth
        {
            get { return _sraiDepth; }
        }

        //public virtual SettingMinMaxCurrent<bool> CanSrai { get; set; }

        //public abstract SettingMinMaxCurrent<TimeSpan> Time { get; set; }
        public int UseDictForSetMaxDepth { get; set; }

        public int UseLuceneForGetMaxDepth { get; set; }
        public int UseLuceneForSetMaxDepth { get; set; }

        public virtual int DebugLevel { get; set; }

        public static bool IsSomething(Unifiable s, out Unifiable something)
        {
            return StaticAIMLUtils.IsSomething(s, out something);
        }
        public static bool IsNullOrEmpty(Object unifiable)
        {
            return StaticAIMLUtils.IsNullOrEmpty(unifiable);
        }
        public static string SafeFormat(string f, params object[] args)
        {
            return StaticAIMLUtils.SafeFormat(f, args);
        }
    }

    public interface QuerySettingsReadOnly
    {
        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        string StartGraphName { get; }

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

        SettingMinMaxCurrent<int> SraiDepth { get; }

        int DebugLevel { get; }


        int UseLuceneForSetMaxDepth { get; }
        int UseLuceneForGetMaxDepth { get; }
        int MaxSetVars { get; set; }
        int MaxGetVars { get; set; }
        int UseDictForSetMaxDepth { get; set; }
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
        string StartGraphName { set; }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        bool IsTraced { set; get; }

        int DebugLevel { set; }

        int UseLuceneForSetMaxDepth { set; }
        int UseLuceneForGetMaxDepth { set; }
    }
}