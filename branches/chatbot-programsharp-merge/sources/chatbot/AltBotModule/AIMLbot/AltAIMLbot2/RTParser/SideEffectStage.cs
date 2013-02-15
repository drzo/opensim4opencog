using System;

namespace AltAIMLbot
{
    public class SideEffectStage : IComparable<SideEffectStage>
    {
        public static SideEffectStage UNSTARTED = new SideEffectStage("UNSTARTED", 0);
        public static SideEffectStage PARSING_INPUT = new SideEffectStage("PARSING_INPUT");
        public static SideEffectStage PARSE_INPUT_COMPLETE = new SideEffectStage("PARSE_INPUT_COMPLETE");
        public static SideEffectStage CREATE_GRAPH_QUERIES = new SideEffectStage("CREATE_GRAPH_QUERIES");
        public static SideEffectStage START_PATTERNS = new SideEffectStage("START_PATTERNS");
        public static SideEffectStage COLLECT_PARENT_PATTERNS = new SideEffectStage("COLLECT_PARENT_PATTERNS");
        public static SideEffectStage COLLECT_PRE_PATTERNS = new SideEffectStage("COLLECT_PRE_PATTERNS");
        public static SideEffectStage COLLECT_FALLBACK_PATTERNS = new SideEffectStage("COLLECT_FALLBACK_PATTERNS");
        public static SideEffectStage COLLECT_POST_PATTERNS = new SideEffectStage("COLLECT_POST_PATTERNS");
        public static SideEffectStage PRETEST_PATTERNS = new SideEffectStage("PRETEST_PATTERNS");
        public static SideEffectStage PRETEST_PATTERNS2= new SideEffectStage("PRETEST_PATTERNS2");
        public static SideEffectStage SCREEN_TEMPLATES = new SideEffectStage("SCREEN_TEMPLATES");
        public static SideEffectStage OUTPUT_TEMPLATES = new SideEffectStage("OUTPUT_TEMPLATES");
        public static SideEffectStage COMMIT_OUTPUT = new SideEffectStage("COMMIT_OUTPUT");
        private readonly string name;
        private readonly int value;


        /// <summary>
        /// Compares the current object with another object of the same type.
        /// </summary>
        /// <returns>
        /// A 32-bit signed integer that indicates the relative order of the objects being compared. The return value has the following meanings: 
        ///                     Value 
        ///                     Meaning 
        ///                     Less than zero 
        ///                     This object is less than the <paramref name="other"/> parameter.
        ///                     Zero 
        ///                     This object is equal to <paramref name="other"/>. 
        ///                     Greater than zero 
        ///                     This object is greater than <paramref name="other"/>. 
        /// </returns>
        /// <param name="other">An object to compare with this object.
        ///                 </param>
        public int CompareTo(SideEffectStage other)
        {
            return intValue() - other.intValue();
        }

        private int intValue()
        {
            return value;
        }

        public override string ToString()
        {
            return name;
        }

        static public bool operator >=(SideEffectStage s1, SideEffectStage s2)
        {
            return s1.intValue() >= s2.intValue();
        }

        public static bool operator <=(SideEffectStage s1, SideEffectStage s2)
        {
            return s1.intValue() <= s2.intValue();
        }

        public static bool operator <(SideEffectStage s1, SideEffectStage s2)
        {
            return s1.intValue() < s2.intValue();
        }

        public static bool operator >(SideEffectStage s1, SideEffectStage s2)
        {
            return s1.intValue() > s2.intValue();
        }
        static int LastCreatedValue = 0;
        private SideEffectStage(string nm)
        {
            name = nm;
            value = ++LastCreatedValue;
        }
        private SideEffectStage(string nm, int val)
        {
            name = nm;
            value = val;
        }
    }
}