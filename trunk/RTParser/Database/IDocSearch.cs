using System;
using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser.Database
{
    public interface IDocSearch
    {
        ICollection<ISearchResult> Search(string searchTerm1, WordExpander wordNetExpanderOnNoHits);
    }

    public interface ISearchResult
    {
        string Text { get; }
        float Score { get; }
        object ID { get; }
    }

    public class OneSearchResult : ISearchResult
    {
        public OneSearchResult(object name,  string text , float score)
        {
            ID = name;
            Score = score;
            Text = text;
        }

        public override string ToString()
        {
            return TextPatternUtils.SafeFormat("{0} (menevalue= {1:1} )", Text, Score);
        }

        virtual public string Text { get; set; }
        virtual public float Score { get; set; }
        virtual public object ID { get; set; }
    }
}