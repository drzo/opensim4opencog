using System;
using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser.Variables
{
    public class CollectionProperty
    {
        Unifiable _lastSetTopic;
        private readonly List<Unifiable> list;
        private readonly GetSetProperty provider;
        private readonly GetUnifiable EmptyResult;

        public CollectionProperty(List<Unifiable> unifiables, GetUnifiable noResult)
        {
            list = unifiables;
            EmptyResult = noResult;
            provider = new GetSetProperty(GetTopicInt, SetTopicInt);
        }

        private void SetTopicInt(Unifiable obj)
        {
            if (obj == EmptyResult())
            {
                list.Clear();
                return;
            }
            if (Unifiable.IsNullOrEmpty(obj))
            {
                if (Unifiable.IsNullOrEmpty(_lastSetTopic))
                {
                    list.RemoveAt(0);
                    return;
                }
                int removeTopicInt = RemoveTopicInt(_lastSetTopic);
            }
            int indexOf = list.IndexOf(obj);
            if (indexOf == 0) return;
            if (indexOf > 0)
            {
                list.RemoveAt(indexOf);
            }
            list.Insert(0, obj);
        }

        private int RemoveTopicInt(Unifiable obj)
        {
            if (Unifiable.IsNullOrEmpty(obj))
            {
                return -1;
            }
            int indexOf = list.IndexOf(obj);
            if (indexOf == -1) return -1;
            list.RemoveAt(indexOf);
            return indexOf;
        }

        private Unifiable GetTopicInt()
        {
            if (list.Count == 0)
            {
                return EmptyResult();
            }
            return list[0];
        }

        public GetSetProperty GetProvider()
        {
            return provider;
        }
    }
}