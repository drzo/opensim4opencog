using System;
using System.Collections.Generic;
using AltAIMLbot.Utils;

namespace AltAIMLbot.Variables
{
    public class CollectionProperty<T>
    {
        T _lastSetTopic;
        private readonly List<T> list;
        private readonly GetSetProperty<T> provider;
        private readonly GetUnifiable<T> EmptyResult;

        public CollectionProperty(List<T> unifiables, GetUnifiable<T> noResult)
        {
            list = unifiables;
            EmptyResult = noResult;
            provider = new GetSetProperty<T>(GetTopicInt, SetTopicInt);
        }

        private void SetTopicInt(T obj)
        {
            if (Object.Equals(obj, EmptyResult()))
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

        private int RemoveTopicInt(T obj)
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

        private T GetTopicInt()
        {
            if (list.Count == 0)
            {
                return EmptyResult();
            }
            return list[0];
        }

        public GetSetProperty<T> GetProvider()
        {
            return (GetSetProperty<T>)provider;
        }
    }
}