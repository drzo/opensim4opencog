using System;
using System.Collections;
using System.Collections.Generic;

namespace SbsSW.SwiPlCs
{
    public class PrologBackedDictionary<TKey, TValue> : IDictionary<TKey, TValue>
    {
        public void InForiegnFrame(Action action)
        {
            uint fid = libpl.PL_open_foreign_frame();
            try
            {
                action();
            }
            finally
            {
                libpl.PL_close_foreign_frame(fid);
            }
        }

        private static PlTerm KeyToTerm(TKey key)
        {
            if (key.Equals(default(TValue))) return PlTerm.PlVar();
            return PrologClient.ToProlog(key);
        }

        private static PlTerm ValueToTerm(TValue value)
        {
            if (value.Equals(default(TValue))) return PlTerm.PlVar();
            return PrologClient.ToProlog(value);
        }

        private static PlTermV TermVOf(KeyValuePair<TKey, TValue> item)
        {
            return new PlTermV(KeyToTerm(item.Key), ValueToTerm(item.Value));
        }

        private readonly string _module = null;//"user";
        private readonly string _predname;
        private readonly Type keyType;
        private readonly Type valueType;
        public PrologBackedDictionary(string module, string predname)
        {
            //_module = module;
            _predname = predname;
            keyType = typeof(TKey);
            valueType = typeof(TValue);
        }

        #region Implementation of IEnumerable

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator()
        {
            return new PrologBackedDictionaryEnumerator(this);
        }

        public class PrologBackedDictionaryEnumerator : IEnumerator<KeyValuePair<TKey, TValue>>
        {
            private readonly PrologBackedDictionary<TKey, TValue> _dictionary;
            private uint fframe = 0;
            private PlTermV termV;
            private PlQuery plQuery;

            public PrologBackedDictionaryEnumerator(PrologBackedDictionary<TKey, TValue> dictionary)
            {
                _dictionary = dictionary;
                Reset();
            }

            #region Implementation of IDisposable

            /// <summary>
            /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
            /// </summary>
            /// <filterpriority>2</filterpriority>
            public void Dispose()
            {
                if (plQuery != null) plQuery.Dispose();
                plQuery = null;
                if (fframe != 0) libpl.PL_discard_foreign_frame(fframe);
                fframe = 0;
            }

            #endregion

            #region Implementation of IEnumerator

            /// <summary>
            /// Advances the enumerator to the next element of the collection.
            /// </summary>
            /// <returns>
            /// true if the enumerator was successfully advanced to the next element; false if the enumerator has passed the end of the collection.
            /// </returns>
            /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
            ///                 </exception><filterpriority>2</filterpriority>
            public bool MoveNext()
            {
                return plQuery.NextSolution();
            }

            /// <summary>
            /// Sets the enumerator to its initial position, which is before the first element in the collection.
            /// </summary>
            /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
            ///                 </exception><filterpriority>2</filterpriority>
            public void Reset()
            {
                Dispose();
                fframe = libpl.PL_open_foreign_frame();
                termV = new PlTermV(2);
                plQuery = new PlQuery(_dictionary._module, _dictionary._predname, termV);
            }

            /// <summary>
            /// Gets the element in the collection at the current position of the enumerator.
            /// </summary>
            /// <returns>
            /// The element in the collection at the current position of the enumerator.
            /// </returns>
            public KeyValuePair<TKey, TValue> Current
            {
                get
                {
                    return new KeyValuePair<TKey, TValue>(
                        (TKey)PrologClient.CastTerm(plQuery.Args[0], _dictionary.keyType),
                        (TValue)PrologClient.CastTerm(plQuery.Args[1], _dictionary.valueType));
                }
            }

            /// <summary>
            /// Gets the current element in the collection.
            /// </summary>
            /// <returns>
            /// The current element in the collection.
            /// </returns>
            /// <exception cref="T:System.InvalidOperationException">The enumerator is positioned before the first element of the collection or after the last element.
            ///                 </exception><filterpriority>2</filterpriority>
            object IEnumerator.Current
            {
                get { return Current; }
            }

            #endregion
        }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion

        #region Implementation of ICollection<KeyValuePair<TKey,TValue>>

        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public void Add(KeyValuePair<TKey, TValue> item)
        {
            InForiegnFrame(() =>
            {
                PlTerm newPlTermV = PrologClient.PlC(_predname, TermVOf(item));
                PlCall(_module, "assert", new PlTermV(newPlTermV));
            });

        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only. 
        ///                 </exception>
        public void Clear()
        {
            InForiegnFrame(() =>
            {
                PlTerm newPlTermV = PrologClient.PlC(_predname, new PlTermV(2));
                PlCall(_module, "retractall", new PlTermV(newPlTermV));
            });
        }

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.ICollection`1"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param>
        public bool Contains(KeyValuePair<TKey, TValue> item)
        {
            bool found = false;
            InForiegnFrame(() =>
                               {
                                   found = PlCall(_module, _predname, TermVOf(item));
                               });
            return found;
        }

        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.Generic.ICollection`1"/> to an <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">The one-dimensional <see cref="T:System.Array"/> that is the destination of the elements copied from <see cref="T:System.Collections.Generic.ICollection`1"/>. The <see cref="T:System.Array"/> must have zero-based indexing.
        ///                 </param><param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="arrayIndex"/> is less than 0.
        ///                 </exception><exception cref="T:System.ArgumentException"><paramref name="array"/> is multidimensional.
        ///                     -or-
        ///                 <paramref name="arrayIndex"/> is equal to or greater than the length of <paramref name="array"/>.
        ///                     -or-
        ///                     The number of elements in the source <see cref="T:System.Collections.Generic.ICollection`1"/> is greater than the available space from <paramref name="arrayIndex"/> to the end of the destination <paramref name="array"/>.
        ///                     -or-
        ///                     Type <paramref name="T"/> cannot be cast automatically to the type of the destination <paramref name="array"/>.
        ///                 </exception>
        public void CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Removes the first occurrence of a specific object from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> was successfully removed from the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false. This method also returns false if <paramref name="item"/> is not found in the original <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        /// <param name="item">The object to remove from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public bool Remove(KeyValuePair<TKey, TValue> item)
        {
            bool removed = false;
            InForiegnFrame(() =>
            {
                PlTerm newPlTermV = PrologClient.PlC(_predname, TermVOf(item));
                removed = PlCall(_module, "retract", new PlTermV(newPlTermV));
            });
            return removed;

        }

        /// <summary>
        /// Gets the number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// The number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        public int Count
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        public bool IsReadOnly
        {
            get { throw new NotImplementedException(); }
        }

        #endregion

        #region Implementation of IDictionary<TKey,TValue>

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the specified key.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the key; otherwise, false.
        /// </returns>
        /// <param name="key">The key to locate in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception>
        public bool ContainsKey(TKey key)
        {
            bool found = false;
            InForiegnFrame(() =>
                               {
                                   found = PlCall(_module, _predname, new PlTermV(KeyToTerm(key), PlTerm.PlVar()));
                               });
            return found;
        }

        /// <summary>
        /// Adds an element with the provided key and value to the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <param name="key">The object to use as the key of the element to add.
        ///                 </param><param name="value">The object to use as the value of the element to add.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentException">An element with the same key already exists in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public void Add(TKey key, TValue value)
        {
            Add(new KeyValuePair<TKey, TValue>(key, value));
        }

        /// <summary>
        /// Removes the element with the specified key from the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// true if the element is successfully removed; otherwise, false.  This method also returns false if <paramref name="key"/> was not found in the original <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        /// <param name="key">The key of the element to remove.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public bool Remove(TKey key)
        {
            bool removed = false;
            InForiegnFrame(() =>
                               {

                                   PlTerm newPlTermV = PrologClient.PlC(_predname, KeyToTerm(key), PlTerm.PlVar());
                                   removed = PlCall(_module, "retract", new PlTermV(newPlTermV));
                               });
            return removed;
        }

        /// <summary>
        /// Gets the value associated with the specified key.
        /// </summary>
        /// <returns>
        /// true if the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the specified key; otherwise, false.
        /// </returns>
        /// <param name="key">The key whose value to get.
        ///                 </param><param name="value">When this method returns, the value associated with the specified key, if the key is found; otherwise, the default value for the type of the <paramref name="value"/> parameter. This parameter is passed uninitialized.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception>
        public bool TryGetValue(TKey key, out TValue value)
        {
            TValue value0 = default(TValue);
            bool res = false;
            InForiegnFrame(() =>
                               {
                                   PlTerm plTermPlVar = PlTerm.PlVar();
                                   PlTermV newPlTermV = new PlTermV(KeyToTerm(key), plTermPlVar);
                                   res = PlCall(_module, _predname, newPlTermV);
                                   if (res)
                                   {
                                       value0 = (TValue)PrologClient.CastTerm(newPlTermV[1], valueType);
                                   }
                                   else
                                   {

                                   }
                               });
            value = value0;
            return res;
        }

        private bool PlCall(string module, string predname, PlTermV termV)
        {
            return PrologClient.PlCall(module, predname, termV);
        }

        /// <summary>
        /// Gets or sets the element with the specified key.
        /// </summary>
        /// <returns>
        /// The element with the specified key.
        /// </returns>
        /// <param name="key">The key of the element to get or set.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.Collections.Generic.KeyNotFoundException">The property is retrieved and <paramref name="key"/> is not found.
        ///                 </exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public TValue this[TKey key]
        {
            get
            {
                TValue tvalue = default(TValue);
                InForiegnFrame(() =>
                {
                    PlTerm newPlTermV = PrologClient.PlC(_predname, KeyToTerm(key), PlTerm.PlVar());
                    bool res = PlCall(_module, _predname, new PlTermV(newPlTermV));
                    if (res)
                    {
                        tvalue = (TValue)PrologClient.CastTerm(newPlTermV.Arg(1), valueType);
                    }
                    else
                    {
                        // tvalue = default(TValue);
                    }
                });
                return tvalue;

            }
            set
            {
                Remove(key);
                Add(new KeyValuePair<TKey, TValue>(key, value));
            }
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the keys of the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the keys of the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<TKey> Keys
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<TValue> Values
        {
            get { throw new NotImplementedException(); }
        }

        #endregion
    }

}
