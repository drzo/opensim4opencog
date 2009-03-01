using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;
using System.Windows.Forms;
using System.Collections;

namespace cogbot.TheOpenSims.Navigation
{
    /// <summary>
    /// The SortableList allows to maintain a list sorted as long as needed.
    /// If no IComparer interface has been provided at construction, then the list expects the Objects to implement IComparer.
    /// If the list is not sorted it behaves like an ordinary list.
    /// When sorted, the list's "Add" method will put new objects at the right place.
    /// As well the "Contains" and "IndexOf" methods will perform a binary search.
    /// </summary>
    [Serializable]
    public class SortableList : IList, ICloneable
    {
        private ArrayList _List;
        private IComparer _Comparer;
        private bool _UseObjectsComparison;
        private bool _IsSorted;
        private bool _KeepSorted;
        private bool _AddDuplicates;

        /// <summary>
        /// Default constructor.
        /// Since no IComparer is provided here, added objects must implement the IComparer interface.
        /// </summary>
        public SortableList() { InitProperties(null, 0); }

        /// <summary>
        /// Constructor.
        /// Since no IComparer is provided, added objects must implement the IComparer interface.
        /// </summary>
        /// <param name="Capacity">Capacity of the list (<see cref="ArrayList.Capacity">ArrayList.Capacity</see>)</param>
        public SortableList(int Capacity) { InitProperties(null, Capacity); }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="Comparer">Will be used to compare added elements for sort and search operations.</param>
        public SortableList(IComparer Comparer) { InitProperties(Comparer, 0); }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="Comparer">Will be used to compare added elements for sort and search operations.</param>
        /// <param name="Capacity">Capacity of the list (<see cref="ArrayList.Capacity">ArrayList.Capacity</see>)</param>
        public SortableList(IComparer Comparer, int Capacity) { InitProperties(Comparer, Capacity); }

        /// <summary>
        /// 'Get only' property that indicates if the list is sorted.
        /// </summary>
        public bool IsSorted { get { return _IsSorted; } }

        /// <summary>
        /// Get : Indicates if the list must be kept sorted from now on.
        /// Set : Tells the list if it must stay sorted or not. Impossible to set to true if the list is not sorted.
        /// <see cref="KeepSorted">KeepSorted</see>==true implies that <see cref="IsSorted">IsSorted</see>==true
        /// </summary>
        /// <exception cref="InvalidOperationException">Cannot be set to true if the list is not sorted yet.</exception>
        public bool KeepSorted
        {
            set
            {
                if (value == true && !_IsSorted) throw new InvalidOperationException("The SortableList can only be kept sorted if it is sorted.");
                _KeepSorted = value;
            }
            get { return _KeepSorted; }
        }

        /// <summary>
        /// If set to true, it will not be possible to add an object to the list if its value is already in the list.
        /// </summary>
        public bool AddDuplicates { set { _AddDuplicates = value; } get { return _AddDuplicates; } }

        /// <summary>
        /// IList implementation.
        /// Gets - or sets - object's value at a specified index.
        /// The set operation is impossible if the <see cref="KeepSorted">KeepSorted</see> property is set to true.
        /// </summary>
        /// <exception cref="ArgumentOutOfRangeException">Index is less than zero or Index is greater than Count.</exception>
        /// <exception cref="InvalidOperationException">[] operator cannot be used to set a value if KeepSorted property is set to true.</exception>
        public object this[int Index]
        {
            get
            {
                if (Index >= _List.Count || Index < 0) throw new ArgumentOutOfRangeException("Index is less than zero or Index is greater than Count.");
                return _List[Index];
            }
            set
            {
                if (_KeepSorted) throw new InvalidOperationException("[] operator cannot be used to set a value if KeepSorted property is set to true.");
                if (Index >= _List.Count || Index < 0) throw new ArgumentOutOfRangeException("Index is less than zero or Index is greater than Count.");
                if (ObjectIsCompliant(value))
                {
                    object OBefore = Index > 0 ? _List[Index - 1] : null;
                    object OAfter = Index < Count - 1 ? _List[Index + 1] : null;
                    if (OBefore != null && _Comparer.Compare(OBefore, value) > 0 || OAfter != null && _Comparer.Compare(value, OAfter) > 0) _IsSorted = false;
                    _List[Index] = value;
                }
            }
        }

        /// <summary>
        /// IList implementation.
        /// If the <see cref="KeepSorted">KeepSorted</see> property is set to true, the object will be added at the right place.
        /// Else it will be added at the end of the list.
        /// </summary>
        /// <param name="O">The object to add.</param>
        /// <returns>The index where the object has been added.</returns>
        /// <exception cref="ArgumentException">The SortableList is set to use object's IComparable interface, and the specifed object does not implement this interface.</exception>
        public int Add(object O)
        {
            int Return = -1;
            if (ObjectIsCompliant(O))
            {
                if (_KeepSorted)
                {
                    int Index = IndexOf(O);
                    int NewIndex = Index >= 0 ? Index : -Index - 1;
                    if (NewIndex >= Count) _List.Add(O);
                    else _List.Insert(NewIndex, O);
                    Return = NewIndex;
                }
                else
                {
                    _IsSorted = false;
                    Return = _List.Add(O);
                }
            }
            return Return;
        }

        /// <summary>
        /// IList implementation.
        /// Search for a specified object in the list.
        /// If the list is sorted, a <see cref="ArrayList.BinarySearch">BinarySearch</see> is performed using IComparer interface.
        /// Else the <see cref="Equals">Object.Equals</see> implementation is used.
        /// </summary>
        /// <param name="O">The object to look for</param>
        /// <returns>true if the object is in the list, otherwise false.</returns>
        public bool Contains(object O)
        {
            return _IsSorted ? _List.BinarySearch(O, _Comparer) >= 0 : _List.Contains(O);
        }

        /// <summary>
        /// IList implementation.
        /// Returns the index of the specified object in the list.
        /// If the list is sorted, a <see cref="ArrayList.BinarySearch">BinarySearch</see> is performed using IComparer interface.
        /// Else the <see cref="Equals">Object.Equals</see> implementation of objects is used.
        /// </summary>
        /// <param name="O">The object to locate.</param>
        /// <returns>
        /// If the object has been found, a positive integer corresponding to its position.
        /// If the objects has not been found, a negative integer which is the bitwise complement of the index of the next element.
        /// </returns>
        public int IndexOf(object O)
        {
            int Result = -1;
            if (_IsSorted)
            {
                Result = _List.BinarySearch(O, _Comparer);
                while (Result > 0 && _List[Result - 1].Equals(O)) Result--; // We want to point at the FIRST occurence
            }
            else Result = _List.IndexOf(O);
            return Result;
        }

        /// <summary>
        /// IList implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        public bool IsFixedSize { get { return _List.IsFixedSize; } }

        /// <summary>
        /// IList implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        public bool IsReadOnly { get { return _List.IsReadOnly; } }

        /// <summary>
        /// IList implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        public void Clear() { _List.Clear(); }

        /// <summary>
        /// IList implementation.
        /// Inserts an objects at a specified index.
        /// Cannot be used if the list has its KeepSorted property set to true.
        /// </summary>
        /// <param name="Index">The index before which the object must be added.</param>
        /// <param name="O">The object to add.</param>
        /// <exception cref="ArgumentException">The SortableList is set to use object's IComparable interface, and the specifed object does not implement this interface.</exception>
        /// <exception cref="ArgumentOutOfRangeException">Index is less than zero or Index is greater than Count.</exception>
        /// <exception cref="InvalidOperationException">If the object is added at the specify index, the list will not be sorted any more and the <see cref="KeepSorted"/> property is set to true.</exception>
        public void Insert(int Index, object O)
        {
            if (_KeepSorted) throw new InvalidOperationException("Insert method cannot be called if KeepSorted property is set to true.");
            if (Index >= _List.Count || Index < 0) throw new ArgumentOutOfRangeException("Index is less than zero or Index is greater than Count.");
            if (ObjectIsCompliant(O))
            {
                object OBefore = Index > 0 ? _List[Index - 1] : null;
                object OAfter = _List[Index];
                if (OBefore != null && _Comparer.Compare(OBefore, O) > 0 || OAfter != null && _Comparer.Compare(O, OAfter) > 0) _IsSorted = false;
                _List.Insert(Index, O);
            }
        }

        /// <summary>
        /// IList implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        /// <param name="Value">The object whose value must be removed if found in the list.</param>
        public void Remove(object Value) { _List.Remove(Value); }

        /// <summary>
        /// IList implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        /// <param name="Index">Index of object to remove.</param>
        public void RemoveAt(int Index) { _List.RemoveAt(Index); }

        /// <summary>
        /// IList.ICollection implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        /// <param name="array"></param>
        /// <param name="arrayIndex"></param>
        public void CopyTo(Array array, int arrayIndex) { _List.CopyTo(array, arrayIndex); }

        /// <summary>
        /// IList.ICollection implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        public int Count { get { return _List.Count; } }

        /// <summary>
        /// IList.ICollection implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        public bool IsSynchronized { get { return _List.IsSynchronized; } }

        /// <summary>
        /// IList.ICollection implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        public object SyncRoot { get { return _List.SyncRoot; } }

        /// <summary>
        /// IList.IEnumerable implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        /// <returns>Enumerator on the list.</returns>
        public IEnumerator GetEnumerator() { return _List.GetEnumerator(); }

        /// <summary>
        /// ICloneable implementation.
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        /// <returns>Cloned object.</returns>
        public object Clone()
        {
            SortableList Clone = new SortableList(_Comparer, _List.Capacity);
            Clone._List = (ArrayList)_List.Clone();
            Clone._AddDuplicates = _AddDuplicates;
            Clone._IsSorted = _IsSorted;
            Clone._KeepSorted = _KeepSorted;
            return Clone;
        }

        /// <summary>
        /// Idem IndexOf(object), but starting at a specified position in the list
        /// </summary>
        /// <param name="O">The object to locate.</param>
        /// <param name="Start">The index for start position.</param>
        /// <returns></returns>
        public int IndexOf(object O, int Start)
        {
            int Result = -1;
            if (_IsSorted)
            {
                Result = _List.BinarySearch(Start, _List.Count - Start, O, _Comparer);
                while (Result > Start && _List[Result - 1].Equals(O)) Result--; // We want to point at the first occurence
            }
            else Result = _List.IndexOf(O, Start);
            return Result;
        }

        /// <summary>
        /// Defines an equality for two objects
        /// </summary>
        public delegate bool Equality(object O1, object O2);

        /// <summary>
        /// Idem IndexOf(object), but with a specified equality function
        /// </summary>
        /// <param name="O">The object to locate.</param>
        /// <param name="AreEqual">Equality function to use for the search.</param>
        /// <returns></returns>
        public int IndexOf(object O, Equality AreEqual)
        {
            for (int i = 0; i < _List.Count; i++)
                if (AreEqual(_List[i], O)) return i;
            return -1;
        }

        /// <summary>
        /// Idem IndexOf(object), but with a start index and a specified equality function
        /// </summary>
        /// <param name="O">The object to locate.</param>
        /// <param name="Start">The index for start position.</param>
        /// <param name="AreEqual">Equality function to use for the search.</param>
        /// <returns></returns>
        public int IndexOf(object O, int Start, Equality AreEqual)
        {
            if (Start < 0 || Start >= _List.Count) throw new ArgumentException("Start index must belong to [0; Count-1].");
            for (int i = Start; i < _List.Count; i++)
                if (AreEqual(_List[i], O)) return i;
            return -1;
        }

        /// <summary>
        /// Idem <see cref="ArrayList">ArrayList</see>
        /// </summary>
        public int Capacity { get { return _List.Capacity; } set { _List.Capacity = value; } }

        /// <summary>
        /// Object.ToString() override.
        /// Build a string to represent the list.
        /// </summary>
        /// <returns>The string refecting the list.</returns>
        public override string ToString()
        {
            string OutString = "{";
            for (int i = 0; i < _List.Count; i++)
                OutString += _List[i].ToString() + (i != _List.Count - 1 ? "; " : "}");
            return OutString;
        }

        /// <summary>
        /// Object.Equals() override.
        /// </summary>
        /// <returns>true if object is equal to this, otherwise false.</returns>
        public override bool Equals(object O)
        {
            SortableList SL = (SortableList)O;
            if (SL.Count != Count) return false;
            for (int i = 0; i < Count; i++)
                if (!SL[i].Equals(this[i])) return false;
            return true;
        }

        /// <summary>
        /// Object.GetHashCode() override.
        /// </summary>
        /// <returns>HashCode value.</returns>
        public override int GetHashCode() { return _List.GetHashCode(); }

        /// <summary>
        /// Sorts the elements in the list using <see cref="ArrayList.Sort">ArrayList.Sort</see>.
        /// Does nothing if the list is already sorted.
        /// </summary>
        public void Sort()
        {
            if (_IsSorted) return;
            _List.Sort(_Comparer);
            _IsSorted = true;
        }

        /// <summary>
        /// If the <see cref="KeepSorted">KeepSorted</see> property is set to true, the object will be added at the right place.
        /// Else it will be appended to the list.
        /// </summary>
        /// <param name="C">The object to add.</param>
        /// <returns>The index where the object has been added.</returns>
        /// <exception cref="ArgumentException">The SortableList is set to use object's IComparable interface, and the specifed object does not implement this interface.</exception>
        public void AddRange(ICollection C)
        {
            if (_KeepSorted) foreach (object O in C) Add(O);
            else _List.AddRange(C);
        }

        /// <summary>
        /// Inserts a collection of objects at a specified index.
        /// Should not be used if the list is the KeepSorted property is set to true.
        /// </summary>
        /// <param name="Index">The index before which the objects must be added.</param>
        /// <param name="C">The object to add.</param>
        /// <exception cref="ArgumentException">The SortableList is set to use objects's IComparable interface, and the specifed object does not implement this interface.</exception>
        /// <exception cref="ArgumentOutOfRangeException">Index is less than zero or Index is greater than Count.</exception>
        /// <exception cref="InvalidOperationException">If the object is added at the specify index, the list will not be sorted any more and the <see cref="KeepSorted"/> property is set to true.</exception>
        public void InsertRange(int Index, ICollection C)
        {
            if (_KeepSorted) foreach (object O in C) Insert(Index++, O);
            else _List.InsertRange(Index, C);
        }

        /// <summary>
        /// Limits the number of occurrences of a specified value.
        /// Same values are equals according to the Equals() method of objects in the list.
        /// The first occurrences encountered are kept.
        /// </summary>
        /// <param name="Value">Value whose occurrences number must be limited.</param>
        /// <param name="NbValuesToKeep">Number of occurrences to keep</param>
        public void LimitNbOccurrences(object Value, int NbValuesToKeep)
        {
            if (Value == null) throw new ArgumentNullException("Value");
            int Pos = 0;
            while ((Pos = IndexOf(Value, Pos)) >= 0)
            {
                if (NbValuesToKeep <= 0) _List.RemoveAt(Pos);
                else { Pos++; NbValuesToKeep--; }
                if (_IsSorted && _Comparer.Compare(_List[Pos], Value) > 0) break; // No need to follow
            }
        }

        /// <summary>
        /// Removes all duplicates in the list.
        /// Each value encountered will have only one representant.
        /// </summary>
        public void RemoveDuplicates()
        {
            int PosIt;
            if (_IsSorted)
            {
                PosIt = 0;
                while (PosIt < Count - 1)
                {
                    if (_Comparer.Compare(this[PosIt], this[PosIt + 1]) == 0) RemoveAt(PosIt);
                    else PosIt++;
                }
            }
            else
            {
                int Left = 0;
                while (Left >= 0)
                {
                    PosIt = Left + 1;
                    while (PosIt > 0)
                    {
                        if (Left != PosIt && _Comparer.Compare(this[Left], this[PosIt]) == 0) RemoveAt(PosIt);
                        else PosIt++;
                    }
                    Left++;
                }
            }
        }

        /// <summary>
        /// Returns the object of the list whose value is minimum
        /// </summary>
        /// <returns>The minimum object in the list</returns>
        public int IndexOfMin()
        {
            int RetInt = -1;
            if (_List.Count > 0)
            {
                RetInt = 0;
                object RetObj = _List[0];
                if (!_IsSorted)
                {
                    for (int i = 1; i < _List.Count; i++)
                        if (_Comparer.Compare(RetObj, _List[i]) > 0)
                        {
                            RetObj = _List[i];
                            RetInt = i;
                        }
                }
            }
            return RetInt;
        }

        /// <summary>
        /// Returns the object of the list whose value is maximum
        /// </summary>
        /// <returns>The maximum object in the list</returns>
        public int IndexOfMax()
        {
            int RetInt = -1;
            if (_List.Count > 0)
            {
                RetInt = _List.Count - 1;
                object RetObj = _List[_List.Count - 1];
                if (!_IsSorted)
                {
                    for (int i = _List.Count - 2; i >= 0; i--)
                        if (_Comparer.Compare(RetObj, _List[i]) < 0)
                        {
                            RetObj = _List[i];
                            RetInt = i;
                        }
                }
            }
            return RetInt;
        }

        private bool ObjectIsCompliant(object O)
        {
            if (_UseObjectsComparison && !(O is IComparable)) throw new ArgumentException("The SortableList is set to use the IComparable interface of objects, and the object to add does not implement the IComparable interface.");
            if (!_AddDuplicates && Contains(O)) return false;
            return true;
        }

        private class Comparison : IComparer
        {
            public int Compare(object O1, object O2)
            {
                IComparable C = O1 as IComparable;
                return C.CompareTo(O2);
            }
        }

        private void InitProperties(IComparer Comparer, int Capacity)
        {
            if (Comparer != null)
            {
                _Comparer = Comparer;
                _UseObjectsComparison = false;
            }
            else
            {
                _Comparer = new Comparison();
                _UseObjectsComparison = true;
            }
            _List = Capacity > 0 ? new ArrayList(Capacity) : new ArrayList();
            _IsSorted = true;
            _KeepSorted = true;
            _AddDuplicates = true;
        }
    }

     class Approacher
    {

        static public bool AutoGoto(BotClient Client, Vector3 target3, float dist, long maxMs)
        {
            long endAt = Environment.TickCount + maxMs;
            Vector2 target = new Vector2(target3.X, target3.Y);
            float d = DistanceTo(Client, target);
            if (d < dist) return true;
            float ld = d;
            float traveled = 0.0f;
            uint x, y;
            // Vector2 P = Position();
            Utils.LongToUInts(Client.Network.CurrentSim.Handle, out x, out y);
            Client.Self.AutoPilot((ulong)(x + target.X), (ulong)(y + target.Y), target3.Z);
            bool AutoPilot = true;
            while (AutoPilot)
            {
                // float moved = Vector2.Distance(P, Position());
                // WriteLine("Moved=" + moved);
                if (d < dist)
                {
                    AutoPilot = false;
                }
                else
                    if (Environment.TickCount > endAt)
                    {
                        AutoPilot = false;
                    }
                    else
                    {
                        Application.DoEvents();
                        d = DistanceTo(Client, target);
                        traveled = ld - d;
                        if (traveled < 0)
                        {
                            AutoPilot = false;
                        }
                        Client.Self.Movement.TurnToward(target3);
                        ld = d;
                    }
                //    P = Position();
            }
            Client.Self.AutoPilotCancel();
            Client.WorldSystem.TheSimAvatar.StopMoving();
            Client.Self.Movement.TurnToward(target3);
            return true;
        }

        public static float DistanceTo(BotClient Client, Vector2 v2)
        {
            Vector2 cp = Position(Client);
            return Vector2.Distance(v2, cp);
        }

        public static Vector2 Position(BotClient Client)
        {
            return new Vector2(Client.Self.SimPosition.X, Client.Self.SimPosition.Y);
        }

    }

    class GotoVector
    {
        Vector3 myPos = new Vector3();
        Vector2 myPos0 = new Vector2();
        Vector3 target = new Vector3();
        Vector2 target0 = new Vector2();
        float diff, olddiff, saveolddiff;
        int startTime = 0;
        int duration = 10000;
        float maxDist = 2.0f;
        bool StillMoving = false;
        readonly BotClient Client;
        OpenMetaverse.ObjectManager.ObjectUpdatedCallback cb;

        public GotoVector(BotClient client, Vector3 targetXYZ, int maxTimeMS, float maxDistance)
        {
            Client = client;
            target = targetXYZ;
            duration = maxTimeMS;
            maxDist = maxDistance;
            cb = new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
        }

        //readonly OpenMetaverse.ObjectManager.ObjectUpdatedCallback cb = new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
        public void Goto()
        {
            //}

            //public override string Execute(string[] args, UUID fromAgentID)
            //{
            //    if (args.Length > 4 || args.Length < 3)
            //        return "Usage: FlyTo x y z [seconds]";

            //    if (!float.TryParse(args[0], out target.X) ||
            //        !float.TryParse(args[1], out target.Y) ||
            //        !float.TryParse(args[2], out target.Z))
            //    {
            //        return "Usage: FlyTo x y z [seconds]";
            //    }
            target0.X = target.X;
            target0.Y = target.Y;
            //target.Z = Client.Self.SimPosition.Z;

            //if (args.Length == 4 && Int32.TryParse(args[3], out duration))
            //    duration *= 1000;
            Client.Objects.OnObjectUpdated += cb;
            StillMoving = true;
            startTime = Environment.TickCount;
            //Client.Self.Movement.Fly = true;
            Client.Self.Movement.AtPos = true;
            Client.Self.Movement.AtNeg = false;
            ZMovement();
            Client.Self.Movement.TurnToward(target);
            while (StillMoving)
            {
                Thread.Sleep(7);
                DoTick();
                //Application.
                // StopMoving();
            }
            //ZMovement();
            //Client.Self.Movement.SendUpdate(false);

            Debug(string.Format("flying to {0} in {1} seconds", target.ToString(), duration / 1000));
        }


        bool InUpdate = false;
        object InUpdateLock = new Object();
        private void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (startTime == 0) return;
            if (update.LocalID == Client.Self.LocalID)
                DoTick();
        }

        private void DoTick()
        {
            {
                if (!StillMoving) return;
                lock (InUpdateLock)
                {
                    if (InUpdate) return;
                    InUpdate = true;
                }

                XYMovement();
                ZMovement();
                if (Client.Self.Movement.AtPos || Client.Self.Movement.AtNeg)
                {
                    Client.Self.Movement.TurnToward(target);
                    Debug("Fly xy ");
                }
                else if (Client.Self.Movement.UpPos || Client.Self.Movement.UpNeg)
                {
                    Client.Self.Movement.TurnToward(target);
                    //Client.Self.Movement.SendUpdate(false);
                    Debug("Fly z ");
                }
                else if (Vector3.Distance(target, Client.Self.SimPosition) <= maxDist)
                {
                    StopMoving();
                    Debug("At Target");
                }
                InUpdate = false;
            }
            if (Environment.TickCount - startTime > duration)
            {
                StopMoving();
                Debug("End Flyto");
            }
        }

        private bool XYMovement()
        {
            bool res = false;

            myPos = Client.Self.SimPosition;
            myPos0.X = myPos.X;
            myPos0.Y = myPos.Y;
            diff = Vector2.Distance(target0, myPos0);
            Vector2 vvel = new Vector2(Client.Self.Velocity.X, Client.Self.Velocity.Y);
            float vel = vvel.Length();
            if (diff >= 10.0)
            {
                Client.Self.Movement.AtPos = true;
                //  Client.Self.Movement.AtNeg = false;
                //if (Math.Abs(diff - olddiff) > 1.5) {
                //  Client.Self.Movement.AtPos = diff < olddiff;
                //  Client.Self.Movement.AtNeg = diff > olddiff;
                //} else if (!Client.Self.Movement.AtPos && !Client.Self.Movement.AtNeg) {
                //  Client.Self.Movement.AtPos = true;
                //  Client.Self.Movement.AtNeg = false;
                //}
                res = true;
            }
            else if (diff >= 2 && vel < 5)
            {
                Client.Self.Movement.AtPos = true;
            }
            else
            {
                Client.Self.Movement.AtPos = false;
                Client.Self.Movement.AtNeg = false;
            }
            saveolddiff = olddiff;
            olddiff = diff;
            return res;
        }

        private void ZMovement()
        {
            return;
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            float diffz = (target.Z - Client.Self.SimPosition.Z);
            if (diffz >= 20.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -20.0)
                Client.Self.Movement.UpNeg = true;
            else if (diffz >= +5.0 && Client.Self.Velocity.Z < +4.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -5.0 && Client.Self.Velocity.Z > -4.0)
                Client.Self.Movement.UpNeg = true;
            else if (diffz >= +2.0 && Client.Self.Velocity.Z < +1.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -2.0 && Client.Self.Velocity.Z > -1.0)
                Client.Self.Movement.UpNeg = true;
        }

        private void StopMoving()
        {
            Client.Objects.OnObjectUpdated -= cb;
            startTime = 0;
            Client.Self.Movement.AtPos = false;
            Client.Self.Movement.AtNeg = false;
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            Client.Self.Movement.SendUpdate(false);
            StillMoving = false;
        }

        private void Debug(string x)
        {
            //return; /* remove for debugging */
            Console.WriteLine(x + " {0,3:##0} {1,3:##0} {2,3:##0} diff {3,5:##0.0} olddiff {4,5:##0.0}  At:{5,5} {6,5}  Up:{7,5} {8,5}  v: {9} w: {10}",
        myPos.X, myPos.Y, myPos.Z, diff, saveolddiff,
        Client.Self.Movement.AtPos, Client.Self.Movement.AtNeg, Client.Self.Movement.UpPos, Client.Self.Movement.UpNeg,
        Client.Self.Velocity.ToString(), Client.Self.AngularVelocity.ToString());
        }
    }


    public class MovementToVector
    {
        public static bool MoveTo(BotClient bc, Vector3 targ, float dist)
        {
            if (true)
            {
                Approacher.AutoGoto(bc, targ, dist, 10000);
                return true;
            }

            MovementToVector mtv = new MovementToVector(bc, targ, dist);
            mtv.Goto();
            if (mtv.GetDistance() > dist) return false;
            return true;
        }
        // SimAvatar theAvatar;
        Vector3 Destination;
        Vector3 LastPosition;
        BotClient Client;
        //private AutoResetEvent Ready = new AutoResetEvent(false);
        Boolean justStopped = false;
        float lastDistance = Single.MaxValue;
        int autoPilotsRemaining = 6;

        float followDist = 2.0F;
        public MovementToVector(BotClient bc, Vector3 targ, float fd)
        {
            //theAvatar = bc;
            Client = bc;//.GetGridClient();
            Destination = targ;
            followDist = fd;
        }

        public void Goto()
        {
            float d = GetDistance();
            if (d < followDist)
            {
                followDist = d / 2;
            }
            //Client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
            tracker();
            StopMoving();
            Client.Self.Movement.TurnToward(Destination);
            if (madePhantom.Count > 0)
            {
                foreach (SimObject obj in madePhantom)
                {
                    obj.RestoreEnterable(Client.WorldSystem.TheSimAvatar);
                }
                madePhantom.Clear();
            }
        }

        private float GetDistance()
        {
            return Vector3.Distance(Client.Self.SimPosition, Destination);
        }

        void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            //{
            //    if (Vector3.Distance(Client.Self.BotPosition, Destination) > followDist)
            //    {
            //        //if (Vector3.Dist(LastTarget, Destination) > 1)
            //        //{
            //        //   LastTarget = Destination;
            //        //    Client.Self.Movement.TurnToward(Destination);
            //        //    Client.Self.Movement.AtPos = true;
            //        //    //Client.Self.AutoPilotCancel();
            //        //      Client.Self.Movement.UpdateInterval = 0;
            //        //    Client.Self.Movement.SendUpdate();
            //        //}
            //        //      Client.Self.AutoPilotLocal((int)Destination.X,
            //        //          (int)Destination.Y, Destination.Z);
            //    }
            //    else
            //    {
            //        //Client.Self.AutoPilotCancel();
            //    }
            //}
        }

        readonly ListAsSet<SimObject> madePhantom = new ListAsSet<SimObject>();

        void tracker()
        {
            float curDist = GetDistance();
            bool UseAutoPilot = false;
            float traveled = 10f;
            while (curDist > followDist && autoPilotsRemaining > 0)
            {
                LastPosition = Client.Self.SimPosition;
                if (UseAutoPilot)
                {
                    autoPilotsRemaining--;
                    if (autoPilotsRemaining > 0)
                    {
                        Console.WriteLine("AutoPilot due to traveled=" + traveled);
                        PhantomizeArea();
                        Client.Self.AutoPilotLocal((int)Destination.X, (int)Destination.Y, Destination.Z);
                        Thread.Sleep(2000);
                    }
                    else
                    {
                        UseAutoPilot = false;
                    }

                }
                if (!UseAutoPilot)
                {
                    Client.Self.AutoPilotCancel();
                    UpdateHeading();
                }
                Thread.Sleep(250);
                traveled = Vector3.Distance(LastPosition, Client.Self.SimPosition);
                if (traveled < 0.1)
                {
                    UseAutoPilot = true;
                }
                else
                {
                    UseAutoPilot = false;
                }

                curDist = GetDistance();

            }
            Client.Self.AutoPilotCancel();
        }

        private void PhantomizeArea()
        {
            foreach (SimObject obj in Client.WorldSystem.GetNearByObjects(Client.Self.SimPosition, null, 2.0f, true)) //should be false
            {
                madePhantom.AddTo(obj);
                obj.MakeEnterable(Client.WorldSystem.TheSimAvatar);
            }
        }

        private void UpdateHeading()
        {
            Random somthing = new Random(DateTime.Now.Millisecond);// We do stuff randomly here
            float curDist = GetDistance();

            if (lastDistance <= curDist)
            {
                //    StopMoving();
                //    followDist = curDist + 1.0F;
            }
            lastDistance = curDist;

            if (curDist > followDist)
            {

                //Client.Self.AnimationStop(Animations.WALK, true);
                //Client.Self.AnimationStart(Animations.WALK, true);
                //Client.Self.Movement.SendUpdate();
                if (curDist < (followDist * 1.25))
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(125);
                    Client.Self.Movement.Stop = true;
                    Client.Self.Movement.AtPos = false;
                    Client.Self.Movement.NudgeAtPos = true;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(100);
                    Client.Self.Movement.NudgeAtPos = false;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(100);
                }
                else
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.UpdateInterval = 0; //100
                    Client.Self.Movement.SendUpdate(true);
                    //(int)(25 * (1 + (curDist / followDist)))
                    Thread.Sleep(somthing.Next(25, 100));
                }
                justStopped = true;
            }
            else
            {
                if (justStopped)
                {
                    StopMoving();

                    Thread.Sleep(25);
                    justStopped = false;
                }
                else
                {
                    Thread.Sleep(100);
                }


            }
        }

        private void StopMoving()
        {
            Client.Self.Movement.TurnToward(Destination);
            Client.Self.Movement.AtPos = false;
            //Client.Self.Movement.UpdateInterval = 0;
            Client.Self.Movement.StandUp = true;
            //Client.Self.Movement.SendUpdate();
            Client.Self.Movement.FinishAnim = true;
            Client.Self.Movement.Stop = true;
            Client.Self.Movement.SendUpdate(true);

        }
    }
    
}
