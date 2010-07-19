using System;
using System.Collections.Generic;
using System.Threading;

namespace RTParser.Utils
{
    public class UndoStack
    {
        private Stack<ThreadStart> todo = null;
        private object objext = null;

        public bool pushValues(ISettingsDictionary settings, string n, Unifiable v)
        {
            bool local = settings.containsLocalCalled(n);
            bool containsAtAll = settings.containsSettingCalled(n);

            var oldValue = settings.grabSetting(n);

            if (oldValue.AsString() == v)
            {
                return false;
            }
            if (!local)
            {
                settings.addSetting(n, v);
                AddUndo(new ThreadStart(() =>
                                            {
                                                Unifiable newValue = settings.grabSetting(n);
                                                if (newValue != v)
                                                {
                                                    writeToLog("ERROR unexpected '" + n + "'='" + newValue + "' expecting '" +
                                                               v + "' ");
                                                }
                                                settings.removeSetting(n);
                                            }));
            }
            else
            {
                settings.updateSetting(n, v);
                AddUndo(new ThreadStart(() =>
                                            {
                                                Unifiable newValue = settings.grabSetting(n);
                                                if (newValue != v)
                                                {
                                                    writeToLog("ERROR unexpected '" + n + "'='" + newValue + "' expecting '" +
                                                               v + "' ");
                                                }
                                                settings.updateSetting(n, oldValue);
                                            }));
            }
            return true;
        }

        internal void writeToLog(string message, params object[] args)
        {
            RTPBot.writeDebugLine( message, args);
        }

        public void AddUndo(ThreadStart start)
        {
            lock (this)
            {
                if (todo == null) todo = new Stack<ThreadStart>();
                this.todo.Push(start);
            }
        }

        public void UndoAll()
        {
            lock (this)
            {
                if (todo == null) return;
                while (todo.Count > 0)
                {
                    ThreadStart undo = todo.Pop();
                    if (undo != null)
                    {
                        try
                        {
                            undo();
                        }
                        catch (Exception exception)
                        {
                            RTPBot.writeDebugLine("ERROR in UNDO " + exception);
                        }
                    }
                }
            }
        }

        static readonly Dictionary<object, UndoStack> ObjectUndoStacks = new Dictionary<object, UndoStack>();

        private UndoStack(object o)
        {
            objext = o;
        }

        public static UndoStack GetStackFor(object o)
        {
            lock (ObjectUndoStacks)
            {

                UndoStack u;
                if (!ObjectUndoStacks.TryGetValue(o, out u))
                {
                    u = ObjectUndoStacks[o] = new UndoStack(o);
                }
                return u;
            }
        }

        public static UndoStack FindStackFor(object o)
        {
            if (o==null) return null;
            lock (ObjectUndoStacks)
            {
                UndoStack u;
                if (ObjectUndoStacks.TryGetValue(o, out u))
                {
                    return u;
                }
                return null;
            }
        }

        public static void FindUndoAll(SubQuery query)
        {
            var u = FindStackFor(query);
            if (u != null) u.UndoAll();
        }
    }
}