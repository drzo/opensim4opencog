using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using RTParser.Variables;

namespace RTParser.Utils
{
    public class UndoStack
    {
        private static readonly Dictionary<object, UndoStack> ObjectUndoStacks = new Dictionary<object, UndoStack>();
        private object objext;
        private Stack<ThreadStart> todo;
        private Stack<ThreadStart> commits;

        private UndoStack(object o)
        {
            objext = o;
        }

        public bool pushValues(ISettingsDictionary settings, string n, Unifiable v)
        {
            bool local = settings.containsLocalCalled(n);
            bool containsAtAll = settings.containsSettingCalled(n);

            Unifiable oldValue = settings.grabSetting(n);
            if (oldValue == v)
            {
                return false;
            }
            if (!local)
            {
                settings.addSetting(n, v);
                AddUndo(() =>
                            {
                                Unifiable newValue = settings.grabSetting(n);
                                if (newValue != v)
                                {
                                    writeToLog("ERROR unexpected '" + n + "'='" + newValue + "' expecting '" +
                                               v + "' ");
                                }
                                settings.removeSetting(n);
                            });
            }
            else
            {
                settings.updateSetting(n, v);
                AddUndo(() =>
                            {
                                Unifiable newValue = settings.grabSetting(n);
                                if (newValue != v)
                                {
                                    writeToLog("ERROR unexpected '" + n + "'='" + newValue + "' expecting '" +
                                               v + "' ");
                                }
                                settings.updateSetting(n, oldValue);
                            });
            }
            return true;
        }

        internal void writeToLog(string message, params object[] args)
        {
            RTPBot.writeDebugLine(message, args);
        }

        public void AddUndo(ThreadStart start)
        {
            lock (this)
            {
                if (todo == null) todo = new Stack<ThreadStart>();
                this.todo.Push(start);
            }
        }

        public void AddCommit(ThreadStart start)
        {
            lock (this)
            {
                if (commits == null) commits = new Stack<ThreadStart>();
                this.commits.Push(start);
            }
        }

        public void UndoAll()
        {
            lock (this)
            {
                if (todo == null) return; 
                DoAll(todo);
            }
        }
        public static void DoAll(Stack<ThreadStart> todo)
        {
            lock (todo)
            {
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
                            RTPBot.writeDebugLine("ERROR in DoAll " + exception);
                        }
                    }
                }
            }
        }


        public void CommitAll()
        {
            lock (this)
            {
                if (commits == null) return;
                DoAll(commits);
            }
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
            if (o == null) return null;
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

        public static void FindUndoAll(object query)
        {
            UndoStack u = FindStackFor(query);
            if (u != null) u.UndoAll();
        }
    }
}