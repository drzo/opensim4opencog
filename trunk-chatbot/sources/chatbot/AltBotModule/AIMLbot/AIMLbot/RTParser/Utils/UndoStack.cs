using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using AltAIMLbot.Variables;
using java.lang;
using Exception=System.Exception;
using String=System.String;

namespace AltAIMLbot.Utils
{
    public class NamedAction
    {
        public string Name;
        public ThreadStart Value;

        public NamedAction(string named, ThreadStart start)
        {
            Name = named;
            Value = start;
        }

        public override string ToString()
        {
            return "NamedThreadStart: " + Name ?? "NONAME" + " " + Value ?? "NULLVALUE";
        }

        internal void Invoke(string prefix)
        {
            try
            {
               // if (prefix != null) AltBot.writeDebugLine(prefix + "INVOKING: " + Name);
                Value.Invoke();
            }
            catch (Exception e)
            {

                AltBot.writeDebugLine("ERROR " + prefix + " in " + this + "\n -reason: " + e);
                
                
            }
        }
    }
    public class UndoStack
    {
        private static readonly Dictionary<object, UndoStack> ObjectUndoStacks = new Dictionary<object, UndoStack>();
        private readonly object objext;
        private Stack<NamedAction> todo;

        public override string ToString()
        {
            return "UndoStack " + Size + " for " + objext;
        }

        protected int Size
        {
            get
            {
                if (todo == null) return -1;
                return todo.Count;
            }
        }

        public UndoStack(object o)
        {
            objext = o;
            UndoStackHolder holder = AsUndoStackHolder(o);
            holder.UndoStackValue = this;           
            if (holder != null)
            {
                holder.UndoStackValue = this;
            }
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
            string debugStr = String.Format("pushValues {0} {1} {2}", settings, n, v);
            if (!local)
            {
                settings.addSetting(n, v);
                AddUndo(debugStr,() =>
                            {
                                Unifiable newValue = settings.grabSetting(n);
                                if (newValue != v)
                                {
                                    writeToLog("ERROR unexpected '" + n + "'='" +
                                        Unifiable.DescribeUnifiable(newValue) + "' expecting '" +
                                              Unifiable.DescribeUnifiable(v) + "' ");
                                }
                                settings.removeSetting(n);
                            });
            }
            else
            {
                settings.updateSetting(n, v);
                AddUndo(debugStr,() =>
                            {
                                Unifiable newValue = settings.grabSetting(n);
                                if (newValue != v)
                                {
                                    writeToLog("ERROR unexpected '" + n + "'='" +
                                        Unifiable.DescribeUnifiable(newValue) + "' expecting '" +
                                              Unifiable.DescribeUnifiable(v) + "' ");
                                }
                                settings.updateSetting(n, oldValue);
                            });
            }
            return true;
        }

        internal void writeToLog(string message, params object[] args)
        {
            AltBot.writeDebugLine(message, args);
        }

        public void AddUndo(string named, ThreadStart start)
        {
            lock (this)
            {
                if (todo == null) todo = new Stack<NamedAction>();
                this.todo.Push(new NamedAction(named, start));
            }
        }

        /*public void AddCommit(NamedAction start)
        {
            lock (this)
            {
                if (commits == null) commits = new Stack<NamedAction>();
                this.commits.Push(start);
            }
        }
        */
        public void UndoAll()
        {
            lock (this)
            {
                if (todo == null) return; 
                DoAll(todo);
            }
        }
        public static void DoAll(Stack<NamedAction> todo)
        {
            lock (todo)
            {
                while (todo.Count > 0)
                {
                    NamedAction undo = todo.Pop();
                    if (undo != null)
                    {
                        try
                        {
                            undo.Invoke("UNSTACK ");
                        }
                        catch (Exception exception)
                        {
                            AltBot.writeDebugLine("ERROR in DoAll " + exception);
                        }
                    }
                }
            }
        }

        public static UndoStack GetStackFor(object o)
        {
            UndoStackHolder holder = AsUndoStackHolder(o);
            //holder.ToString();
            UndoStack u;
            if (holder != null)
            {                
                u = holder.UndoStackValue;
                if (u != null) return u;
            }            
            lock (ObjectUndoStacks)
            {                
                if (!ObjectUndoStacks.TryGetValue(o, out u))
                {
                    u = ObjectUndoStacks[o] = new UndoStack(o);
                    if (holder != null) holder.UndoStackValue = u;
                }
                return u;
            }
        }

        [Deprecated]
        public static UndoStack FindStackFor(object o, bool remove)
        {
            if (o == null) return null;
            UndoStack u;
            UndoStackHolder holder = AsUndoStackHolder(o);
            holder.ToString();
            if (remove)
            {
                lock (ObjectUndoStacks)
                {
                    if (ObjectUndoStacks.TryGetValue(o, out u))
                    {
                        ObjectUndoStacks.Remove(o);
                        return u;
                    }
                }
            }
            if (holder != null)
            {
                u = holder.UndoStackValue;
                if (u != null) return u;
            }

            return null;
        }

        private static UndoStackHolder AsUndoStackHolder(object o)
        {
            if (o is UndoStackHolder) return (UndoStackHolder) o;
            lock(AnyUndoStackHolders)
            {
                UndoStackHolder holder;
                if (!AnyUndoStackHolders.TryGetValue(o, out holder))
                {
                    holder = AnyUndoStackHolders[o] = new AnyUndoStackHolder();
                    holder.UndoStackValue = new UndoStack(holder);
                }
                return holder;
            }
        }

        private static readonly Dictionary<object, UndoStackHolder> AnyUndoStackHolders =
            new Dictionary<object, UndoStackHolder>();


        public static void FindUndoAll(object o, bool remove)
        {
            UndoStackHolder holder = AsUndoStackHolder(o);
            UndoStack u;
            if (holder != null)
            {
                u = holder.UndoStackValue;
                if (u != null)
                {
                    u.UndoAll();           
                    return;
                }
            }       
            lock (ObjectUndoStacks)
            {
                if (ObjectUndoStacks.TryGetValue(o, out u))
                {
                    if (remove) ObjectUndoStacks.Remove(o);
                    u.UndoAll();
                }
            }
        }
    }
    public interface UndoStackHolder
    {
        UndoStack UndoStackValue { get; set; }
    }
    public class AnyUndoStackHolder:UndoStackHolder
    {
        public UndoStack UndoStackValue { get; set; }
    }
   
}