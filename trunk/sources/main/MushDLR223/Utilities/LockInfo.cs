using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Xml;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public struct Boxed<T>
    {
        public FirstUse<T> io;
        public static implicit operator T(Boxed<T> value)
        {
            return value.io.Value;
        }
        public static implicit operator Boxed<T>(Func<T> value)
        {
            return (Boxed<T>)(FirstUse<T>)value;
        }

    }
    public static class InitOnceExtensionsTest
    {
        static void Main()
        {
            var m = new Func<int>(() => 1);
            var vg = m.ToFirstUse();
            var v = vg.Value;
            int i = vg;

            int? foo = 1;
            var f3 = foo + 2;
            FirstUse<int> bi = (Func<int>)(() => 1);
            int f4 = 1 + bi;
            int f5 = (int)new Nullable<int>(f4).ToFirstUseN();
            int f6 = (int)((int?)f4).ToFirstUseN();           
        }
    }
    public static class InitOnceExtensions
    {
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this Func<T> func)
        {
            return func;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this T? func) where T : struct
        {
            return (Func<T>)(() => func.Value);
        }
        public static T? ToFirstUseN<T>(this T? func) where T : struct
        {
            return new Nullable<T>(new FirstUse<T>(() => func.Value));
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this Boxed<T> func)
        {
            return func.io;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this FirstUse<T> func)
        {
            return func;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this object func)
        {
            return (Delegate)func;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this Delegate func)
        {
            return (Func<T>)func;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(AnyFunc func)
        {
            return (Func<T>)(() => (T)func());
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(AnyFunc<T> func)
        {
            return (Func<T>) (() => func());
           
        }
    }
    
    public delegate object AnyFunc(params object[] args);
    public delegate T AnyFunc<T>(params object[] args);

    public class FirstUse<T> // : Nullable<T>
    {
        public FirstUse()
        {
            
        }
        public static implicit operator T(FirstUse<T> value)
        {
            return value.Value;
        } 
        public static implicit operator Boxed<T>(FirstUse<T> value)
        {
            return new Boxed<T>() { io = value };
        }
        public static implicit operator Func<T>(FirstUse<T> value)
        {
            return () => value.Value;
        }
        public static implicit operator FirstUse<T>(T value)
        {
            return new FirstUse<T>()
            {
                m_value = value,
                m_isValid = true,
                m_valueFactory = (() => value)
            };

        }
        public static implicit operator FirstUse<T>(Func<T> value)
        {
            return new FirstUse<T>()
            {
                m_valueFactory = value
            };
        }
        public static implicit operator FirstUse<T>(Delegate value)
        {
            return new FirstUse<T>()
            {
                m_valueFactory = (Func<T>)value
            };
        }

        private T m_value;
        private bool m_isValid;
        // a delegate that returns the created value, if null the created value will be default(T)
        private Func<T> m_valueFactory;

        public FirstUse(Func<T> func)
        {
            m_valueFactory = func;
            m_value = default(T);
            m_isValid = false;
        }
        public bool HasValue
        {
            get
            {
                // once we have m_isValid true.. the value is valid!
                if (m_isValid) return true;
                // if m_isValid is false we might need to be waiting on a m_valueFactory invokation on another thread
                lock (m_valueFactory.GetType()) return m_isValid;
            }
        }
        public T Value
        {
            get
            {
                // once we have m_isValid true.. the value is valid!
                //if (m_isValid) return m_value;
                // if m_isValid is false we might need to be waiting on a m_valueFactory invokation on another thread
                lock (m_valueFactory.GetType())
                {
                    if (!m_isValid)
                    {
                        m_value = m_valueFactory();
                        m_isValid = true;
                    }
                    return m_value;
                }
            }
        }

        public static FirstUse<T> F(Func<T> func)
        {
            return new FirstUse<T>(func);
        }
    }

    public class LockInfo
    {

        public static object Watch(object o, params string[] named)
        {
            return o;
            //return Swicli.Library.LockInfo.Watch(o, named);
        }
    

        public static IList<T> CopyOf<T>(List<T> list)
        {
            if (list == null) return new List<T>();
            lock (list)
            {
                return list.ToArray();
            }
        }
        public static IEnumerable<object> CopyOf<T>(System.Collections.ICollection list)
        {
            var copy = new List<object>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var o in copy)
                {
                    copy.Add(o);
                }
            }
            return copy;
        }

        public static IList<T> CopyOf<T>(IEnumerable<T> list)
        {
            var copy = new List<T>();
            if (list == null) return copy;
            lock (list)
            {
                copy.AddRange(list);
            }
            return copy;
        }

        public static IDictionary<K, V> CopyOf<K, V>(IDictionary<K, V> list)
        {
            var copy = new Dictionary<K, V>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var kv in list)
                {
                    copy.Add(kv.Key, kv.Value);
                }
            }
            return copy;
        }

        public static bool DontRealyLock = true;
        public static R WeaklyLock<R>(object lockObject, TimeSpan maxWaitTryEnter, Func<R> action, Func<string> operationType, OutputDelegate output)
        {
            if (DontRealyLock)
            {
                return action();
            }
            Action needsExit = MonitorTryEnter(operationType(), lockObject, maxWaitTryEnter);
            try
            {
                return action();
            }
            finally
            {
                needsExit();
            }
        }
        public static void WeaklyLock(object lockObject, TimeSpan maxWaitTryEnter, Action action, Func<string> operationType, OutputDelegate output)
        {
            if (DontRealyLock)
            {
                action(); ;
                return;
            }
            Action needsExit = MonitorTryEnter(operationType(), lockObject, maxWaitTryEnter);
            try
            {
                action();
            }
            finally
            {
                needsExit();
            }
        }

        public static Action MonitorTryEnter(string lockType, object codeLock, TimeSpan maxWaitTryEnter)
        {
            //lock (LockInfos)
            {
                Thread currentThread = Thread.CurrentThread;
                bool needsExit = Monitor.TryEnter(codeLock, maxWaitTryEnter);

                if (!needsExit)
                {
                    lock (LockInfos)
                    {
                        LockInfo made = CantEnterUserThread(lockType, currentThread, codeLock);
                        return () => { };
                    }
                }
                else
                {
                    lock (LockInfos)
                    {
                        LockInfo made = LockInfo.EnterUserThread(lockType, currentThread, codeLock);
                        return () =>
                                   {
                                       lock (LockInfos)
                                       {
                                           try
                                           {
                                               LockInfo.ExitUserThread(lockType, made, codeLock);
                                           }
                                           finally
                                           {
                                               if (codeLock != null)
                                               {
                                                   Monitor.Exit(codeLock);
                                               }
                                           }
                                       }
                                   };
                    }
                }
            }
        }

        private static LockInfo CantEnterUserThread(string lockType, Thread currentThread, object codeLock)
        {
            LockInfo info = LockInfo.FindLockInfo(codeLock);
            string infostring = "Cannot get lock " + lockType;
            string newVariable = infostring + "in " + (info.StartTime - DateTime.Now) + " on " + info;
            writeDebugLine(newVariable);
            info.MoreInfoWST(infostring);
            return info;
        }

        public static LockInfo FindLockInfo(object codeLock)
        {
            LockInfo info = null;
            lock (LockInfos)
            {
                if (LockInfos.TryGetValue(codeLock, out info))
                {
                }
            }
            return info;
        }
        
        public static LockInfo EnterUserThread(string lockType, Thread currentThread, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo info = FindLockInfo(codeLock);
                if (info != null)
                {
                    if (info.FirstThread == Thread.CurrentThread)
                    {
                        info.MoreInfo("entering " + lockType);
                        info.needsUnlock++;
                        return info;
                    }
                    {
                        if (!DLRConsole.SkipStackTraces) info.MoreInfoWST("side-entering " + lockType);
                        /*
                        string here = LockInfo.GetStackTraceString();
                        string there = info.StartStack.ToString();

                        string newVariable = "FoundLock ??! " + lockType + " " + info;
                        writeDebugLine(newVariable);
                        writeDebugLine("here: " + here);
                        writeDebugLine("there: " + there);
                        writeDebugLine(newVariable);                                  
                        info.MoreInfo("Weird Entry " + lockType);
                         */
                        info.needsUnlock++;
                        return info;
                    }
                }
                info = CreateLockInfo(lockType, codeLock);
                info.needsUnlock++;
                return info;
            }
        }

        internal static void writeDebugLine(string s)
        {
            DLRConsole.DebugWriteLine(s);
        }

        public static LockInfo ExitUserThread(string lockType, LockInfo lockInfo, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo info = FindLockInfo(codeLock);

                if (info != null)
                {
                    if (info.IsLockerCurrentThread)
                    {
                        if (codeLock != null)
                        {
                            info.needsUnlock--;
                            //if (info.needsUnlock==0)
                            {
                                info.wasUnlocked++;
                            } 
                            
                            if (info.needsUnlock == 0)
                            {
                                info.MoreInfo("Exiting " + lockType);
                                LockInfos.Remove(info);
                            } else
                            {
                                info.MoreInfo("departing " + lockType);
                            }
                        }
                    }
                }
                else
                {
                    writeDebugLine("Cannot exit lock " + lockType + " " + lockInfo);
                }
                return info;
            }
        }
        private static readonly Dictionary<object, LockInfo> LockInfos = new Dictionary<object, LockInfo>();
        public static LockInfo CreateLockInfo(string lockType, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo lockinfo;
                if (!LockInfos.TryGetValue(codeLock, out lockinfo))
                {
                    return LockInfos[codeLock] = new LockInfo(lockType);
                }
                return lockinfo;
            }
        }
        public LockInfo(string named)
        {
            Name = named;
            StartTime = DateTime.Now;
            FirstThread = Thread.CurrentThread;
            if (DLRConsole.SkipStackTraces) return;
            StartStack = new StackTrace(true);
            MoreInfoWST("Created" + this);
        }
        public bool IsLockerCurrentThread
        {
            get
            {
                return Thread.CurrentThread == FirstThread;
            }
        }
        public override string ToString()
        {
            string s = "LockInfo " + Name + "\n" + (DateTime.Now - StartTime);
            //s += GetExtraInfo();
            return s;
        }

        public static bool TestLock(string named, object busyTrackingLock, TimeSpan timeSpan)
        {
            if (DontRealyLock) return true;
            // return;
            if (Monitor.TryEnter(busyTrackingLock, timeSpan))
            {
                Monitor.Exit(busyTrackingLock);
                return true;
            }
            DLRConsole.DebugWriteLine("ERROR: Cant get into " + named + " " + busyTrackingLock);
            return false;
        }

        public string GetExtraInfo()
        {
            StringBuilder sb = new StringBuilder();
            lock(Waiters)
            {
                foreach (string infostring in Waiters)
                {
                    sb.AppendLine(infostring);
                }
            }
            return sb.ToString();
        }

        public readonly Thread FirstThread;
        public readonly DateTime StartTime;
        public StackTrace StartStack;
        public int wasUnlocked = 0;
        public int needsUnlock = 0;
        public readonly string Name;
        public readonly List<string > Waiters = new List<string>();

        public void MoreInfoWST(string s)
        {
            string toString1 = GetStackTraceString();
            MoreInfo(s + "\n" + toString1);
        }

        public static string GetStackTraceString()
        {
            return (new StackTrace(true)).ToString();
        }

        public void MoreInfo(string p)
        {
            lock (Waiters)
                Waiters.Add(p);
        }

        public static void EnsureLocked(object lockObj, Action<string> bad)
        {
            string s = CheckLocked(lockObj);
            if (s == null) return;
            if (bad != null) bad(s);
        }
        static Dictionary<object, Thread> myPool = new Dictionary<object, Thread>();
        static public string CheckLocked(object lockObj)
        {
            if (DontRealyLock) return null;
            bool[] o = { false };
            Thread n = new Thread(() => isUnlocked(o, lockObj));
            n.Start();
            if (!n.Join(2000))
            {
                return "Could not join";
            }
            bool lockingIt = o[0];
            if (!lockingIt)
            {
                return "Everyone forgot to lock";
            }
            if (!Monitor.TryEnter(lockObj))
            {
                return "We forgot to lock";
            }
            else
            {
                Monitor.Exit(lockObj);
                return null;
            }
        }
        static private void isUnlocked(bool[] o, object lockObj)
        {
            if (Monitor.TryEnter(lockObj))
            {
                Monitor.Exit(lockObj);
                o[0] = false;
            }
            else
            {
                o[0] = true;
            }
        }

        public static T WithLock<T>(object lockObj, Func<T> func)
        {
            lock (lockObj) return func();
        }
    }

}