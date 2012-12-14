using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
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
    }

}