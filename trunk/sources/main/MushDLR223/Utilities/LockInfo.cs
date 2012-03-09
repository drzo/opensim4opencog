using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public class LockInfo
    {
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

        public static Action MonitorTryEnter(string lockType, object botUsers, TimeSpan maxWaitTryEnter)
        {
            //lock (LockInfos)
            {
                Thread currentThread = Thread.CurrentThread;
                bool needsExit = Monitor.TryEnter(botUsers, maxWaitTryEnter);

                if (!needsExit)
                {
                    lock (LockInfos)
                    {
                        LockInfo made = CantEnterUserThread(lockType, currentThread, botUsers);
                        return () => { };
                    }
                }
                else
                {
                    lock (LockInfos)
                    {
                        LockInfo made = LockInfo.EnterUserThread(lockType, currentThread, botUsers);
                        return () =>
                                   {
                                       lock (LockInfos)
                                       {
                                           try
                                           {
                                               LockInfo.ExitUserThread(lockType, made, botUsers);
                                           }
                                           finally
                                           {
                                               if (botUsers != null)
                                               {
                                                   Monitor.Exit(botUsers);
                                               }
                                           }
                                       }
                                   };
                    }
                }
            }
        }

        private static LockInfo CantEnterUserThread(string lockType, Thread currentThread, object botUsers)
        {
            LockInfo info = LockInfo.FindLockInfo(botUsers);
            string infostring = "Cannot get lock " + lockType;
            string newVariable = infostring + "in " + (info.StartTime - DateTime.Now) + " on " + info;
            writeDebugLine(newVariable);
            info.MoreInfoWST(infostring);
            return info;
        }

        public static LockInfo FindLockInfo(object botUsers)
        {
            LockInfo info = null;
            lock (LockInfos)
            {
                if (LockInfos.TryGetValue(botUsers, out info))
                {
                }
            }
            return info;
        }
        
        public static LockInfo EnterUserThread(string lockType, Thread currentThread, object botUsers)
        {
            lock (LockInfos)
            {
                LockInfo info = FindLockInfo(botUsers);
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
                info = CreateLockInfo(lockType, currentThread, botUsers);
                info.needsUnlock++;
                return info;
            }
        }

        internal static void writeDebugLine(string s)
        {
            DLRConsole.DebugWriteLine(s);
        }

        public static LockInfo ExitUserThread(string lockType, LockInfo lockInfo, object botUsers)
        {
            lock (LockInfos)
            {
                LockInfo info = FindLockInfo(botUsers);

                if (info != null)
                {
                    if (info.IsLockerCurrentThread)
                    {
                        if (botUsers != null)
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
        public static LockInfo CreateLockInfo(string lockType, Thread currentThread, object botUsers)
        {
            lock (LockInfos)
            {
                LockInfo lockinfo;
                if (!LockInfos.TryGetValue(botUsers, out lockinfo))
                {
                    return LockInfos[botUsers] = new LockInfo(lockType);
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
    }
}