using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using System.Diagnostics;
using System.Windows.Forms;
using AltAIMLbot;
using DcBus;
using MushDLR223.Utilities;
using RTParser;

/******************************************************************************************
AltAIMLBot -- Copyright (c) 2011-2012,Kino Coursey, Daxtron Labs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

namespace AltAIMLbot
{
    /// <summary>
    /// An implementation of the Cron service.
    /// </summary>
    [Serializable ]

    public class Cron
    {
        private ArrayList crontab_ar;
        private Hashtable crontab;
        private ArrayList processes;
        int lastMinute;
        int lastSecond;
        [NonSerialized ]
        public AltBot myBot;

        public static bool SuspendCrons;

        public Cron(AltBot deBot)
        { 
            myBot = deBot;
            crontab = new Hashtable();
            crontab_ar = new ArrayList();
            Console.WriteLine(" NEW CRONTAB: New CRON");

        }

        public void clear()
        {
            if (crontab != null) lock (crontab) crontab.Clear();
            if (processes != null) processes.Clear();
            Console.WriteLine(" CLEAR CRONTAB: clear");
        }
        public void start()
        {
            DateTime now;



            lastMinute = DateTime.Now.Minute - 1;
            lastSecond = DateTime.Now.Second - 1;

            if (crontab == null)
            {
                crontab = new Hashtable();
                Console.WriteLine(" NEW CRONTAB: start");

            }
            if (crontab_ar == null) crontab_ar = new ArrayList();
            if (processes == null) processes = new ArrayList();

           // try
          //  {
                //readCrontab(Application.StartupPath + "\\cron.tab");

                while (true)
                {
                    try
                    {
                        //Thread.Sleep(30000); // half a minute
                        Thread.Sleep(500); // half a second
                        if (Cron.SuspendCrons) continue;
                        if (myBot.isAcceptingUserInput)
                        {
                            if ((!myBot.inCritical) && (!myBot.blockCron) && (myBot.servitor.mLoadCompleteAndPersonalityShouldBeDefined))
                            {
                                now = DateTime.Now;
                                checkProcesses(now);
                                doCrontab(now);
                            }
                            else
                            {
                                // Console.WriteLine("--inCritical--");
                                Thread.Sleep(500);
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        reportError(e.ToString());
                    }
                }
           // }
           // catch (Exception e)
           // {
          //      reportError(e.ToString());
          //  }
        }

        public void checkProcesses(DateTime now)
        {
            ArrayList toRemove = new ArrayList();

            for (int i = 0; i < processes.Count; i++)
            {
                Process proc = (Process)processes[i];

                if (proc.HasExited)
                {
                    toRemove.Add(proc);

                    if (proc.ExitCode != 0)
                    {
                        reportError("The process " + proc.StartInfo.FileName + " " +
                            proc.StartInfo.Arguments + " returned with error " + proc.ExitCode.ToString());
                    }
                }
                else if (DateTime.Compare(proc.StartTime, DateTime.Now.Subtract(new System.TimeSpan(0, 20, 0))) < 0)
                {
                    reportError(proc.StartInfo.FileName + " takes longer than 20 minutes and will be killed.");
                    proc.Kill();
                }
            }

            for (int i = toRemove.Count - 1; i >= 0; i--)
            {
                processes.Remove(toRemove[i]);
            }
        }

        public void doCrontab(DateTime now)
        {
            //if (now.Minute.Equals(lastMinute))
            //    return;
            if ((now.Second.Equals(lastSecond)) && (now.Minute.Equals(lastMinute)) )
                return;
            //Console.WriteLine("doCrontab {0} {1} c={2}", now.Minute, now.Second,crontab.Count);
            // for loop: deal with the highly unexpected eventuality of
            // having lost more than one minute to unavailable processor time
           // for (int minute = (lastMinute == 59 ? 0 : lastMinute + 1); minute <= now.Minute; minute++)
           // {
           // IEnumerable<object> lockInfoCopyOf = null;
           // lock (crontab) lockInfoCopyOf = LockInfo.CopyOf<string>(crontab.Keys);
            string [] cronKeyList = new string [crontab.Keys.Count];
            lock (crontab) crontab.Keys.CopyTo(cronKeyList, 0);
            //foreach (String cronKey in lockInfoCopyOf)
            foreach (String cronKey in cronKeyList)
                {
                    ArrayList entry = (ArrayList) crontab[cronKey];
                    String timeMode = (String)entry[6];
                    if (timeMode.Contains("relative"))
                    {
                        DateTime startTime = (DateTime)entry[9];
                        TimeSpan diffTime = now - startTime;
                        // Note variation from absolute time below
                        if (contains(entry[0], startTime.Month - now.Month) &&
                            contains(entry[1], Math.Abs(startTime.DayOfYear - now.DayOfYear) % 30) &&
                            contains(entry[2], Math.Abs(startTime.DayOfYear - startTime.DayOfYear) % 7) &&
                            contains(entry[3], diffTime.Hours) &&
                            contains(entry[4], diffTime.Minutes) &&
                            contains(entry[5], diffTime.Seconds)
                            )
                        {
                            // yay, we get to execute something!
                            try
                            {
                                String behaviorName = (String)entry[7];
                                String behaviorArguments = (String)entry[8];
                                myBot.myBehaviors.runBotBehavior(behaviorName, myBot);
                                //Console.WriteLine("doCrontab Relative runBotBehavior {0} {1} :: {2} {3}", now.Minute, now.Second, behaviorName, behaviorArguments);
                                // if its one shot and we're done then remove from queue
                                if (timeMode.Contains("onetime"))
                                {
                                    lock (crontab) crontab.Remove(entry);
                                }
                            }
                            catch (Exception e)
                            {
                                Console.WriteLine("ERR CRON: {0}", e.Message);
                            }
                        }

                    }
                    else
                    {
                        // Assume its "absolute"
                        if (contains(entry[0], now.Month) &&
                            contains(entry[1], getMDay(now)) &&
                            contains(entry[2], getWDay(now)) &&
                            contains(entry[3], now.Hour) &&
                            contains(entry[4], now.Minute) &&
                            contains(entry[5], now.Second)
                            )
                        {
                            // yay, we get to execute something!
                            try
                            {
                                String behaviorName = (String)entry[7];
                                String behaviorArguments = (String)entry[8];
                                myBot.myBehaviors.runBotBehavior(behaviorName, myBot);
                                //Console.WriteLine("doCrontab Absolute runBotBehavior {0} {1} :: {2} {3}", now.Minute, now.Second, behaviorName, behaviorArguments);
                                // if its one shot and we're done then remove from queue
                                if (timeMode.Contains("onetime"))
                                {
                                    lock (crontab) crontab.Remove(entry);
                                }
                            }
                            catch (Exception e)
                            {
                                Console.WriteLine("ERR CRON: {0}", e.Message);
                            }
                        }
                    }
                }
            //}

            lastMinute = now.Minute;
            lastSecond = now.Second;
        }

        // sort of a macro to keep the if-statement above readable
        private bool contains(Object list, int val)
        {
            // -1 represents the star * from the crontab
            //return ((ArrayList)list).Contains(val) || ((ArrayList)list).Contains(-1);
            //KHC: quick patch for parser err or reload
            return ((ArrayList)list).Contains(val) || ((ArrayList)list).Contains(-1) || ((ArrayList)list).Contains(1);
        }
        private string decodeList(Object list)
        {
            string decoded = "";
            foreach (int elem in ((ArrayList)list))
            {
                if (decoded.Length == 0)
                {
                    decoded += elem.ToString();
                }
                else
                {
                    decoded += ","+elem.ToString();
                }
            }
            return decoded;
        }
        private int getMDay(DateTime date)
        {
            date.AddMonths(-(date.Month - 1));
            return date.DayOfYear;
        }

        private int getWDay(DateTime date)
        {
            if (date.DayOfWeek.Equals(DayOfWeek.Sunday))
                return 7;
            else
                return (int)date.DayOfWeek;
        }

        public void addLine(string line)
        {
            ArrayList seconds,minutes, hours, mDays, months, wDays;

            line = line.Trim();

            if (line.Length == 0 || line.StartsWith("#"))
                return;

            // re-escape space- and backslash-escapes in a cheap fashion
            line = line.Replace("\\\\", "<BACKSLASH>");
            line = line.Replace("\\ ", "<SPACE>");

            // split string on whitespace
            String[] cols = line.Split(new char[] { ' ', '\t' });

            for (int i = 0; i < cols.Length; i++)
            {
                cols[i] = cols[i].Replace("<BACKSLASH>", "\\");
                cols[i] = cols[i].Replace("<SPACE>", " ");
            }

            if (cols.Length < 7)
            {
                reportError("Parse error in crontab (line too short).");
                crontab = new Hashtable ();
                crontab_ar = new ArrayList();
            }

            if (crontab == null)
            {
                crontab = new Hashtable();
                crontab_ar = new ArrayList();
                Console.WriteLine(" NEW CRONTAB: addline");
            }
            seconds = parseTimes(cols[0], 0, 59);
            minutes = parseTimes(cols[1], 0, 59);
            hours = parseTimes(cols[2], 0, 23);
            months = parseTimes(cols[4], 1, 12);

            if (!cols[3].Equals("*") && cols[4].Equals("*"))
            {
                // every n monthdays, disregarding weekdays
                mDays = parseTimes(cols[3], 1, 31);
                wDays = new ArrayList();
                wDays.Add(-1); // empty value
            }
            else if (cols[3].Equals("*") && !cols[4].Equals("*"))
            {
                // every n weekdays, disregarding monthdays
                mDays = new ArrayList();
                mDays.Add(-1); // empty value
                wDays = parseTimes(cols[5], 1, 7); // 60 * 24 * 7
            }
            else
            {
                // every n weekdays, every m monthdays
                mDays = parseTimes(cols[3], 1, 31);
                wDays = parseTimes(cols[5], 1, 7); // 60 * 24 * 7
            }

            String args = "";

            for (int i = 8; i < cols.Length; i++)
                args += " " + cols[i];

            ArrayList entry = new ArrayList(8);

            entry.Add(months); //0
            entry.Add(mDays);  //1
            entry.Add(wDays);  //2
            entry.Add(hours);  //3
            entry.Add(minutes);  //4
            entry.Add(seconds);  //5
            entry.Add(cols[6]);  //6 mode
            entry.Add(cols[7]);  //6 Behavior ID
            entry.Add(args);   //7
            entry.Add(DateTime.Now); //8 For Elapsed Time

            //crontab.Add(cols[7], entry);
            Console.WriteLine("AddCron :{0}", cols[7]);

            try
            {
                lock (crontab) crontab[cols[7]] = entry;
            }
            catch (Exception e)
            {
                reportError(" while adding '"+ line+"'"+e.ToString());
            }
        }
        
        public void readCrontab(String filename)
        {
            StreamReader sr;

            try
            {
                String line;

                sr = new StreamReader(filename);

                while ((line = sr.ReadLine()) != null)
                {
                    addLine(line);
                }

                sr.Close();
            }
            catch (Exception e)
            {
                reportError(e.ToString());
            }
        }

        public ArrayList parseTimes(String line, int startNr, int maxNr)
        {
            ArrayList vals = new ArrayList();
            String[] list, parts;
            try
            {
                list = line.Split(new char[] { ',' });

                foreach (String entry in list)
                {
                    int start, end, interval;

                    parts = entry.Split(new char[] {'-', '/'}, StringSplitOptions.RemoveEmptyEntries);

                    if (parts[0].Equals("*"))
                    {
                        if (parts.Length > 1)
                        {
                            start = startNr;
                            end = maxNr;

                            interval = int.Parse(parts[1]);
                        }
                        else
                        {
                            // put a -1 in place
                            start = -1;
                            end = -1;
                            interval = 1;
                        }
                    }
                    else
                    {
                        // format is 0-8/2
                        start = int.Parse(parts[0]);
                        end = parts.Length > 1 ? int.Parse(parts[1]) : int.Parse(parts[0]);
                        interval = parts.Length > 2 ? int.Parse(parts[2]) : 1;
                    }

                    for (int i = start; i <= end; i += interval)
                    {
                        vals.Add(i);
                    }
                }
            }
            catch(Exception e)
            {
                if (line == "-1")
                {
                    vals.Add(-1);
                }
                else
                {
                    vals.Add(1);
                }
            }
            return vals;
        }

        public void reportError(String error)
        {
            // Error reporting is left up to you; this is a case apart
            // (besides, my implementation was too specific to post here)
            Console.WriteLine("ERR CRON : {0}", error);
            Thread.Sleep(5000);
        }

        public List<string> cronXmlList()
        {
            List<string> XList = new List<string>();
            lock (crontab) foreach (String cronKey in crontab.Keys)
            {
                ArrayList entry = (ArrayList)crontab[cronKey];
                string dat = String.Format("<crontag timeline=\"{5} {4} {3} {2} {1} {0}\" mode=\"{6}\" id=\"{7}\"> {8} </crontag>",
                  decodeList(entry[0]),decodeList( entry[1]),
                  decodeList(entry[2]), decodeList(entry[3]),
                  decodeList(entry[4]), decodeList(entry[5]),
                  entry[6], cronKey, entry[8]);
                XList.Add(dat);
            }
            return XList ;
        }

        public Dictionary<string,string> cronXmlDictionary()
        {
            Dictionary<string, string> XDict = new Dictionary<string, string>();
            lock (crontab) foreach (String cronKey in crontab.Keys)
                {
                    ArrayList entry = (ArrayList)crontab[cronKey];
                    string dat = String.Format("<crontag timeline=\"{5} {4} {3} {2} {1} {0}\" mode=\"{6}\" id=\"{7}\"> {8} </crontag>",
                      decodeList(entry[0]), decodeList(entry[1]),
                      decodeList(entry[2]), decodeList(entry[3]),
                      decodeList(entry[4]), decodeList(entry[5]),
                      entry[6], cronKey, entry[8]);
                    XDict[cronKey] = dat;
                }
            return XDict;
        }
        public Dictionary<string, string> cronLinesDictionary()
        {
            Dictionary<string, string> XDict = new Dictionary<string, string>();
            lock (crontab) foreach (String cronKey in crontab.Keys)
                {
                    ArrayList entry = (ArrayList)crontab[cronKey];
                    string dat = String.Format("{5} {4} {3} {2} {1} {0} {6} {7} {8}",
                      decodeList(entry[0]), decodeList(entry[1]),
                      decodeList(entry[2]), decodeList(entry[3]),
                      decodeList(entry[4]), decodeList(entry[5]),
                      entry[6], cronKey, entry[8]);
                    XDict[cronKey] = dat;
                }
            return XDict;
        }
    }
}