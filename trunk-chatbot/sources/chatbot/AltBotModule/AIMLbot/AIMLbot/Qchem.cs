using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Collections;
using System.IO;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using System.Timers;
using DcBus;
using LogicalParticleFilter1;

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
    [Serializable]
    public class ChemTrace
    {
        public Qchem ourSoup = null;
        System.Timers.Timer Clock = null;
        public Hashtable trace = new Hashtable();
        public SIProlog prologEngine = null;
        public ChemTrace(Qchem _soup)
        {
            ourSoup = _soup;
        }
        public void startMonitor()
        {
                    if (Clock == null)
            {
                Clock = new System.Timers.Timer();
                Clock.Elapsed += new ElapsedEventHandler(Timer_Tick);
            }
            Clock.Interval =1000;
            Clock.Enabled = true;
            tickMonitor();
            updateProlog();
        }
        public void stopMonitor()
        {
            Clock.Enabled = false;
            Clock.Stop();
        }
        public void Timer_Tick(object sender, EventArgs eArgs)
        {
            if (sender == Clock)
            {
                tickMonitor();
                updateProlog();
            }
        }
        public void tickMonitor()
        {
            lock (trace)
            {
                lock (ourSoup.Chemicals)
                {
                    foreach (string ID in ourSoup.Chemicals.Keys)
                    {
                        double v1 = (double)ourSoup.Chemicals[ID];
                        if (!trace.ContainsKey(ID))
                        {
                            //double[] timeseries = new double[100];
                            Queue<double> new_timeseries = new Queue<double>();
                            trace[ID] = new_timeseries;
                        }
                        Queue<double> timeseries = (Queue<double>)trace[ID];
                        timeseries.Enqueue(v1);
                        while (timeseries.Count > 120)
                        {
                            timeseries.Dequeue();
                        }
                    }
                }
            }
        }
        public void updateProlog()
        {
            if (prologEngine ==null) return;
            string spindle = "bioTraceMt";
            string commonMt = "bioCommonMt";
            string bioNow = "bioNowMt";
            prologEngine.insertKB("plot(X,Y):-trace(ID,X,Y).\n",commonMt);
            lock (trace)
            {
                foreach (string ID in trace.Keys)
                {
                    Queue<double> timeseries = (Queue<double>)trace[ID];
                    double[] Vlist = timeseries.ToArray();
                    string tlog ="" ;
                    string mt = String.Format("biotrace_{0}", ID);
                    int vlen = Vlist.Length;
                    for (int i = 0; i < vlen; i++)
                    {
                        double v1 = Vlist[i];
                        string point = String.Format("trace({0},{1},{2}).\n", ID, ourSoup.biochemticks - (vlen-i), v1);
                        tlog += point;
                    }
                    prologEngine.insertKB(tlog, mt);
                    prologEngine.connectMT(spindle, mt);
                    prologEngine.connectMT(mt, commonMt );
                }
                string tlog2 = "nowTick("+ourSoup.biochemticks +").\n";
                lock(ourSoup.Chemicals)
                {
                    foreach (string ID in ourSoup.Chemicals.Keys)
                    {
                        double v1 = (double)ourSoup.Chemicals[ID];
                        string point = String.Format("bioNow({0},{1}).\n", ID, v1);
                        tlog2 += point;
                    }
                    prologEngine.insertKB(tlog2, bioNow);
                    prologEngine.connectMT(spindle, bioNow);
                }
            }
        }
    }
    public class Qchem
    {
        public HashtableSerailizable Reactions = new HashtableSerailizable();
        public HashtableSerailizable Chemicals = new HashtableSerailizable();
        public HashtableSerailizable Halflifes = new HashtableSerailizable();
        public HashtableSerailizable DriveSet = new HashtableSerailizable();
        public HashtableSerailizable EmotiveSet = new HashtableSerailizable();
        public HashtableSerailizable SemSet = new HashtableSerailizable();
        public HashtableSerailizable InSet = new HashtableSerailizable();
        public HashtableSerailizable OutSet = new HashtableSerailizable();
        TextWriter gtw;
        UTF8Encoding encoding = new UTF8Encoding();
        public int biochemticks = 0;
        public int genID = 0;
         // KHC:TOFIX
        //public Form1 outForm=null;

        public HashtableSerailizable BlackBoard = new HashtableSerailizable(); // raw I/O blackboard
        public HashtableSerailizable subsumptionBlackBoard = new HashtableSerailizable(); // parsed so only the highest value for each is visible
        public HashtableSerailizable Settings = new HashtableSerailizable();

        public List<KeyValuePair<String, int>> SortedBlackBoard = new List<KeyValuePair<String, int>>();
        public HashtableSerailizable IORules = new HashtableSerailizable();
        //public Breather myBreather = null;

        public RChem m_RChem = null;

        [NonSerialized]
        System.Timers.Timer Clock = null;
        [NonSerialized]
        public ListBox myWatchBox = null;
        [NonSerialized]
        public SIProlog prologEngine = null;
        [NonSerialized]
        public ChemTrace tracer = null;

        string cacheIP = "127.0.0.1";
        public string watchMt = null;

        // References:
        // http://code.google.com/p/openc2e/
        // https://code.launchpad.net/openc2e
        // https://github.com/ccdevnet/openc2e

        // lookup table, snaffled from real creatures
        // TODO: work out if these are meaningful values :)
         int [] c1rates = new int[32]{
	        0, 0x32A5, 0x71DD, 0xAABB, 0xD110, 0xE758, 0xF35C,
	        0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999,
	        0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999,
	        0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999, 0xF999,
	        0xFFFF};

         public int map2c1 (int r1)
         {
             return r1 / 8;
         }

        public Qchem()
        {
            cacheIP = "127.0.0.1";
        }
        public Qchem(string ipAddress)
        {
            cacheIP = ipAddress;
        }

        public void initRChem()
        {
            // Initialize our link to the RemoteChem Proxy
            //m_RChem = new RChem(true);
            m_RChem = new RChem(cacheIP,true);
            m_RChem.m_cBus.initQueue("CHEMCMD");
            m_RChem.m_cBus.watchQueue("CHEMCMD");
            m_RChem.m_cBus.registerWatcher("CHEMCMD", interepretCmdList);
            update_Rchem(true);
            m_RChem.m_cBus.startWatch();
        }

        // Our simulator real-time heartbeat

        public void startEngine()
        {
            if (Clock == null)
            {
                Clock = new System.Timers.Timer();
                Clock.Elapsed += new ElapsedEventHandler(Timer_Tick);
            }
            //Clock.Interval = 50;
            Clock.Interval = 333;
            Clock.Enabled = true;
            if (tracer != null)
            {
                tracer.startMonitor();
            }
            update_Rchem(true);
        }
        public void stopEngine()
        {
            Clock.Enabled = false;
            Clock.Stop();
            if (tracer != null)
            {
                tracer.stopMonitor();
            }
        }

        public void Timer_Tick(object sender, EventArgs eArgs)
        {
            if (sender == Clock)
            {
                tickBiochemistry();
               
            }
        }

        public void AddLink(string ID, SoupIORule ER)
        {
            if (ID.Length == 0)
            {
                ID = String.Format("SIO{0}",(genID++));
               // ER.ID = ID;
            }
            IORules[ID] = ER;
            if (!Halflifes.Contains(ER.Chem)) { Halflifes[ER.Chem] = (int)255; }
            if (ER.emitter )
                OutSet[ER.Chem] = 1;
            else
                InSet[ER.Chem] = 1;
            if (!BlackBoard.ContainsKey(ER.Locus)) BlackBoard[ER.Locus] = (int)0;
 
        }

        public void AddReaction(String ID,Reaction R)
        {
            if (ID.Length == 0)
            {
                //ID = "REACT" + (genID++).ToString();
                ID = String.Format("R{4}:{0} + {1} = {2} + {3} ", R.reactant[0], R.reactant[1], R.reactant[2], R.reactant[3], (genID++));
            }
            R.ID = ID;
            Reactions[ID] = R;
            if (!Halflifes.Contains(R.reactant[0].ToString())) { Halflifes[R.reactant[0].ToString()] = (int)255; }
            if (!Halflifes.Contains(R.reactant[1].ToString())) { Halflifes[R.reactant[1].ToString()] = (int)255; }
            if (!Halflifes.Contains(R.reactant[2].ToString())) { Halflifes[R.reactant[2].ToString()] = (int)255; }
            if (!Halflifes.Contains(R.reactant[3].ToString())) { Halflifes[R.reactant[3].ToString()] = (int)255; }
            InSet[R.reactant[0].ToString()] = 1;
            InSet[R.reactant[1].ToString()] = 1;
            OutSet[R.reactant[2].ToString()] = 1;
            OutSet[R.reactant[3].ToString()] = 1;
        }
        public void applyHalflife(string chem, int v)
        {
            if (Halflifes.ContainsKey(chem))
            {
                Halflifes[chem] = (int)v;
            }
        }

        public double getChem(String ID)
        {
            double v1 = 0;
            if (ID == "null") return 0;
            lock (Chemicals)
            {
                try
                {
                    if (!Chemicals.ContainsKey(ID)) Chemicals[ID] = v1;
                    v1 = (double)Chemicals[ID];
                }
                catch (Exception e)
                {
                    Chemicals[ID] = (double)0;
                }
            }
            return v1;
        }
        public void subChemical(String ID, double v)
        {
            subChemical(ID, v, "atomic");
        }
        public void subChemical(String ID, double v,String note)
        {
            if (ID.Length == 0) return;
            if (ID == "null") return;

            double v1 = 0;
            lock (Chemicals)
            {
                try
                {
                    v1 = (double)Chemicals[ID];
                }
                catch (Exception e)
                {
                }
                v1 = v1 - v;
                if (v1 < 0) v1 = 0;
                if (v1 > 255) v1 = 255;
                Chemicals[ID] = v1;
                if (myWatchBox != null)
                {
                    try
                    {
                        if (myWatchBox.Tag.ToString().Contains(ID))
                        {
                            myWatchBox.Items.Add(biochemticks + ": " + ID + " -" + v + "(" + note + ")");
                        }
                    }
                    catch (Exception e)
                    {
                    }
                }
                if ((watchMt != null) && (prologEngine != null))
                {
                    try
                    {
                    string report = "biolog(" + biochemticks + "," + ID + ",sub," + v + "," + note + ").\n";
                    report = report.Replace(" ", "_");
                    report = report.Replace(",,", ",null,");
                    prologEngine.appendKB(report, watchMt);
                    }
                    catch (Exception e)
                    {
                    }
                }
            }
        }

        public void addChemical(String ID, double v)
        {
            addChemical(ID, v, "atomic");
        }
 
        public void addChemical(String ID, double v,String note)
        {
            if (ID.Length == 0) return;
            if (ID == "null") return;
            double v1 = 0;
            lock (Chemicals)
            {

                try
                {
                    if (!Chemicals.ContainsKey(ID)) Chemicals[ID] = v1;
                    v1 = (double)Chemicals[ID];
                }
                catch (Exception e)
                {
                }
                v1 = v1 + v;
                if (v1 < 0) v1 = 0;
                if (v1 > 255) v1 = 255;
                Chemicals[ID] = v1;
                if (myWatchBox != null)
                {
                    try
                    {
                        if (myWatchBox.Tag.ToString().Contains(ID))
                        {
                            myWatchBox.Items.Add(biochemticks + ": " + ID + " +" + v + "(" + note + ")");
                        }
                    }
                    catch (Exception E)
                    {
                    }
                }
            }
            if ((watchMt != null) && (prologEngine != null))
            {
                     try
                    {
               string report = "biolog(" + biochemticks + "," + ID + ",add," + v + "," + note + ").\n";
                report = report.Replace(" ", "_");
                report = report.Replace(",,", ",null,");
                prologEngine.appendKB(report, watchMt);
                    }
                     catch (Exception e)
                     {
                     }
            }

        }
        
       public  int calculateTickMask(int rate) {
            if (rate < 7) return 0;
            else return (1 << (( int)rate - 7)) - 1;
           }

     public int calculateMultiplier(int rate) {
	        return c1rates[rate];
            }

        public void ProcessReaction(String ID)
        {
            Reaction R = (Reaction)Reactions[ID];
            R.processReaction(this);
        }

        public void tickBiochemistry()
        {
            try
            {
                lock (Chemicals)
                {
                    // Emitters and Receptors don't need permutation since they
                    // don't potentially deadlock by starvation (you sense or add chems)
                    // it is possible for process A to starve process B if proc A is faster and uses
                    // up all the chemicals used by B. So you need to randomize the order a bit to be fair
                    // Don't think that is required for SoupIORules, except possibly for those that potentially have clear bits

                    fetchChemsFromCache();

                    // Process emitters (Inputs go in)
                    foreach (String RuleID in IORules.Keys)
                    {
                        SoupIORule ER = (SoupIORule)IORules[RuleID];
                        if (ER.emitter)
                        {
                            ER.processRule(this);
                        }

                    }

                    // Process receptors (Output come out)
                    if (prologEngine != null) prologEngine.clearKB("chemSimOutMt");
                    foreach (String RuleID in IORules.Keys)
                    {
                        SoupIORule ER = (SoupIORule)IORules[RuleID];
                        if (!ER.emitter)
                        {
                            ER.processRule(this);
                        }

                    }

                    // Process reactions
                    // enable a random permutation
                    ArrayList ReactList = new ArrayList();
                    foreach (String React in Reactions.Keys)
                    {
                        ReactList.Add(React);

                    }
                    int tnum = ReactList.Count;
                    for (int i = 0; i < 1000; i++)
                    {
                        Random r = new Random();

                        int p1 = r.Next(tnum);
                        int p2 = r.Next(tnum);
                        string t = (String)ReactList[p1];
                        ReactList[p1] = ReactList[p2];
                        ReactList[p2] = t;
                    }
                    foreach (String React in ReactList)
                    {
                        if (React == "drive lower - hunger")
                        {
                            int x = 0; // breakpoing chance
                        }
                        if (React == "drive raise - hunger")
                        {
                            int x = 0; // breakpoing chance
                        }
                        //ProcessReaction(React);
                        Reaction R = (Reaction)Reactions[React];
                        R.processReaction(this);
                    }
                    // Process half-lives
                    //if (!halflives) return; // TODO: correct?
                    foreach (string ID in Halflifes.Keys)
                    {
                        if (ID == "drive lower - hunger")
                        {
                            int x = 0; // breakpoing chance
                        }


                        // work out which rate we're dealing with
                        int rate = (int)((int)Halflifes[ID] / 8);

                        // if the tickmask doesn't want us to change things this tick, don't!
                        if ((biochemticks & calculateTickMask(rate)) != 0) continue;

                        // do the actual adjustment
                        Chemicals[ID] = ((double)getChem(ID) * calculateMultiplier(rate)) / 65536;

                    }
                    biochemticks++;
                    //Update the SortedBlackBoard
                    SortedBlackBoard = sortedBlackBoard();

                    if (m_RChem != null)
                    {
                        update_Rchem(false);
                    }

                    //Update the Cache
                    postChemsToCache();
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR:tickBioChemistry :{0}", e.Message);
            }
        }

        public void update_Rchem(bool full)
        {
            if (m_RChem == null) return;
               // Update the world
                m_RChem.BlackBoard = BlackBoard;
                m_RChem.Chemicals = Chemicals;
                m_RChem.IORules = IORules;
                m_RChem.Settings = Settings;
                m_RChem.subsumptionBlackBoard =subsumptionBlackBoard;
                m_RChem.Halflifes = Halflifes;
                m_RChem.send_chemistry(full);
        }

        public void postChemsToCache()
        {
            if (m_RChem == null) return;
            foreach (string key in BlackBoard.Keys)
            {
                int v = (int)BlackBoard[key];
                m_RChem.m_cBus.setHash(key, v.ToString());
            }
            foreach (string key in Chemicals.Keys)
            {
                int v = (int)Chemicals[key];
                m_RChem.m_cBus.setHash(key, v.ToString());

            }
            foreach (string key in subsumptionBlackBoard.Keys)
            {
                string v = (string)subsumptionBlackBoard[key];
                m_RChem.m_cBus.setHash(key, v);
            }

        }
        public void fetchChemsFromCache()
        {
            if (m_RChem == null) return;
            // Process emitters (Inputs go in)
            foreach (String RuleID in IORules.Keys)
            {
                SoupIORule ER = (SoupIORule)IORules[RuleID];
                if (ER.emitter)
                {
                    try
                    {
                        //ER.processRule(this);
                        string key = ER.Locus;
                        string v = m_RChem.m_cBus.getHash(key);

                        BlackBoard[key] = (int)int.Parse(v);
                    }
                    catch
                    {
                    }
                }

            }
        }

        public void setBlackBoard(String ID, int v)
        {
            if (v > 255) v = 255;
            if (v < 0) v = 0;
            BlackBoard[ID] = v;
        }
        public int getBlackBoard(string ID)
        {
            try
            {
                return (int)BlackBoard[ID];
            }
            catch (Exception E)
            {
                return 0;
            }
        }

        public void genSubsumptionBlackboard()
        {
            // sorts through the Blackboard looking for terms of the form "Line:...:value"
            // and makes sure the highest scoring "line:...:" will have the value 
            // so the best response for "TTS:Pain:..." will be present at "TTS:Pain"
            // similar to the old pandamonium model
            Hashtable priority = new Hashtable();
            foreach (string key in BlackBoard.Keys)
            {
                int v = (int)BlackBoard[key];
                string[] keyfield = key.Split(':');
                if (keyfield.Length > 0)
                {
                    string val = keyfield[keyfield.Length-1];
                    string line = key.Replace(":" + val, "");
                    if (!priority.Contains(line))
                    {
                        priority[line] = v;
                        subsumptionBlackBoard[line] = val;
                    }
                    else
                    {
                        if ((int)priority[line] < v)
                        {
                            priority[line] = v;
                            subsumptionBlackBoard[line] = val;

                        }
                    }
                }
            }
        }

        public void genGraph(string filename)
        {
            // create a writer and open the file
            gtw = new StreamWriter(filename);
            // C:\Progra~1\Graphviz2.26.3\bin\dot -Tpdf c:\_daxlab\dolllab\biograph.dot -o biograph.pdf
 
            gtw.WriteLine("digraph metab{ graph [ fontsize = 5 ] ");

            foreach (string ID in BlackBoard.Keys)
            {
                string shape = "parallelogram";
                string color = "lightgrey";
                if (ID.Length > 0)
                {
                    int v = (int)BlackBoard[ID];
                   // WriteLineGTW(String.Format("\"{0}\" [shape={1}, style=filled,color={2},label=\"{0} hf({3})\"];", ID.Replace(" ", "_"), shape, color, v));
                    WriteLineGTW(String.Format("\"{0}\" [shape=box, style=filled,color={2},label=\"{0} hf({3})\"];", ID.Replace(" ", "_"), shape, color, v));
                }
            }

            foreach (string ID in Halflifes.Keys)
            {
                string shape = "box";
                string color = "lightgrey";

 
                if (!InSet.ContainsKey(ID))
                {
                    shape = "hexagon"; color = "orange";

                }
                if (!OutSet.ContainsKey(ID))
                {
                    shape = "hexagon"; color = "magenta";

                }
                if ((OutSet.ContainsKey(ID)) &&(OutSet.ContainsKey(ID)))
                {
                    shape = "box"; color = "lightgrey";

                }

               if (DriveSet.ContainsKey(ID))
                {
                    shape="octagon"; color="skyblue";
                    
                }
                if (EmotiveSet.ContainsKey(ID))
                {
                    shape = "octagon"; color = "green";
                }
                if (SemSet.ContainsKey(ID))
                {
                    shape = "triangle"; color = "pink";
                }
                
                if (ID.Contains("++") ||ID.Contains("--"))
                {
                    shape = "octagon"; color = "yellow";
                }
                if (ID.Contains("_incr") || ID.Contains("_dec"))
                {
                    shape = "octagon"; color = "yellow";
                }
                if (ID.Length > 0)
                {
                    int v = (int)Halflifes[ID];
                   // WriteLineGTW(String.Format("\"{0}\" [shape={1}, style=filled,color={2},label=\"{0} hf({3})\"];", ID.Replace(" ", "_"), shape, color, v));
                    WriteLineGTW(String.Format("\"{0}\" [shape=box, style=filled,color={2},label=\"{0} hf({3})\"];", ID.Replace(" ", "_"), shape, color, v));
                }
            }

            foreach (String React in Reactions.Keys)
            {
                WriteLineGTW(String.Format("\"{0}\" [shape=ellipse, label=\"{0} r({1})\"];", React.Replace(" ", "_"), ((Reaction)Reactions[React]).rate));

            }
            foreach (String React in Reactions.Keys)
            {
                //tw.WriteLine("\"{0}\" [shape=ellipse];", React.Replace(" ", "_"));

                Reaction R = (Reaction)Reactions[React];
                if (R.reactant[0].Length > 0) { WriteLineGTW(String.Format("\"{0}\" -> \"{1}\" [label=\"{2}\", weight={2} ];", R.reactant[0].Replace(" ", "_"), React.Replace(" ", "_"), R.quantity[0])); }
                if (R.reactant[1].Length > 0) {WriteLineGTW(String.Format("\"{0}\" -> \"{1}\" [label=\"{2}\", weight={2} ];", R.reactant[1].Replace(" ", "_"), React.Replace(" ", "_"), R.quantity[1]));}
                if (R.reactant[2].Length > 0) {WriteLineGTW(String.Format("\"{0}\" -> \"{1}\" [label=\"{2}\", weight={2} ];", React.Replace(" ", "_"), R.reactant[2].Replace(" ", "_"), R.quantity[2]));}
                if (R.reactant[3].Length > 0) { WriteLineGTW(String.Format("\"{0}\" -> \"{1}\" [label=\"{2}\", weight={2} ];", React.Replace(" ", "_"), R.reactant[3].Replace(" ", "_"), R.quantity[3])); }
            }

            foreach (String ID in IORules.Keys)
            {
                SoupIORule R = (SoupIORule) IORules[ID];
                if (R.emitter)
                {
                    WriteLineGTW(String.Format("\"{0}\" [shape=box, label=\"{0} rule({1} [{3}] {2})\"];", R.ID.Replace(" ", "_"), R.Locus,R.Chem,R.opts));
                    WriteLineGTW(String.Format("\"{1}\" -> \"{0}\" [label=\"{2}\", weight={2} ];", R.ID.Replace(" ", "_"), R.Locus.Replace(" ", "_"), R.threshold));
                    WriteLineGTW(String.Format("\"{0}\" -> \"{1}\" [label=\"{2}\", weight={2} ];", R.ID.Replace(" ", "_"), R.Chem.Replace(" ", "_"), R.gain));
                }
                else
                {
                    WriteLineGTW(String.Format("\"{0}\" [shape=box, label=\"{0} rule({2} [{3}] {1})\"];", R.ID.Replace(" ", "_"), R.Locus, R.Chem, R.opts));
                    WriteLineGTW(String.Format("\"{0}\" -> \"{1}\" [label=\"{2}\", weight={2} ];", R.ID.Replace(" ", "_"), R.Locus.Replace(" ", "_"), R.gain));
                    WriteLineGTW(String.Format("\"{1}\" -> \"{0}\" [label=\"{2}\", weight={2} ];", R.ID.Replace(" ", "_"), R.Chem.Replace(" ", "_"), R.threshold));
                }

                }

            gtw.WriteLine("}");
            gtw.Close();

        }
        public void WriteLineGTW(string line)
        {
            string pattern = @"[^A-Za-z0-9\t\-\+\,\(\)\[\]\=\>"+"\"" +"]+";
            string replacer = " ";
            Regex rgx = new Regex(pattern);
            string result = rgx.Replace(line,replacer);
            byte[] postBytes = encoding.GetBytes(result);
            string altstr = encoding.GetString(postBytes);
            gtw.WriteLine(altstr);

        }
        public void defineAll()
        {
  
            loadDir(@"C:\_daxlab\Norns\dollbio");
            loadDir(@"C:\_daxlab\Norns\dollbio\conf");
        }
        public void createCreature()
        {
            defineAll();
            birthChems();
           // defineHalflives();
       }
        public void normalDrive(string drive,int reward, int punishment)
        {
            AddReaction(String.Format("drive cancel {0}", drive), new Reaction(1, String.Format("{0}_incr", drive), 1, String.Format("{0}_dec", drive), 1, "", punishment, "", 8));
            AddReaction(String.Format("drive raise {0}", drive), new Reaction(1, String.Format("{0}_incr", drive), 1, "", 3, drive, punishment, "punishment", 32));
            AddReaction(String.Format("drive lower {0}", drive), new Reaction(1, String.Format("{0}_dec", drive), 3, drive, reward, "reward", 1, "", 32));
            AddReaction(String.Format("push drive {0}", drive), new Reaction(1, String.Format("{0}", drive), 1, "", 1, String.Format("backup{0}", drive), 1, "", 63));
            AddReaction(String.Format("pop drive {0}", drive), new Reaction(1, String.Format("backup{0}", drive), 1, "", 1, String.Format("{0}", drive), 1, "", 64));
            DriveSet[drive] = 1;
            Halflifes[drive] = (int)128;
            Halflifes[String.Format("{0}_dec", drive)] = (int)64;
            Halflifes[String.Format("{0}_incr", drive)] = (int)64;
            Halflifes[String.Format("backup{0}", drive)] = (int)70;
        }

        public void genlFeel(String spec, String genls)
        {
            AddReaction(String.Format("genl({0},{1})", spec, genls), new Reaction(1, String.Format("{0}", spec), 1, "", 1, String.Format("{0}", genls), 1, "", 8));
            Halflifes[spec] = (int)255;
            Halflifes[genls] = (int)255;
            SemSet[spec] = 1;
            SemSet[genls] = 1;
        }

        public void defineMetabolism()
        {
            //file:///C:/_daxlab/Norns/formulae.htm
 
        }
        public void defineC2Metabo()
        {

  

        }
        public void defineThrustBio()
        {

 
			

        }
		
	    public void defineFaceBio()
        {
 
        }
        public void defineFeelings()
        {
	


        }

        public void animFeel(string anim, string spec, string genls)
        {
            
            anim = anim.ToLower();
            spec = spec.ToLower();
            genls = genls.ToLower();
            string R =String.Format("genl({0},{1})", spec, genls);
            if(!Reactions.ContainsKey (R))
            {
            AddReaction(R, new Reaction(1, String.Format("{0}", spec), 1, "", 1, String.Format("{0}", genls), 1, "", 8));
            Halflifes[spec] = (int)255;
            Halflifes[genls] = (int)255;
            SemSet[spec] = 1;
            SemSet[genls] = 1;
            }
            if (spec.Equals(anim)) return;
            string R2 = String.Format("actionFor({0},{1})",spec, anim);
            if (!Reactions.ContainsKey(R2))
            {
            AddReaction(R2,  new Reaction(1, String.Format("{0}", spec), 1, "", 1, String.Format("{0}", anim), 1, "", 8));
            Halflifes[spec] = (int)255;
            Halflifes[genls] = (int)255;
            SemSet[spec] = 1;
            SemSet[genls] = 1;
            EmotiveSet[spec] = 1;
            EmotiveSet[anim] = 1;
            }
        }
        public void defineAnimBio()
        {
 

        }
        public void bodyInterface()
        {
            // Breathing -> air
            // Injury
            // ingestion of (protein, fat,alcohol, charbohydrates,fullness)
            // Contact with Antigens
            // Contact with Toxins
            // excreation
            // pheromone detection

            // Can do spreading activation using object/action hiearchies
            // a food class can cause a cascade into other types (vodka vs. beer / hamburger vs. apple pie)
            // if you have rapelin and lusting what about violatin, wrongin, lovlin?
            //  (object rec providing direct injection to "skip a beat")


        }
	
		public void birthChems()
		{
		addChemical("muscleTissue",32,"birth");
        addChemical("adiposeTissue", 40, "birth");
        addChemical("glucose", 255, "birth");
        addChemical("ATP", 255, "birth");
        addChemical("Energy", 255, "birth");
        addChemical("triglyceride", 16, "birth");
        addChemical("hunger for fat", 33, "birth");
        addChemical("hunger for protein", 33, "birth");
        addChemical("antibody1", 52, "birth");
        addChemical("antibody2", 46, "birth");
        addChemical("antibody3", 50, "birth");
        addChemical("antibody4", 48, "birth");
        addChemical("antibody5", 56, "birth");
        addChemical("antibody6", 23, "birth");
        addChemical("fattyAcid", 16, "birth");
        addChemical("antibody7", 46, "birth");
        addChemical("pyruvate", 64, "birth");
        addChemical("oxygen", 191, "birth");
        addChemical("glycogen", 34, "birth");
        addChemical("water", 255, "birth");
        addChemical("air", 64, "birth");
        addChemical("breath", 64, "birth");
        addChemical("hunger for carbohydrate", 13, "birth");
        addChemical("antibody0", 96, "birth");
        addChemical("boredom", 90, "birth");
        addChemical("life", 255, "birth");
        addChemical("hunger", 16, "birth");

		}
        public void defineHalflives()
        {
	
            return;

        }

        public void loadDir(string dirname)
        {
            DirectoryInfo di = new DirectoryInfo(dirname);
            FileInfo[] rgFiles = di.GetFiles("*.txt");
            //CARA,CARLEY, CAREY,CARE,SELMA,SERE
            foreach (FileInfo fi in rgFiles)
            {
                interpretSpec(fi.FullName);
            }

        }

        public void writeStreamHT(StreamWriter objWriter,string format, Hashtable HT)
        {
            foreach (string k in HT.Keys)
            {
                objWriter.WriteLine(String.Format(format,HT[k].ToString(),k));
            }
        }
        public void saveTextFile(string filename)
        {
            StreamWriter objWriter = new StreamWriter(filename);

            writeStreamHT(objWriter, "setting,{1},{0}", Settings);
            writeStreamHT(objWriter, "addchemical,{1},{0}", Chemicals);
            writeStreamHT(objWriter, "halflifes,{1},{0}", Halflifes);
            writeStreamHT(objWriter, "driveset,{1},{0}", DriveSet);
            writeStreamHT(objWriter,"{0}", Reactions);
            writeStreamHT(objWriter, "emotiveset,{1},{0}", EmotiveSet);
            writeStreamHT(objWriter, "semset,{1},{0}", SemSet);
            writeStreamHT(objWriter, "setblackboard,{1},{0}", BlackBoard);
            //writeStreamHT(objWriter, "{0}", subsumptionBlackBoard);
            writeStreamHT(objWriter, "{0}", IORules);
            objWriter.Close();
        }
        public void  saveBinary(string filename)
        {
        }

        string[] SplitCSV(string inputText)
        {
            return Regex.Split(inputText, ",(?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))");
        }
        public void interpertGLine(string altstr)
        {
            string prologCode = null;
            if (altstr.ToLower().Contains("addiorule") && altstr.Contains("(") && altstr.Contains(")"))
            {
                // Guessing it has prolog in format of ",Mt:pred(x,u),"
                // so keep and replace with ",PROLOGCODE," and replace on aiorule
                Match match = Regex.Match(altstr, @"\,([^\,]+?\:.*?\(.*?\))\,",
                    RegexOptions.IgnoreCase);
                if (match.Success)
                {
                    // Finally, we get the Group value and display it.
                    prologCode = match.Groups[1].Value;
                    altstr = altstr.Replace(prologCode, "PROLOGCODE");
                }

            }
            lock (Chemicals)
            {

                altstr = altstr.Replace(";", ","); // Escape ";" into "," before processing

                string[] cmdArgs = SplitCSV(altstr);
                string cmd = cmdArgs[0].ToLower();

                if (cmd.Equals("animfeel"))
                {
                    animFeel(cmdArgs[1], cmdArgs[2], cmdArgs[3]);
                }
                if (cmd.Equals("genlfeel"))
                {
                    genlFeel(cmdArgs[1], cmdArgs[2]);
                }
                if (cmd.Equals("normaldrive"))
                {
                    normalDrive(cmdArgs[1], Int32.Parse(cmdArgs[2]), Int32.Parse(cmdArgs[3]));
                }
                if (cmd.Equals("addchemical"))
                {
                    addChemical(cmdArgs[1], Int32.Parse(cmdArgs[2]));
                }
                if (cmd.Equals("subchemical"))
                {
                    subChemical(cmdArgs[1], Int32.Parse(cmdArgs[2]));
                }
                if (cmd.Equals("driveset"))
                {
                    DriveSet[cmdArgs[1]] = Int32.Parse(cmdArgs[2]);
                    addChemical(cmdArgs[1], 0);
                }
                if (cmd.Equals("halflifes"))
                {
                    Halflifes[cmdArgs[1]] = Int32.Parse(cmdArgs[2]);
                    addChemical(cmdArgs[1], 0);
                }
                if (cmd.Equals("semset"))
                {
                    SemSet[cmdArgs[1]] = Int32.Parse(cmdArgs[2]);
                }
                if (cmd.Equals("emotiveset"))
                {
                    EmotiveSet[cmdArgs[1]] = Int32.Parse(cmdArgs[2]);
                }
                if (cmd.Equals("monitor"))
                {
                    //KHC:FIX
                    //if (outForm != null)
                    //{
                    //    outForm.monitorChem(cmdArgs[1]);
                    //}
                }

                if (cmd.Equals("addreaction"))
                {
                    AddReaction(cmdArgs[1], new Reaction(Int32.Parse(cmdArgs[2]), cmdArgs[3], Int32.Parse(cmdArgs[4]), cmdArgs[5], Int32.Parse(cmdArgs[6]), cmdArgs[7], Int32.Parse(cmdArgs[8]), cmdArgs[9], Int32.Parse(cmdArgs[10])));
                }
                if (cmd.Equals("addiorule"))
                {
                    if (cmdArgs[1].Length == 0)
                    {
                        cmdArgs[1] = String.Format("GR{0}", (genID++));
                    }
                    if (cmdArgs[1].Contains("pro"))
                    {
                        Console.WriteLine(genID);
                    }
                    if ((prologCode != null)&&(cmdArgs [2]=="PROLOGCODE"))
                    {
                        cmdArgs[2] = prologCode;
                    }
                    SoupIORule R = new SoupIORule(cmdArgs[1], cmdArgs[2], cmdArgs[3], cmdArgs[4], Double.Parse(cmdArgs[5]), Double.Parse(cmdArgs[6]), Double.Parse(cmdArgs[7]), Double.Parse(cmdArgs[8]));
                    // R.emitter = true;
                    AddLink(R.ID, R);
                    Console.WriteLine("ADDIORULE:{0}", R.ToString());
                }
                if (cmd.Equals("addiorulecmd"))
                {
                    if (IORules.ContainsKey(cmdArgs[1]))
                    {
                        SoupIORule R = (SoupIORule)IORules[cmdArgs[1]];
                        R.cmd = cmdArgs[2];
                        IORules[cmdArgs[1]] = R;
                    }
                }
                if (cmd.Equals("setinterface"))
                {
                    setBlackBoard(cmdArgs[1], Int32.Parse(cmdArgs[2]));
                }
                if (cmd.Equals("setblackboard"))
                {
                    setBlackBoard(cmdArgs[1], Int32.Parse(cmdArgs[2]));
                }
                if (cmd.Equals("setting"))
                {
                    Settings[cmdArgs[1]] = cmdArgs[2];
                }


                if (cmd.Equals("loaddir"))
                {
                    loadDir(cmdArgs[1]);
                }
                if (cmd.Equals("loadfile"))
                {
                    interpretSpec(cmdArgs[1]);
                }
                if (cmd.Equals("gengraph"))
                {
                    genGraph(cmdArgs[1]);
                }
                if (cmd.Equals("loadbreath"))
                {
                    // if (myBreather != null)
                    // {
                    //myBreather.loadDir(cmdArgs[1]);
                    // }
                }
                if (cmd.Equals("start"))
                {
                    startEngine();
                }
                if (cmd.Equals("stop"))
                {
                    stopEngine();
                }
            }
         }

        public void interepretCmdList(string cmdLines)
        {
            cmdLines=cmdLines.Replace('|','\n');
            string[] cmdList = cmdLines.Split('\n');
            foreach (string cmd in cmdList)
            {
                //do processing
                try
                {
                    interpertGLine(cmd);

                }
                catch (Exception e)
                {
                    // KHC:TOFIX
                    // just skip
                    //if (outForm != null)
                    //{
                    //    outForm.myOutBox.Items.Add(e.Message+":["+cmd+"]");
                    //}
                }
                   
            }
        }

        public void interpretSpec(string filename)
        {
            StreamReader objReader = new StreamReader(filename);
            UTF8Encoding encoding = new UTF8Encoding();
            string strLineText;
            int linenum = 0;
            bool skipit = false;
            while ((strLineText = objReader.ReadLine()) != null)
            {
                linenum++;

                byte[] postBytes = encoding.GetBytes(strLineText);
                string altstr = encoding.GetString(postBytes);
                if (altstr.Contains(@"//")) continue;
                if (altstr.Contains ("skipon")) skipit=true;
                if (altstr.Contains ("skipoff")) skipit=false;
                if (altstr.Contains("quit")) break;
                if (skipit == true) continue;

                string[] cmdArgs = SplitCSV(altstr);
                //do processing
                try
                {
                    interpertGLine(altstr);

                }
                catch (Exception e)
                {
                    // KHC: TOFIX
                    // just skip
                    //if (outForm != null)
                    //{
                    //    outForm.myOutBox.Items.Add(filename + " ln " + linenum.ToString() + ":" + e.Message);
                    //}
                }

            }
            objReader.Close();

        }

        public List <KeyValuePair<String, int>> sortedBlackBoard()
        {
            // Declare List
            List <KeyValuePair<String, int>> myList = new List<KeyValuePair<String, int>>();
            foreach (String key in BlackBoard.Keys)
            {
                myList.Add( new KeyValuePair<string,int>(key,(int)BlackBoard[key]) );
            }
            // Sort the list passing in a delegate comparator
            myList.Sort(delegate(KeyValuePair<String, int> x, KeyValuePair<String, int> y) { return y.Value.CompareTo(x.Value); });
            return myList;
        }

        public void tick_chemistry(bool full)
        {
            //KHC:TOFIX
            tickBiochemistry();
        }

        public void webWriter(StreamWriter writer, string action, string query, string mt, string serverRoot)
        {
            serverRoot = "/";
            //webWriter0(writer, action, query, mt, serverRoot, true);
            if (action == "autorefresh")
            {
                writer.WriteLine("<META HTTP-EQUIV=\"REFRESH\" content=\"10\">");
            }

            writer.WriteLine("<html>");
            writer.WriteLine("<head>");
            writer.WriteLine("<script type=\"text/javascript\"");
            writer.WriteLine("  src=\"{0}jsbin/dygraph-combined.js\"></script>", serverRoot);
            writer.WriteLine("</head>");
            writer.WriteLine("<body>");
            writer.WriteLine("<a href='{0}siprolog/?q=list'>List Mts</a><br/>", serverRoot);
            writer.WriteLine("<a href='{0}siprolog/?q=listing'>List All Rules</a><br/>", serverRoot);
            writer.WriteLine("<a href='{0}plot/?mt={1}&a=autorefresh&q=plot(X,Y)'>Scope mt {1}</a><br/>", serverRoot, mt);
            writer.WriteLine("<h3>Mt:{0}</h3>", mt);
            writer.WriteLine("<h3>Query:{0}</h3>", query);
            writer.WriteLine("<div id=\"graphdiv2\");");
            writer.WriteLine("  style=\"width:500px; height:300px;\"></div>");
            writer.WriteLine("<script type=\"text/javascript\">");
            writer.WriteLine("  g2 = new Dygraph(");
            writer.WriteLine("    document.getElementById(\"graphdiv2\"),");
            if ((query!=null) && (mt!=null))
            {
            string csvText = kbToCsv(query, mt);
            writer.WriteLine(csvText);
            }
            else
            {
            writer.WriteLine("    \"Date,Temperature\\n\" +");
            writer.WriteLine("        \"2008-05-07,75\\n\" +");
            writer.WriteLine("        \"2008-05-08,70\\n\" +");
            writer.WriteLine("        \"2008-05-09,80\\n\" , ");
            }
            writer.WriteLine("    ,{}          // options");
            writer.WriteLine("  );");
            writer.WriteLine("</script>");
            writer.WriteLine("</body>");
            writer.WriteLine("</html>");

        }

        public string kbToCsv(string query, string mtName)
        {
            // A query like coppeliaAnger(AGENT,TARGET,VALUE) might return 
            // AGENT,TARGET,VALUE
            // self, other,0.8
            string code = "";
            string header = "";
            try
            {
                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                string[] part = query.Split('(');
                string head = part[0].Trim();
                prologEngine.askQuery(query, mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string frag = "\"";
                    header = "\"";
                    int cnt = 0;
                    foreach (string key in bindings.Keys)
                    {
                        cnt++;
                        header += key.ToLower();
                        frag +=  bindings[key].Trim();
                        if (cnt < bindings.Count)
                        {
                            header += ",";
                            frag += ",";
                        }
                    }
                    frag += "\\n\" + \n";
                    header += "\\n\" + \n";
                    code += frag;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Error kbToBTXML '{0}':{1}", mtName, e.Message );
            }
            return header + code+ "\"\"";
        }
    }

    public class SoupIORule
    {
        // responsible for contact with the outside world
        // A rule can either look at Soup.BlackBoard[Locus] and emit Soup.Chemicals[Chem]
        //   or can examine Soup.chemicals[Chem] and change Soup.BlackBoard[Locus]

        // Output example 
        // Locus=Creature:Sensorimotor:Gait1, chem=Pain, thresh=33, nom=0, gain=239, features=Digital
        // which would set Soup.BlackBoard["Creature:Sensorimotor:Gait1"]=255 when Soup.Chemicals["Pain"] > 33 (so it might limp)
        // This could be utilized in analogue mode to provide weights or modulation to certain output options (like wav files or responses)
        // An external user process could also add relevant IO rules , and eventually a fuzzy rule system may be in order
 
        // Input example:
        //Locus=Creature:Reproductive:fertile, chem=ArousalPotential, thresh=128, samp=3, gain=14, features=Digital Emitter
        // Which checks at rate=3 and does Soup.AddChemical("ArousalPotential",14) if Soup.BlackBoard["Creature:Reproductive:fertile"]>128
        //
        // Note: we can extend the Locus check to include other logical situational checks like using an inference engine
 
        public String Locus;
        public String Chem;
        public bool digital=false;
        public double threshold;
        public double gain;
        public double rate;

        public bool emitter = false;
        public bool clear = false;
        public bool invert = false;
        public bool diff = false;
        public double nominal;
        public String cmd;
        public String ID;
        public String opts;
        public String BBKey;
        public String BBValue;
        public static int genID = 0;

        public SoupIORule(String _ID, String _Locus, String _Chem, string Options, double _Threshold, double _gain,double _nom, double _rate)
        {
            ID = _ID;
            Locus = _Locus;
            Chem = _Chem;
            nominal = _nom;

            threshold = _Threshold;
            gain = _gain;
            rate = _rate;
            setBits(Options);
            opts = Options;
        }

        public override String ToString()
        {
            return String.Format("addIORule,{0},{1},{2},{3},{4},{5},{6},{7}", ID, Locus, Chem, bitsToString(), threshold, gain,nominal, rate);
        }

        public void setBits(String s)
        {
            s = s.ToLower();
            digital = s.Contains("digital");
            emitter = s.Contains("emitter");
            invert = s.Contains("invert");
            clear = s.Contains("clear");
            diff = s.Contains("diff");
        
        }

        public String bitsToString()
        {
            String bts = "";
            if (clear) bts += "clear ";
            if (invert) bts += "invert ";
            if (diff) bts += "diff ";
            if (digital) bts += "digital "; else bts += "analog ";
            if (emitter) bts += "emitter "; else bts += "receptor ";
           return bts.Trim();
        }

        public void fillCheckedListBox(CheckedListBox box)
        {
            box.Items.Clear();
            box.Items.Add("clear");
            box.Items.Add("invert");
            box.Items.Add("diff");
            box.Items.Add("digital");
            box.Items.Add("analog");
            box.Items.Add("emitter");
            box.Items.Add("receptor");
            box.SetItemChecked(box.Items.IndexOf("clear"), clear);
             box.SetItemChecked(box.Items.IndexOf("invert"), invert);
             box.SetItemChecked(box.Items.IndexOf("diff"), diff);
             box.SetItemChecked(box.Items.IndexOf("digital"), digital);
             box.SetItemChecked(box.Items.IndexOf("analog"), !digital);
             box.SetItemChecked(box.Items.IndexOf("emitter"), emitter);
             box.SetItemChecked(box.Items.IndexOf("receptor"), !emitter);
       
        }
        public void readCheckedListBox(CheckedListBox box)
        {

            clear=box.GetItemChecked(box.Items.IndexOf("clear") );
            invert=box.GetItemChecked(box.Items.IndexOf("invert"));
            diff=box.GetItemChecked(box.Items.IndexOf("diff"));
            digital = box.GetItemChecked(box.Items.IndexOf("digital"));
           // box.SetItemChecked(box.Items.IndexOf("analog"), !digital);
            emitter = box.GetItemChecked(box.Items.IndexOf("emitter"));
           // box.SetItemChecked(box.Items.IndexOf("receptor"), !emitter);

        }

        
        public void processRule(Qchem soup)
        {
            if (emitter)
            {
                // From the BlackBoard to the soup
                if ((soup.biochemticks % (int)rate) != 0) return;

                bool prologPass=true;
                if (soup.prologEngine != null)
                {
                    // Prolog should be in the format of <mt>:<query>

                    if (Locus.Contains("("))
                    {
                        string[] parms = Locus.Split(':');
                        string mt = parms[0];
                        string query = parms[1];
                        prologPass = soup.prologEngine.isTrueIn(query, mt);
                        if (prologPass)
                        {
                            soup.BlackBoard[Locus] = 255;
                        }
                        else
                        {
                            soup.BlackBoard[Locus] = 0;
                        }
                    }
                }
                if (!soup.BlackBoard.ContainsKey(Locus))  return;
                int f = (int)soup.BlackBoard[Locus];
                if (clear) soup.BlackBoard[Locus] = (int)0;
                if (invert) f = 255 - f;
                if (Chem.Length > 0)
                {
                    if (digital)
                    {
                        if (f < threshold) return;
                        soup.addChemical(Chem, gain,ID);
                     }
                    else
                    {
                        int r = (int)((((double)f - threshold) * gain) / 255);

                        // clip the result of the calculation to unsigned char, and reassign it
                        if (r < 0) r = 0; else if (r > 255) r = 255;
                        f = r;

                        soup.addChemical(Chem, f,ID);
                    }
                }
                if (cmd!=null)
                {
                    // We have a special line to interpert
                    soup.interepretCmdList(cmd);
                }

            }
            else
            {
                // From the soup to the BlackBoard
                //if (!soup.BlackBoard.ContainsKey(Locus)) return;
                int f = (int)soup.getChem(Chem);
                int r;
                if (digital)
                    r = (double)f > threshold ? (int) gain : (int)0;
                else
                    
                    r = (int) ( (((double)f - threshold) * gain) / 255);
                //basically scaled dist = 1-abs((f-threshold)/255) so you get a triangle activation
                // with the peak @ threshold  
                if (diff)
                    r =(int)( 255*gain * (1-Math.Abs((double)f - threshold)/255));

                if (invert) r =(int)( nominal - r);
                else r +=(int) nominal;
                // clip the result of the calculation to unsigned char, and reassign it
                if (r < 0) r = 0;
                if (r > 255) r = 255;
                f = r;
                // send it out
                soup.BlackBoard[Locus] = (int)f;
                if (soup.prologEngine != null)
                {
                    // Prolog should be in the format of <mt>:<query>

                    if (Locus.Contains("("))
                    {
                        string[] parms = Locus.Split(':');
                        string mt = parms[0];
                        string query = parms[1] + ".\n";
                        if (r > 0)
                        {
                            soup.prologEngine.appendKB(query, mt);
                        }
                        if (r == 0)
                        {
                            //soup.prologEngine.retractKB(query, mt);
                        }
 
                    }
                }              
            }
        }
 
        static string[] SplitCSV(string inputText)
        {
            return Regex.Split(inputText, ",(?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))");
        }

        public static SoupIORuleProxy Parse(string altstr)
        {
            altstr = altstr.Replace(";", ","); // Escape ";" into "," before processing
            string[] cmdArgs = SplitCSV(altstr);
            string cmd = cmdArgs[0].ToLower();
            if (cmd.Equals("addiorule"))
            {
                if (cmdArgs[1].Length == 0)
                {
                    cmdArgs[1] = String.Format("GR{0}", (genID++));
                }
                SoupIORuleProxy R = new SoupIORuleProxy(cmdArgs[1], cmdArgs[2], cmdArgs[3], cmdArgs[4], Double.Parse(cmdArgs[5]), Double.Parse(cmdArgs[6]), Double.Parse(cmdArgs[7]), Double.Parse(cmdArgs[8]));
                // R.emitter = true;
                return R;

            }
            return null;
        }

    }

    public class Reaction
    {
        public String ID = null;
        public String [] reactant = new String[4];
        public double[] quantity = new double[4];
        public  double rate = 0;

        
        public Reaction(String RA,double QA,String RB, double QB, String RC,double QC,String RD,double QD,double Rate)
        {
            reactant[0] = RA;
            reactant[1] = RB;
            reactant[2] = RC;
            reactant[3] = RD;

            quantity[0] = QA;
            quantity[1] = QB;
            quantity[2] = QC;
            quantity[3] = QD;
            rate = Rate;

        }
        public Reaction(double QA,String RA,  double QB,String RB,   double QC,String RC, double QD,String RD,  double Rate)
        {
            reactant[0] = RA;
            reactant[1] = RB;
            reactant[2] = RC;
            reactant[3] = RD;

            quantity[0] = QA;
            quantity[1] = QB;
            quantity[2] = QC;
            quantity[3] = QD;
            rate = Rate;

        }
        public override  string ToString()
        {
            return String.Format ("addReaction,{0},{1},{2},{3},{4},{5},{6},{7},{8},{9}",ID, quantity[0], reactant[0], quantity[1], reactant[1], quantity[2], reactant[2], quantity[3], reactant[3], rate);
        }
        public void processReaction(Qchem soup)
        {
            double ratio = 0;
            double ratio2 = 0;

            int myRate = (int)rate / 8;

            // if the tickmask doesn't want us to change things this tick, don't!
            int ctm = soup.calculateTickMask(myRate);
            if ((soup.biochemticks & ctm) != 0) return;

            if ((reactant[0].Length != 0)&&(reactant[0]!="null"))
            {
                ratio = (double)soup.getChem(reactant[0]) / quantity[0];
            }
            if ((reactant[1].Length != 0)&&(reactant[1]!="null"))
            {
                ratio2 = (double)soup.getChem(reactant[1]) / quantity[1];
            }
            else
            {
                ratio2 = ratio; // It was "<NOTHING>" so don't bother
            }
            if (ratio2 < ratio) ratio = ratio2;
            if (ratio == 0) return;

            // C1 model ratio = ratio - (ratio * R.rate);
            // calculate the actual adjustment (can't go out of bounds)
            int cmut = soup.calculateMultiplier(myRate);
            ratio = ratio - ((ratio * cmut) / 65536);

            // of the possible amount of the total possible transfer, only transfer p
            // where p = 1-rv, and rv = an exponentially decreasing amount as defined by c1rates.
            // when R.rate=256 rv would equal 1 and thus none would be transfered

            //C2 model
            //double rate = 1.0 - Math.Pow(0.5, 1.0 / Math.Pow(2.2, (1.0 - R.rate) * 32.0));
            //ratio = ratio * rate;
            //change chems
            string reason = ToString();
            soup.subChemical(reactant[0], ratio * quantity[0], reason);
            soup.subChemical(reactant[1], ratio * quantity[1], reason);
            soup.addChemical(reactant[2], ratio * quantity[2], reason);
            soup.addChemical(reactant[3], ratio * quantity[3], reason);

        }
    }
}
