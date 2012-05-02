using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Threading;
using Enyim.Caching;
using Enyim.Caching.Configuration;
using Enyim.Caching.Memcached;
using System.Windows.Forms;
using System.Xml.Serialization;
using System.Xml;
using System.Xml.Schema;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text.RegularExpressions;

using System.IO;

namespace DcBus
{
        [Serializable]
        public class RChem
        {
            public HashtableSerailizable BlackBoard = new HashtableSerailizable(); // raw I/O blackboard
            public HashtableSerailizable subsumptionBlackBoard = new HashtableSerailizable(); // parsed so only the highest value for each is visible
            public HashtableSerailizable Settings = new HashtableSerailizable();
            public HashtableSerailizable Chemicals = new HashtableSerailizable();
            public HashtableSerailizable IORules = new HashtableSerailizable();
            public HashtableSerailizable Halflifes = new HashtableSerailizable();

            public CacheBus m_cBus = null;
            public int biochemticks = 0;

            public delegate void chemUpdateWatcher();
            public Hashtable chemWatchers = new Hashtable();
            public string _ipAddress = "127.0.0.1";
            [NonSerialized ]
            public System.Windows.Forms.Timer Clock = null;
            [NonSerialized]
            public ListBox myWatchBox = null;
            public RChem(string ipaddress)
            {

                m_cBus = new CacheBus(ipaddress);
                _ipAddress = m_cBus._ipAddress;
                tick_chemistry(true);
                startEngine();
            }

            public RChem(bool master)
            {
                m_cBus = new CacheBus();
                _ipAddress = m_cBus._ipAddress;
                if (master == true)
                {
                    send_chemistry(true);
                }
                else
                {
                    tick_chemistry(true);
                }
                startEngine();
 
            }

            public RChem(string ipaddress, bool master)
            {

                m_cBus = new CacheBus(ipaddress);
                _ipAddress = m_cBus._ipAddress;
                if (master == true)
                {
                    send_chemistry(true);
                }
                else
                {
                    tick_chemistry(true);
                }
                startEngine();
            }

            public RChem()
            {
                m_cBus = new CacheBus();
                _ipAddress = m_cBus._ipAddress;
                tick_chemistry(true);
                startEngine();
            }

            // Our simulator real-time heartbeat
            public void registerWatcher(string ID, chemUpdateWatcher w)
            {
                chemWatchers[ID] = w;
            }
            public void startEngine()
            {
                if (Clock == null)
                {
                    Clock = new System.Windows.Forms.Timer();
                    Clock.Tick += new EventHandler(Timer_Tick);
                }
                Clock.Interval = 50;
                Clock.Enabled = true;
                tick_chemistry(true);
            }
            public void stopEngine()
            {
                Clock.Enabled = false;
                Clock.Stop();
            }

            public void Timer_Tick(object sender, EventArgs eArgs)
            {
                if (sender == Clock)
                {

                    tick_chemistry(false);
                }
            }
            public double getChem(String ID)
            {
                double v1 = 0;
                try
                {
                    v1 = (double)Double.Parse((string)Chemicals[ID]);
                }
                catch (Exception e)
                {
                    Chemicals[ID] = "0.0";// (double)0;
                }
                return v1;
            }
            public void addChemical(String ID, double v)
            {
                addChemical(ID, v, "atomic");
            }

            public void addChemical(String ID, double v, String note)
            {
                if (ID.Length == 0) return;
                double v1 = 0;
                if (m_cBus != null)
                {
                    m_cBus.enqueue("CHEMCMD", String.Format("addChemical,{0},{1}", ID, v));
                }

                try
                {
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
            public void subChemical(String ID, double v)
            {
                subChemical(ID, v, "atomic");
            }
            public void subChemical(String ID, double v, String note)
            {
                if (ID.Length == 0) return;
                double v1 = 0;
                if (m_cBus != null)
                {
                    m_cBus.enqueue("CHEMCMD", String.Format("subChemical,{0},{1}", ID, v));
                }
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
                if (Chemicals !=null) Chemicals[ID] = v1;
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
            }
            public void setBlackBoard(String ID, int v)
            {
                if (v > 255) v = 255;
                if (v < 0) v = 0;
                if (BlackBoard !=null) BlackBoard[ID] = v;
                if (m_cBus != null)
                {
                    m_cBus.enqueue("CHEMCMD", String.Format("setblackboard,{0},{1}", ID, v));
                }
            }
            public int getBlackBoard(string ID)
            {
                int v = 0;
                try
                {
                    v = Int32.Parse((string)BlackBoard[ID]);
                }
                catch (Exception E)
                {
                    v = (int)BlackBoard[ID];

                }
                return v;
            }

            public void tick_chemistry(bool full)
            {
                if (m_cBus != null)
                {
                    if (full)
                    {
                        Halflifes = m_cBus.getHashTable("ChemHalflifes");
                        while (Halflifes == null)
                        {
                            Application.DoEvents();
                            Halflifes = m_cBus.getHashTable("ChemHalflifes");
                          
                        }
                        Settings = m_cBus.getHashTable("ChemSettings");
                        IORules = m_cBus.getHashTable("ChemIORules");
                        
                        Stack stk = new Stack();
                        foreach (string key in IORules.Keys)
                        {
                            stk.Push(key);
                        }
                        while (stk.Count > 0)
                        {
                            string key = (string) stk.Pop();
                            if (IORules[key].GetType() == typeof(string))
                            {
                                IORules[key] = SoupIORuleProxy.Parse((string)IORules[key]);
                            }
                        }
                    }
                    BlackBoard = m_cBus.getHashTable("ChemBlackBoard");
                    subsumptionBlackBoard = m_cBus.getHashTable("ChemsubsumptionBlackBoard");
                    Chemicals = m_cBus.getHashTable("ChemChemicals");


                }
                biochemticks++;
                // Notify interested parties
                foreach (string k in chemWatchers.Keys)
                {
                    chemUpdateWatcher watcher = (chemUpdateWatcher) chemWatchers[k];
                    if (watcher != null)
                    {
                        watcher();
                    }
                
                }
            }

            public void send_chemistry(bool full)
            {
                if (m_cBus != null)
                {
                    if (full)
                    {
                        m_cBus.setHashTable("ChemSettings", Settings);
                        m_cBus.setHashTable("ChemIORules", IORules);
                        m_cBus.setHashTable("ChemHalflifes", Halflifes);
                    }
                    m_cBus.setHashTable("ChemBlackBoard", BlackBoard);
                    m_cBus.setHashTable("ChemsubsumptionBlackBoard", subsumptionBlackBoard);
                    m_cBus.setHashTable("ChemChemicals", Chemicals);

                }
            }

            public string getSubsumptionBlackboard(string ID)
            {
                string v = "";
                try
                {
                    if (subsumptionBlackBoard.ContainsKey(ID)) v = (string)subsumptionBlackBoard[ID];
                }
                finally
                {

                }
                return v;
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
                    int v = getBlackBoard(key);
                    string[] keyfield = key.Split(':');
                    if (keyfield.Length > 0)
                    {
                        string val = keyfield[keyfield.Length - 1];
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

            public void genSubsumptionBlackboardEq()
            {
                // sorts through the Blackboard looking for terms of the form "Line:...=value"
                // and makes sure the highest scoring "line:...=" will have the value 
                // so the best response for "TTS:Pain:..." will be present at "TTS:Pain"
                // similar to the old pandamonium model
                Hashtable priority = new Hashtable();
                foreach (string key in BlackBoard.Keys)
                {
                    int v = getBlackBoard(key);
                    string[] keyfield = key.Split('=');
                    if (keyfield.Length > 0)
                    {
                        string val = keyfield[keyfield.Length - 1];
                        string line = key.Replace("=" + val, "");
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

            public List<KeyValuePair<String, int>> SortedBlackBoard()
            {
                // Declare List
                List<KeyValuePair<String, int>> myList = new List<KeyValuePair<String, int>>();
                foreach (String key in BlackBoard.Keys)
                {
                    myList.Add(new KeyValuePair<string, int>(key, getBlackBoard(key)));
                }
                // Sort the list passing in a delegate comparator
                myList.Sort(delegate(KeyValuePair<String, int> x, KeyValuePair<String, int> y) { return y.Value.CompareTo(x.Value); });
                return myList;
            }


        }

        public class SoupIORuleProxy
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
            public bool digital = false;
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

            public SoupIORuleProxy(String _ID, String _Locus, String _Chem, string Options, double _Threshold, double _gain, double _nom, double _rate)
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
                return String.Format("addIORule,{0},{1},{2},{3},{4},{5},{6},{7}", ID, Locus, Chem, bitsToString(), threshold, gain, nominal, rate);
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
    
}
