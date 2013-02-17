using System;
using System.Collections;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using AltAIMLbot.Utils;
using MushDLR223.Virtualization;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class markov : AIMLTagHandlerU
    {

        public markov(AltBot bot,
                      User user,
                      SubQuery query,
                      Request request,
                      Result result,
                      XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChangeU()
        {
            if (templateNode.Name.ToLower() == "markov")
            {
                //return templateNodeInnerText.ToValue().ToUpper(this.Proc.Locale);
                Unifiable names = GetAttribValue("source", null);
                if (!Unifiable.IsIncomplete(names))
                {
                    // Want something special

                    // source can contain a set of names seperated by spaces that define the language models to
                    // be used. So you can say source="general.trn mandy.trn angry.ngm"

                    Proc.STM_Brain.Clear();
                    string names_str = names;
                    string[] nameset = names_str.Split(' ');
                    foreach (string name in nameset)
                    {

                        string file = HostSystem.Combine("trn", name);
                        //if (Directory.Exists(file))
                        if (HostSystem.FileExists(file))
                        {
                            AltBot.writeDebugLine("LoadMarkovSTM: '{0}'", file);
                            StreamReader sr = HostSystem.GetStreamReader(file);
                            Proc.STM_Brain.WindowSize = 4;
                            Proc.STM_Brain.Learn(sr);
                            sr.Close();
                        }

                        file = HostSystem.Combine("ngm", name);
                        //if (Directory.Exists(file))
                        if (HostSystem.FileExists(file))
                        {
                            AltBot.writeDebugLine("LoadMarkovSTM: '{0}'", file);
                            StreamReader sr = HostSystem.GetStreamReader(file);
                            Proc.STM_Brain.WindowSize = 3;
                            Proc.STM_Brain.LearnNgram(sr);
                            sr.Close();
                        }
                    }
                    String line = templateNodeInnerText.ToValue(query);

                    // This section splits the line into words and looks for the words as files
                    // in each directory and loads them if found. Probably should be non-stopwords.

                    string words_str = line.ToLower();
                    string[] wordset = words_str.Split(' ');
                    foreach (string name in wordset)
                    {

                        string file = HostSystem.Combine("trn", name);
                        //if (Directory.Exists(file))
                        if (HostSystem.FileExists(file))
                        {
                            AltBot.writeDebugLine("LoadMarkovSTM: '{0}'", file);
                            StreamReader sr = HostSystem.GetStreamReader(file);
                            Proc.STM_Brain.WindowSize = 4;
                            Proc.STM_Brain.Learn(sr);
                            sr.Close();
                        }

                        file = HostSystem.Combine("ngm", name);
                        //if (Directory.Exists(file))
                        if (HostSystem.FileExists(file))
                        {
                            AltBot.writeDebugLine("LoadMarkovSTM: '{0}'", file);
                            StreamReader sr = HostSystem.GetStreamReader(file);
                            Proc.STM_Brain.WindowSize = 3;
                            Proc.STM_Brain.LearnNgram(sr);
                            sr.Close();
                        }
                    }


                    line = Proc.STM_Brain.GetResponse(line);
                    if (line == null) line = Proc.STM_Brain.GetRandomResponse();
                    Unifiable result = line;
                    // AltBot.writeDebugLine(line);
                    return result;

                }
                else
                {
                    // Use the generic load
                    String line = templateNodeInnerText.ToValue(query);
                    line = Proc.MBrain.GetResponse(line);
                    if (line == null) line = Proc.MBrain.GetRandomResponse();
                    Unifiable result = line;
                    // AltBot.writeDebugLine(line);
                    return result;
                }
            }
            return Unifiable.Empty;
        }
    }

//  A markovian text generator similar to MegaHal
// given the original text as input it tries to generate new text using the language model from the .trn file
// Original source http://svn.adammil.net/viewvc.cgi/?root=HalBot

    [Serializable]
    public class Mnode : IComparable
    { public Mnode(string text) { Text=text; }

        public Mnode AddChild(string text)
        { Mnode n = GetNode(text);
            if(n==null)
            { n = new Mnode(text);
                if(NumChildren==0) Children = new Mnode[16];
                else if(NumChildren==Children.Length)
                { Mnode[] narr = new Mnode[NumChildren*2];
                    Array.Copy(Children, narr, NumChildren);
                    Children = narr;
                }
                Children[NumChildren++] = n;
                Array.Sort(Children, 0, NumChildren);
            }
            n.Count++; Total++;
            return n;
        }

        public Mnode AddChild(string text,int occurances)
        {
            Mnode n = GetNode(text);
            if (n == null)
            {
                n = new Mnode(text);
                if (NumChildren == 0) Children = new Mnode[16];
                else if (NumChildren == Children.Length)
                {
                    Mnode[] narr = new Mnode[NumChildren * 2];
                    Array.Copy(Children, narr, NumChildren);
                    Children = narr;
                }
                Children[NumChildren++] = n;
                Array.Sort(Children, 0, NumChildren);
            }
            //n.Count++; Total++;
            n.Count+=occurances; Total+=occurances;
            return n;
        }


        public int CompareTo(object obj) { return string.Compare(Text, ((Mnode)obj).Text); }
        public override int GetHashCode() { return Text==null ? 0 : Text.GetHashCode(); }

        public Mnode GetNode(string text)
        { if(NumChildren==0) return null;
            //if(text==null) return Children[0].Text==null ? Children[0] : null;
            int index = Array.BinarySearch(Children, 0, NumChildren, text, Compare);
            return index<0 ? null : Children[index];
        }
  
        public Mnode RandomNode()
        { if(NumChildren==0) return null;
            int n = Rand.Next(Total);
            while(n>0)
            { n -= Children[randIndex].Count;
                if(++randIndex>=NumChildren) randIndex=0;
            }
            return Children[randIndex];
        }

        public Mnode RandomNode(Hashtable keyHash)
        {
            if (NumChildren == 0) return null;
            int n = Rand.Next(Total);
            while (n > 0)
            {
                n -= Children[randIndex].Count;
                if (++randIndex >= NumChildren) randIndex = 0;
                if ((Children[randIndex].Text!=null)&&(keyHash.ContainsKey(Children[randIndex].Text))&&((float)Rand.NextDouble()<(float)keyHash[Children[randIndex].Text])) n=0;
            }
            return Children[randIndex];
        }

        public Mnode[] Children;
        public string Text;
        public int    Count, NumChildren, Total;
  
        int randIndex;
  
        sealed class Comparer : IComparer
        { public int Compare(object a, object b)
        { Mnode na=a as Mnode, nb=b as Mnode;
            return string.Compare(na==null ? (string)a : na.Text, nb==null ? (string)b : nb.Text);
        }
        }

        static readonly Random Rand = new Random();
        static readonly Comparer Compare = new Comparer();
    }

    [Serializable]
    public class MBrain
    { static MBrain()
    { Array.Sort(badkeywords);
        Array.Sort(greetings);

        #region keyword swapping
        string[] swaps = new string[]
                             { "dislike",    "like",
                               "hate",       "love",
                               "i",          "you",
                               "i'd",        "you'd",
                               "i'll",       "you'll",
                               "i'm",        "you're",
                               "i've",       "you've",
                               "like",       "dislike",
                               "love",       "hate",
                               "me",         "you",
                               "mine",       "yours",
                               "my",         "your",
                               "myself",     "yourself",
                               "no",         "yes",
                               "why",        "because",
                               "yes",        "no",
                               "you",        "i",
                               "you",        "me",
                               "you'd",      "i'd",
                               "you'll",     "i'll",
                               "you're",     "i'm",
                               "you've",     "i've",
                               "your",       "my",
                               "yours",      "mine",
                               "yourself",   "myself",
                             };
        #endregion
    
        #region normalization
        string[] normals = new string[]
                               { "arent",         "aren't",
                                 "cant",          "can't",
                                 "couldnt",       "couldn't",
                                 "couldve",       "could've",
                                 "didnt",         "didn't",
                                 "doesnt",        "doesn't",
                                 "dont",          "don't",
                                 "hadnt",         "hadn't",
                                 "hasnt",         "hasn't",
                                 "havent",        "haven't",
                                 "heres",         "here's",
                                 "id",            "i'd",
                                 "ill",           "i'll", // this is ugly, but most likely correct
                                 "im",            "i'm",
                                 "isnt",          "isnt",
                                 "itll",          "it'll",
                                 "ive",           "i've",
                                 "shouldnt",      "shouldn't",
                                 "shouldve",      "should've",
                                 "thats",         "that's",
                                 "theres",        "there's",
                                 "theyd",         "they'd",
                                 "theyll",        "they'll",
                                 "theyve",        "they've",
                                 "wasnt",         "wasn't",
                                 "weve",          "we've",
                                 "whats",         "what's",
                                 "whered",        "where'd",
                                 "wheres",        "where's",
                                 "whos",          "who's",
                                 "whove",         "who've",
                                 "wont",          "won't",
                                 "wouldnt",       "wouldn't",
                                 "wouldve",       "would've",
                                 "youd",          "you'd",
                                 "youll",         "you'll",
                                 "youre",         "you're",
                                 "youve",         "you've",
                               };
        #endregion
    
        #region spelling correction
        string[] spells = new string[]
                              { "acn",         "can",
                                "ahve",        "have",
                                "amde",        "made",
                                "benifit",     "benefit",
                                "cna",         "can",
                                "coudl",       "could",
                                "coudln't",    "couldn't",
                                "coudlnt",     "couldn't",
                                "coudn't",     "couldn't",
                                "coudnt",      "couldn't",
                                "doign",       "doing",
                                "doimg",       "doing",
                                "doind",       "doing",
                                "dosn't",      "doesn't",
                                "dosnt",       "doesn't",
                                "ehr",         "her",
                                "ehre",        "here",
                                "esle",        "else",
                                "eyt",         "yet",
                                "fidn",        "find",
                                "firts",       "first",
                                "frmo",        "from",
                                "fwe",         "few",
                                "gloabl",      "global",
                                "gonig",       "going",
                                "gruop",       "group",
                                "gruops",      "groups",
                                "grwo",        "grow",
                                "haev",        "have",
                                "happend",     "happened",
                                "hda",         "had",
                                "hlep",        "help",
                                "hsa",         "has",
                                "hsi",         "his",
                                "htat",        "that",
                                "hte",         "the",
                                "htere",       "there",
                                "htese",       "these",
                                "htey",        "they",
                                "hting",       "thing",
                                "htink",       "think",
                                "htis",        "this",
                                "hvae",        "have",
                                "hvaing",      "having",
                                "hwich",       "which",
                                "idae",        "idea",
                                "idaes",       "idea",
                                "ihs",         "his",
                                "iit",         "it",
                                "iits",        "its",
                                "includ",      "include",
                                "iwll",        "will",
                                "iwth",        "with",
                                "jsut",        "just",
                                "knwo",        "know",
                                "knwos",       "knows",
                                "konw",        "know",
                                "konwn",       "known",
                                "konws",       "knows",
                                "lenght",      "length",
                                "levle",       "level",
                                "libary",      "library",
                                "liek",        "like",
                                "liekd",       "liked",
                                "liev",        "live",
                                "likly",       "likely",
                                "littel",      "little",
                                "litttle",     "little",
                                "liuke",       "like",
                                "loev",        "love",
                                "lookign",     "looking",
                                "makeing",     "making",
                                "mkae",        "make",
                                "mkaes",       "makes",
                                "mkaing",      "making",
                                "moeny",       "money",
                                "mroe",        "more",
                                "mysefl",      "myself",
                                "myu",         "my",
                                "nkow",        "know",
                                "nver",        "never",
                                "nwe",         "new",
                                "nwo",         "now",
                                "ohter",       "other",
                                "omre",        "more",
                                "onyl",        "only",
                                "otehr",       "other",
                                "otu",         "out",
                                "owrk",        "work",
                                "owuld",       "would",
                                "peice",       "piece",
                                "peices",      "pieces",
                                "peolpe",      "people",
                                "peopel",      "people",
                                "poeple",      "people",
                                "porblem",     "problem",
                                "porblems",    "problems",
                                "porvide",     "provide",
                                "probelm",     "problem",
                                "probelms",    "problems",
                                "psoition",    "position",
                                "ptogress",    "progess",
                                "pwoer",       "power",
                                "realyl",      "really",
                                "recrod",      "record",
                                "remeber",     "remember",
                                "reult",       "result",
                                "rwite",       "write",
                                "seh",         "she",
                                "seperate",    "separate",
                                "shcool",      "school",
                                "shoudl",      "should",
                                "shoudln't",   "shouldn't",
                                "shoudlnt",    "shouldn't",
                                "showinf",     "showing",
                                "signifacnt",  "significant",
                                "simpyl",      "simply",
                                "sitll",       "still",
                                "smae",        "same",
                                "smoe",        "some",
                                "soem",        "some",
                                "sohw",        "show",
                                "sombody",     "somebody",
                                "someting",    "something",
                                "somewaht",    "somewhat",
                                "somtimes",    "sometimes",
                                "soudn",       "sound",
                                "soudns",      "sounds",
                                "stnad",       "stand",
                                "stpo",        "stop",
                                "stpos",       "stops",
                                "tahn",        "than",
                                "taht",        "that",
                                "talekd",      "talked",
                                "talkign",     "talking",
                                "taht",        "that",
                                "teh",         "the",
                                "tehy",        "they",
                                "tghe",        "the",
                                "tghis",       "this",
                                "thansk",      "thanks",
                                "theri",       "their",
                                "they'l",      "they'll",
                                "theyl",       "they'll",
                                "they'r",      "they're",
                                "theyr",       "they're",
                                "thgat",       "that",
                                "thge",        "the",
                                "thier",       "their",
                                "thigsn",      "things",
                                "thna",        "than",
                                "thne",        "then",
                                "thnig",       "thing",
                                "thnigs",      "things",
                                "thsi",        "this",
                                "thsoe",       "those",
                                "thta",        "that",
                                "tihs",        "this",
                                "timne",       "time",
                                "tje",         "the",
                                "tjhe",        "the",
                                "tkae",        "take",
                                "tkaes",       "takes",
                                "todya",       "today",
                                "tomorow",     "tomorrow",
                                "tongiht",     "tonight",
                                "tonihgt",     "tonight",
                                "totaly",      "totally",
                                "totalyl",     "totally",
                                "towrad",      "toward",
                                "tthe",        "the",
                                "tyhat",       "that",
                                "tyhe",        "the",
                                "udnerstand",  "understand",
                                "understnad",  "understand",
                                "unliek",      "unlike",
                                "untill",      "until",
                                "useing",      "using",
                                "usualyl",     "usually",
                                "veyr",        "very",
                                "virtualyl",   "virutally",
                                "vrey",        "very",
                                "waht",        "what",
                                "watn",        "want",
                                "wehn",        "when",
                                "wern't",      "weren't",
                                "wernt",       "weren't",
                                "werre",       "were",
                                "whcih",       "which",
                                "wherre",      "where",
                                "whic",        "which",
                                "whihc",       "which",
                                "whta",        "what",
                                "wief",        "wife",
                                "wierd",       "weird",
                                "wihch",       "which",
                                "wiht",        "with",
                                "withe",       "with",
                                "wiull",       "will",
                                "wnat",        "want",
                                "wnated",      "watned",
                                "wnats",       "wants",
                                "woh",         "who",
                                "wohle",       "whole",
                                "wokr",        "work",
                                "woudl",       "would",
                                "woudln't",    "wouldn't",
                                "woudlnt",     "wouldn't",
                                "wriet",       "write",
                                "writting",    "writing",
                                "wrod",        "word",
                                "wroet",       "wrote",
                                "wroking",     "working",
                                "wtih",        "with",
                                "wuould",      "would",
                                "wya",         "way",
                                "yera",        "year",
                                "yeras",       "years",
                                "yersa",       "years",
                                "yoiu",        "you",
                                "ytou",        "you",
                                "yuo",         "you",
                                "yuor",        "you",
                              };
        #endregion
    
        for(int i=0; i<swaps.Length; i+=2) swap[swaps[i]] = swaps[i+1];
        for(int i=0; i<normals.Length; i+=2) normalize[normals[i]] = normals[i+1];
        for(int i=0; i<spells.Length; i+=2) spelling[spells[i]] = spells[i+1];
    }

        // This value must be at least 1. If it's set to higher values, the text
        // will be more similar to the training text.
        public int WindowSize = 4;

        public void Clear()
        { fore.Clear();
            back.Clear();
            lastReply=null;
        }

        public string GetRandomResponse()
        { if(fore.Count==0) return "Sorry, I have no brain.";

            DictionaryEntry[] des = new DictionaryEntry[fore.Count];
            Mnode bnode=null, fnode=null;
            string keyword;
            int i;

            fore.CopyTo(des, 0);
            for(i=0; i<50; i++)
            { DictionaryEntry de = des[rand.Next(des.Length)];
                keyword = ValidKeyword((string)de.Key);
                if(keyword!=null)
                { bnode=(Mnode)back[keyword]; fnode=(Mnode)fore[keyword];
                    if(fnode!=null && bnode!=null) break;
                }
            }
            if(i==50)
                for(i=0; i<25; i++)
                { DictionaryEntry de = des[rand.Next(des.Length)];
                    keyword = ValidKeyword((string)de.Key);
                    if(keyword!=null)
                    { bnode=(Mnode)back[keyword]; fnode=(Mnode)fore[keyword];
                        if(fnode!=null) break;
                    }
                }
            if(i==25) return null;

            float probability;
            ArrayList list = GetResponse(bnode, fnode, out probability);

            return WordsToString(list);
        }
        public string GetResponse(string input)
        {
            //Learn(input, false); // autolearn what we are responding to ...

            string[] bits = Tokenize(input, false);
            if (bits.Length == 0) return null;
            string[] obits = Tokenize(input.ToLower(), false);
            CorrectSpelling(bits);
            for (int i = 0; i < bits.Length; i++) bits[i] = ValidKeyword(bits[i]);
      
            Hashtable keyHash = new Hashtable() ;
            for (int i = 0; i < bits.Length; i++)
            {
                //keyHash[obits[i]] = 1;
                if (bits[i]!=null) keyHash[bits[i]]=0.5f;
            }
            keyHash["?"] = 0.5f;
            keyHash["!"] = 0.5f;
            keyHash["."] = 0.5f;

            Response bestResponse = new Response();
            float bestd = float.MaxValue;
            int besti = 0;
            float bestskew = float.MaxValue;

            // Pick an ideal probability value that is high and find response that is closest
            // if all probabilities are below 0.66 then it picks the highest
            //float target = (float)rand.NextDouble() / 3 + 2 / 3f; // for 0 ...1 space
            float target = ((float)rand.NextDouble() * -10f) + (-0.3f); // for log space

            DateTime starttime = AltBot.Now;
            TimeSpan duration;
            long thinkticks = 0;
            do
            {
                //Response[] resp = new Response[1000];
                //for (int i = 0; i < resp.Length; i++)
                //{
                thinkticks++;
                Response myresp = GetResponse(bits,keyHash);
                if (myresp.Text == null) return null;
                if (myresp.Text.Length > 100 || myresp.Text == lastReply) myresp.Probability /= 3;
                // multi-match boost
                int boostcount = 0;
                float pdiff =target -myresp.Probability;
                float k = 1;
                float skewv = 0;
                float alpha = 0.99f;

                for (int key_index = 0; key_index < obits.Length; key_index++)
                {
                    String keyword = obits[key_index];
                    float q_i = 0;
                    float p_i = 1; // always 1

                    if ((keyword != null) && (keyword.Length > 2) && (myresp.Text.ToLower().Contains(keyword)))
                    {
                        q_i = 1f;
                        boostcount++;
                        Mnode fnode = (Mnode)fore[keyword];
                        if (fnode != null)
                        {
                            if (fnode.Total < 10000) boostcount++;
                        }
                    }
                    skewv += (float) Math.Log(1f / (float)((alpha * q_i) + ((1 - alpha) * p_i)));

                }

                if (boostcount > 0)
                {
                    //myresp.Probability += (1 - myresp.Probability) * (boostcount / obits.Length) * (float)0.5;
                    //myresp.Probability += (pdiff* (float)0.5) * (boostcount / obits.Length) ;
                    k = (float)(1f / (float)(1f + (float)boostcount));  // we reduce the diff by the number of boostcounts we have
                }
                else
                {
                    k = 1f;
                }
                //}
                // Probability and evaluation


                //for (int i = 0; i < resp.Length; i++)
                //{
                float diff = k * Math.Abs(myresp.Probability - target);
                //if ((diff < bestd) || (diff == bestd && myresp.Text.Length < bestResponse.Text.Length) || (skewv < bestskew))
                if ((skewv == bestskew && myresp.Text.Length < bestResponse.Text.Length) || (skewv < bestskew))
                {
                    bestd = diff; bestResponse = myresp; bestskew = skewv;
                    AltBot.writeDebugLine("MKOV: {0} to {1} -->{2} {3} | tg={4}| pb={5} sk={6} tx={7}", bestd,diff, boostcount, k, target, myresp.Probability,skewv,myresp.Text);

                }
                //}
                duration = AltBot.Now - starttime;
            } while ((Math.Abs(duration.TotalMilliseconds) < 7500)||(thinkticks<3000));
            AltBot.writeDebugLine(" ----\n **** Markov generator thunk {0} turns in {1} milliseconds : {2} --> {3}@df={4} sk={5} tx={6}**** \n", thinkticks, duration.TotalMilliseconds, bestResponse.Keyword, bestResponse.Probability, bestd, bestskew,bestResponse.Text);
            return lastReply = bestResponse.Text;
        }

        public string GetResponseNoTime(string input)
        { string[] bits = Tokenize(input, false);
            if(bits.Length==0) return null;
            CorrectSpelling(bits);
            for(int i=0; i<bits.Length; i++) bits[i] = ValidKeyword(bits[i]);

            Response[] resp = new Response[1000];
            for(int i=0; i<resp.Length; i++)
            { resp[i] = GetResponse(bits);
                if(resp[i].Text==null) return null;
                if(resp[i].Text.Length>100 || resp[i].Text==lastReply) resp[i].Probability /= 3;
                // multi-match boost
                int boostcount=0;
                for (int keyword = 0; keyword < bits.Length; keyword++)
                {
                    if ((bits[keyword] != null) && (resp[i].Text.Contains(bits[keyword]))) boostcount++;
                }
                if (boostcount > 1)
                {
                    resp[i].Probability += (1 - resp[i].Probability) * ( boostcount / bits.Length );
                }
            }
            // Probability and evaluation

            // Pick an ideal probability value that is high and find response that is closest
            // if all probabilities are below 0.66 then it picks the highest
            float target=(float)rand.NextDouble()/3 + 2/3f;

            float bestd=float.MaxValue;
            int   besti=0;
            for(int i=0; i<resp.Length; i++)
            { float diff = Math.Abs(resp[i].Probability-target);
                if(diff<bestd || diff==bestd && resp[i].Text.Length<resp[besti].Text.Length) { besti=i; bestd=diff; }
            }
            return lastReply=resp[besti].Text;
        }

        public void Learn(string text, bool correctSpelling)
        { if(text=="") return;
            string[] chunks = Tokenize(text, true);
            if(correctSpelling) CorrectSpelling(chunks);

            if(chunks.Length<=WindowSize) return; // Learn(chunks, 0, chunks.Length); -- dont learn from short input
            else
            { for(int i=0; i<=chunks.Length-WindowSize; i++) Learn(fore, chunks, i, WindowSize);
                for(int i=0,end=(chunks.Length-1)/2; i<end; i++)
                { int swapi = chunks.Length-2-i;
                    string tmp=chunks[i]; chunks[i]=chunks[swapi]; chunks[swapi]=tmp;
                }
                for(int i=0; i<=chunks.Length-WindowSize; i++) Learn(back, chunks, i, WindowSize);
            }
        }
        public void LearnNGram(string intext, bool correctSpelling)
        {
            if (intext == "") return;
            string[] splitline = intext.Split(new char[] {'\t'});
            if (splitline.Length < 1) return;
            string text = splitline[0].ToLower();
            string ng_count = splitline[1];
            int occurances = 100000;
            try
            {
                occurances = int.Parse(ng_count);
            }
            catch (Exception e)
            {
                occurances = 100000;
            }
            if (occurances >100000) occurances=100000; // Truncate to 100k to prevent blow off
            if (occurances <= 6000) return;
            if (text.Length < 1) return;

            text = text.Replace("  ", "");
            text = text.Replace("<s>", "");
            text = text.Replace("</s>", ".");
            text = text.Replace("<unk>", "something");
            text = text.Replace("<unk/>", "something");

            //AltBot.writeDebugLine("NGram-Learn {0} -> {1}", text, occurances);
            string[] chunks = Tokenize(text, true);
            if (correctSpelling) CorrectSpelling(chunks);

            if (chunks.Length <= WindowSize) return; // Learn(chunks, 0, chunks.Length); -- dont learn from short input
            else
            {
                for (int i = 0; i <= chunks.Length - WindowSize; i++) Learn(fore, chunks, i, WindowSize, occurances);
                for (int i = 0, end = (chunks.Length - 1) / 2; i < end; i++)
                {
                    int swapi = chunks.Length - 2 - i;
                    string tmp = chunks[i]; chunks[i] = chunks[swapi]; chunks[swapi] = tmp;
                }
                for (int i = 0; i <= chunks.Length - WindowSize; i++) Learn(back, chunks, i, WindowSize, occurances);
            }
        }

        public void Learn(TextReader tr)
        { string line;
            long linecount = 0;

            while ((linecount < 80000) && ((line = tr.ReadLine()) != null))
            {
                linecount++;
                if (linecount % 1000 == 0) { AltBot.writeDebugLine("Mlearn {0}", linecount); }
                line = line.Trim();
                if(line.Length!=0 && line[0]!='#') Learn(line, false);
            }
            AltBot.writeDebugLine("Last Line Mlearn {0}", linecount);
        }

        public void LearnNgram(TextReader tr)
        {
            string line;
            long linecount = 0;
            try
            {
                while ((linecount < 8000000) && ((line = tr.ReadLine()) != null))
                {
                    linecount++;
                    if (linecount % 1000 == 0) { AltBot.writeDebugLine("NG-learn {0}", linecount); }
                    line = line.Trim();
                    if (line.Length != 0 && line[0] != '#') LearnNGram(line, false);
                }
            }
            catch (Exception e)
            {
            }
            AltBot.writeDebugLine("Last Line NG-learn {0}", linecount);
        }

        struct Response
        { public Response(string keyword, string text, float probability)
        { Keyword=keyword; Text=text; Probability=probability;
        }

            public string Keyword, Text;
            public float  Probability;
        }

        void AddWords(ArrayList list, Hashtable nodes, Mnode node, ref float probability)
        { while(true)
        { Mnode child = node.RandomNode();
            if(child==null)
            { int k = list.Count-WindowSize+1;
                node = (Mnode)nodes[(string)list[k]];
                while(node!=null && ++k<list.Count) node = node.GetNode((string)list[k]);
                if(node!=null) child = node.RandomNode();
                if(child==null) break;
            }
            if(child.Text==null) break; // we fell off the edge
            list.Add(child.Text);
            double rawp = (double)child.Count / (double)node.Total;
            probability += (float)Math.Log(rawp);
            node = child;
        }
        }

        void AddWords(ArrayList list, Hashtable nodes, Mnode node, Hashtable keyHash,ref float probability)
        {
            while (true)
            {
                Mnode child = node.RandomNode(keyHash);
                if (child == null)
                {
                    int k = list.Count - WindowSize + 1;
                    node = (Mnode)nodes[(string)list[k]];
                    while (node != null && ++k < list.Count) node = node.GetNode((string)list[k]);
                    if (node != null) child = node.RandomNode();
                    if (child == null) break;
                }
                if (child.Text == null) break; // we fell off the edge
                list.Add(child.Text);
                double rawp = (double)child.Count / (double)node.Total;
                probability += (float)Math.Log(rawp);
                node = child;
            }
        }

        Response GetResponse(string[] bits)
        { string keyword=null;
            bool greeting=false;
            // Goal 1: pick a keyword to focus on
            //         - if it is a greeting then use that
            //         - otherwise pick some word at random from the input
            for(int i=0; i<bits.Length; i++)
                if(bits[i]!=null && Array.BinarySearch(greetings, bits[i])>=0) { keyword=bits[i]; greeting=true; break; }

            int searchIndex = -1, keywordIndex = 0;
            if(!greeting)
            { int tries;
                // Hunt and peck until we find something
                for (tries = 0; tries < bits.Length*10; tries++)
                { keywordIndex = rand.Next(bits.Length);
                    keyword = bits[keywordIndex];
                    if(keyword != null) break;
                }
                // Ok linear search with first non-zero
                if (tries == bits.Length * 10)
                { for(searchIndex=0; searchIndex<bits.Length; searchIndex++)
                    if(bits[searchIndex] != null)
                    { keywordIndex = searchIndex;
                        keyword = bits[keywordIndex];
                        break;
                    }
                    if(searchIndex == bits.Length) return new Response();
                }
            }

            // given the keyword see if you can find a node with forward / backward extensions

            Mnode bnode, fnode;
            string fallback=null, fallback2=null;
            do
            { bnode=(Mnode)back[keyword]; fnode=(Mnode)fore[keyword];
                if(fnode!=null)
                { if(greeting) break;
                    if(fallback==null) fallback=keyword;
                }
                else if(bnode!=null) fallback2=keyword;

                if((bnode!=null || keywordIndex == 0) && (fnode!=null || keywordIndex==bits.Length-1)) break;

                // if selected keword is bad then try the next word in the input as the keyword
                while(++searchIndex < bits.Length)
                    if(bits[searchIndex]!=null)
                    { keywordIndex = searchIndex;
                        keyword = bits[keywordIndex];
                        break;
                    }
            } while((bnode==null || fnode==null) && searchIndex<bits.Length);

            if(searchIndex == bits.Length)
            { if(fallback!=null) keyword=fallback;
            else return new Response();
            }

            // having the two start point nodes, generate a word list
            float probability;
            ArrayList list = GetResponse(bnode, fnode, out probability);

            return new Response(keyword, WordsToString(list), probability/list.Count);
        }

        Response GetResponse(string[] bits,Hashtable keyHash)
        {
            string keyword = null;
            bool greeting = false;
            // Goal 1: pick a keyword to focus on
            //         - if it is a greeting then use that
            //         - otherwise pick some word at random from the input
            for (int i = 0; i < bits.Length; i++)
                if (bits[i] != null && Array.BinarySearch(greetings, bits[i]) >= 0) { keyword = bits[i]; greeting = true; break; }

            int searchIndex = -1, keywordIndex = 0;
            if (!greeting)
            {
                int tries;
                // Hunt and peck until we find something
                for (tries = 0; tries < bits.Length * 10; tries++)
                {
                    keywordIndex = rand.Next(bits.Length);
                    keyword = bits[keywordIndex];
                    if (keyword != null) break;
                }
                // Ok linear search with first non-zero
                if (tries == bits.Length * 10)
                {
                    for (searchIndex = 0; searchIndex < bits.Length; searchIndex++)
                        if (bits[searchIndex] != null)
                        {
                            keywordIndex = searchIndex;
                            keyword = bits[keywordIndex];
                            break;
                        }
                    if (searchIndex == bits.Length) return new Response();
                }
            }

            // given the keyword see if you can find a node with forward / backward extensions

            Mnode bnode, fnode;
            string fallback = null, fallback2 = null;
            do
            {
                bnode = (Mnode)back[keyword]; fnode = (Mnode)fore[keyword];
                if (fnode != null)
                {
                    if (greeting) break;
                    if (fallback == null) fallback = keyword;
                }
                else if (bnode != null) fallback2 = keyword;

                if ((bnode != null || keywordIndex == 0) && (fnode != null || keywordIndex == bits.Length - 1)) break;

                // if selected keword is bad then try the next word in the input as the keyword
                while (++searchIndex < bits.Length)
                    if (bits[searchIndex] != null)
                    {
                        keywordIndex = searchIndex;
                        keyword = bits[keywordIndex];
                        break;
                    }
            } while ((bnode == null || fnode == null) && searchIndex < bits.Length);

            if (searchIndex == bits.Length)
            {
                if (fallback != null) keyword = fallback;
                else return new Response();
            }

            // having the two start point nodes, generate a word list
            float probability;
            ArrayList list = GetResponse(bnode, fnode,keyHash, out probability);

            return new Response(keyword, WordsToString(list), probability / list.Count);
        }


        ArrayList GetResponse(Mnode bnode, Mnode fnode, out float probability)
        { ArrayList list = new ArrayList();
            probability=0;
            // add words to the output
            if(bnode!=null)
            { list.Add(bnode.Text);
                AddWords(list, back, bnode, ref probability);
                list.Reverse();
            }

            if(fnode!=null)
            { if(bnode==null) list.Add(fnode.Text);
                AddWords(list, fore, fnode, ref probability);
            }

            return list;
        }

        ArrayList GetResponse(Mnode bnode, Mnode fnode,Hashtable keyHash, out float probability)
        {
            ArrayList list = new ArrayList();
            probability = 0;
            // add words to the output
            if (bnode != null)
            {
                list.Add(bnode.Text);
                AddWords(list, back, bnode,keyHash, ref probability);
                list.Reverse();
            }

            if (fnode != null)
            {
                if (bnode == null) list.Add(fnode.Text);
                AddWords(list, fore, fnode,keyHash, ref probability);
            }

            return list;
        }



        Hashtable fore=new Hashtable(), back=new Hashtable();
        string lastReply;

        static Mnode AddNode(Hashtable nodes, string text)
        { Mnode n = (Mnode)nodes[text];
            if(n==null) nodes[text] = n = new Mnode(text);
            n.Count++;
            return n;
        }
        static Mnode AddNode(Hashtable nodes, string text,int occurances)
        {
            Mnode n = (Mnode)nodes[text];
            if (n == null) nodes[text] = n = new Mnode(text);
            //n.Count++;
            n.Count += occurances;
            return n;
        }

        static void CorrectSpelling(string[] words)
        { for(int i=0; i<words.Length; i++)
        { string word = words[i];
            if(word!=null && char.IsLetter(word[0]))
            { string spel = (string)spelling[word];
                if(spel!=null) words[i]=spel;
            }
        }
        }

        static void Learn(Hashtable nodes, string[] chain, int index, int length)
        { Mnode n = AddNode(nodes, chain[index]);
            for(int i=1; i<length; i++) n = n.AddChild(chain[index+i]);
        }
        static void Learn(Hashtable nodes, string[] chain, int index, int length,int occurances)
        {
            Mnode n = AddNode(nodes, chain[index]);
            for (int i = 1; i < length; i++) n = n.AddChild(chain[index + i],occurances);
        }

        public static string[] Tokenize(string text, bool includeNull)
        { MatchCollection ms = wordre.Matches(text.Trim().ToLower());
            string[] ret = new string[ms.Count + (includeNull ? 1 : 0)];
            for(int i=0; i<ms.Count; i++)
            { string normal = (string)normalize[ms[i].Value];
                ret[i] = normal==null ? ms[i].Value : normal;
            }
            return ret;
        }
  
        static string WordsToString(IList words)
        { StringBuilder sb = new StringBuilder();
            bool quote=false, paren=false, sns=false;
            foreach(string s in words)
            { char c=s[0];
                if(c=='"')
                { if(!quote) { sb.Append(' '); sns=true; }
                    quote = !quote;
                }
                else if(c=='(')
                { if(!paren) { sb.Append(' '); sns=true; }
                    paren = !paren;
                }
                else if(c=='$' || c=='*' || c=='&') { sns=true; sb.Append(' '); }
                else if(c=='/') sns=true;
                else if(sns) sns=false;
                else if((char.IsLetterOrDigit(s[0]) || s=="--") && sb.Length!=0) sb.Append(' ');
                sb.Append(s);
            }
            return sb.ToString();
        }

        static string ValidKeyword(string word)
        { if(!char.IsLetterOrDigit(word[0])) return null;

            if(Array.BinarySearch(badkeywords, word)>=0) return null;
            string complement = (string)swap[word];

            return complement!=null && rand.Next(100)<75 ? complement : word;
        }

        static readonly Regex wordre = new Regex(@"(\w[\w\-']*|[`~!@#$%^&*()+=\-\[\]{};:"",.<>/\\?|]+)", RegexOptions.Singleline|RegexOptions.Compiled);
        static readonly Random rand = new Random();
        static readonly SortedList swap=new SortedList(), normalize=new SortedList(), spelling=new SortedList();

        static readonly string[] greetings = { "g'day", "greetings", "hello", "hi", "howdy", "welcome" };

        #region badkeywords
        static readonly string[] badkeywords1 =
            { "a", "ability", "able", "about", "absolute", "absolutely", "across", "actual", "actually", "after", "afternoon",
              "again", "against", "ago", "agree", "all", "almost", "along", "already", "although", "always", "am", "an", "and",
              "another", "any", "anyhow", "anything", "anyway", "are", "aren't", "around", "as", "at", "away", "back",
              "bad", "be", "been", "before", "behind", "being", "believe", "belong", "best", "better", "between", "big",
              "bigger", "biggest", "bit", "both", "buddy", "but", "by", "call", "called", "calling", "came", "can", "can't",
              "cannot", "care", "caring", "case", "catch", "caught", "certain", "certainly", "change", "close",
              "closer", "come", "coming", "common", "constant", "constantly", "could", "current", "day", "days", "derived",
              "describe", "describes", "determine", "determines", "did", "didn't", "do", "does", "doesn't", "doing",
              "don't", "done", "doubt", "down", "each", "earlier", "early", "else", "enjoy", "especially", "even",
              "ever", "every", "everybody", "everyone", "everything", "fact", "fair", "fairly", "far", "fellow", "few", "find",
              "fine", "for", "form", "found", "from", "full", "further", "gave", "get", "getting", "give", "given", "giving",
              "go", "going", "gone", "good", "got", "gotten", "great", "had", "has", "hasn't", "have", "haven't",
              "having", "held", "here", "high", "hold", "holding", "how", "if", "in", "indeed", "inside", "instead", "into",
              "is", "isn't", "it", "it's", "its", "just", "keep", "kind", "knew", "know", "known", "large", "larger",
              "largest", "last", "late", "later", "least", "less", "let", "let's", "level", "likes", "little", "long",
              "longer", "look", "looked", "looking", "looks", "low", "made", "make", "making",
              "many", "mate", "may", "maybe", "mean", "meet", "mention", "mere", "might",
              "moment", "more", "morning", "most", "move", "much", "must", "near", "nearer", "never",
              "next", "nice", "nobody", "none", "noon", "noone", "not", "note", "nothing", "now", "obvious", "of", "off", "on",
              "once", "only", "onto", "opinion", "or", "other", "our", "out", "over", "own", "part", "particular",
              "particularly", "perhaps", "person", "piece", "place", "pleasant", "please", "popular", "prefer", "pretty", "put",
              "quite", "real", "really", "receive", "received", "recent", "recently", "related", "result", "resulting",
              "results", "said", "same", "saw", "say", "saying", "see", "seem", "seemed", "seems", "seen", "seldom", "sense",
              "set", "several", "shall", "short", "shorter", "should", "show", "shows", "simple", "simply", "small", "so",
              "some", "someone", "something", "sometime", "sometimes", "somewhere", "sort", "sorts", "spend", "spent", "still",
              "stuff", "such", "suggest", "suggestion", "suppose", "sure", "surely", "surround", "surrounds", "take", "taken",
              "taking", "tell", "than", "thank", "thanks", "that", "that's", "the", "their", "them", "then", "there",
              "therefore", "these", "they", "thing", "things", "this", "those", "though", "thoughts", "thouroughly", "through",
              "tiny", "to", "today", "together", "told", "tomorrow", "too", "total", "totally", "touch", "try", "twice", "under",
              "understand", "understood", "until", "up", "us", "used", "using", "usually", "various", "very", "want", "wanted",
              "wants", "was", "watch", "way", "ways", "we", "we're", "well", "went", "were", "what", "what's", "whatever",
              "when", "where", "where's", "which", "while", "whilst", "who", "who's", "whom", "will",
              "wish", "with", "within", "wonder", "wonderful", "worse", "worst", "would", "wrong", "yesterday", "yet",
            };
        static readonly string[] badkeywords =
            { "a", "ability", "able", "about", "absolute", "absolutely", "across", "actual", "actually", "after", 
              "again", "against", "ago", "agree", "all", "almost", "along", "already", "although", "always", "am", "an", "and",
              "another", "any",  "are", "aren't", "around", "as", "at", "away", "back",
              "bad", "be", "been", "before", "behind", "being", "believe", "belong", "best", "better", "between", "big",
              "bigger", "biggest", "bit", "both", "buddy", "but", "by",  "came", "can", "can't",
              "cannot", "care", "caring", "case", "catch", "caught", "certain", "certainly", "change", "close",
              "closer", "come", "coming", "common", "could", "current", "day", "days", "derived",
              "did", "didn't", "do", "does", "doesn't", "doing",
              "don't", "done", "doubt", "down", "each",  "else", "enjoy", "especially", "even",
              "ever", "every",  "fair", "fairly", "far",  "few", "find",
              "fine", "for", "form", "found", "from", "full", "further", "gave", "get", "getting", "give", "given", "giving",
              "go", "going", "gone", "good", "got", "gotten", "great", "had", "has", "hasn't", "have", "haven't",
              "having", "held", "here", "high", "hold", "holding", "how", "if", "in", "indeed",  "into",
              "is", "isn't", "it", "it's", "its", "just", "keep", "kind",
              "last", "late", "later", "least", "less", "let", "let's", "level", "likes", "little", "long",
              "longer", "look", "looked", "looking", "looks", "low", "made", "make", "making",
              "many", "mate", "may", "maybe", "mean", "meet", "mention", "mere", "might",
              "moment", "more", "morning", "most", "move", "much", "must", "near", "nearer", "never",
              "next", "nice",  "none", "noon", "noone", "not", "note", "nothing", "now", "obvious", "of", "off", "on",
              "once", "only", "onto", "opinion", "or", "other", "our", "out", "over", "own", "part", "particular",
              "particularly", "perhaps", "person", "piece", "place",  "please", "popular", "prefer", "pretty", "put",
              "quite", "real", "really", "receive", "received", "recent", "recently", "related", "result", "resulting",
              "results", "said", "same", "saw", "say", "saying", "see", "seem", "seemed", "seems", "seen", "seldom", "sense",
              "set", "several", "shall", "short", "shorter", "should", "show", "shows", "simple", "simply", "small", "so",
              "some", "someone", "something", "sometime", "sometimes", "somewhere", "sort", "sorts", "spend", "spent", "still",
              "stuff", "such", "suggest", "suggestion", "suppose", "sure", "surely",  "take", "taken",
              "taking", "tell", "than", "that", "that's", "the", "their", "them", "then", "there",
              "therefore", "these", "they", "this", "those", "thouroughly", "through",
              "to", "today", "together", "told","too", "total", "totally", "touch", "try", "twice", "under",
              "up", "us", "usually", "various", "very", 
              "was",  "way", "ways", "we", "we're", "well", "went", "were", "whatever",
              "which", "while", "whilst", "will",
              "with", "within",  "would",   "yet",
            };
        #endregion
    }
}