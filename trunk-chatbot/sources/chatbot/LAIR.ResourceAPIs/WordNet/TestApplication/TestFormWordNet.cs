using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Threading;

using LAIR.ResourceAPIs.WordNet;
using LAIR.Collections.Generic;

namespace TestApplication
{
    public partial class TestForm : Form
    {
        private WordNetEngine _wordNetEngine;

        public TestForm()
        {
            InitializeComponent();

            // create wordnet engine (use disk-based retrieval by default)
            string root = Directory.GetDirectoryRoot(".");
            //_wordNetEngine = new WordNetEngine(root + @"NLP\Resources\WordNet_3", false);
            _wordNetEngine = new WordNetEngine(root + @"dev\Wordnet30", true);

            if (!_wordNetEngine.InMemory)
                test.Text += " (will take a while)";

            // populate POS list
            foreach (WordNetEngine.POS p in Enum.GetValues(typeof(WordNetEngine.POS)))
                if (p != WordNetEngine.POS.None)
                    pos.Items.Add(p);

            pos.SelectedIndex = 0;

            // allow scrolling of synset list
            synSets.HorizontalScrollbar = true;
        }

        private void getSynSets_Click(object sender, EventArgs e)
        {
            synSets.Items.Clear();
            synSetRelations.Items.Clear();
            getRelatedSynSets.Enabled = false;

            // retrive synsets
            Set<SynSet> synSetsToShow = null;
            if (synsetID.Text != "")
            {
                try { synSetsToShow = new Set<SynSet>(new SynSet[] { _wordNetEngine.GetSynSet(synsetID.Text) }); }
                catch (Exception)
                {
                    MessageBox.Show("Invalid SynSet ID");
                    return;
                }
            }
            else
            {
                try { synSetsToShow = _wordNetEngine.GetSynSets(word.Text, (WordNetEngine.POS)pos.SelectedItem); }
                catch (Exception ex) { MessageBox.Show("Error:  " + ex); }
            }

            if (synSetsToShow.Count > 0)
                foreach (SynSet synSet in synSetsToShow)
                    synSets.Items.Add(synSet);
            else
                MessageBox.Show("No synsets found");
        }

        private void synSets_SelectedIndexChanged(object sender, EventArgs e)
        {
            synSetRelations.Items.Clear();
            getRelatedSynSets.Enabled = synSets.SelectedItem != null;

            // select a synset if none is selected
            if (synSets.SelectedItem == null)
            {
                if (synSets.Items.Count > 0)
                    synSets.SelectedIndex = 0;
            }
            else
            {
                SynSet selectedSynSet = synSets.SelectedItem as SynSet;

                // populate relation list
                synSetRelations.Items.Clear();
                foreach (WordNetEngine.SynSetRelation synSetRelation in selectedSynSet.SynSetRelations)
                    synSetRelations.Items.Add(synSetRelation + ":  " + selectedSynSet.GetRelatedSynSetCount(synSetRelation));

                // show id
                synsetID.Text = selectedSynSet.ID;
            }
        }

        private void getRelatedSynSets_Click(object sender, EventArgs e)
        {
            SynSet selectedSynSet = synSets.SelectedItem as SynSet;

            if (selectedSynSet == null || synSetRelations.SelectedIndex == -1)
                return;

            synSets.Items.Clear();

            // get relations
            string relationStr = synSetRelations.SelectedItem.ToString();
            relationStr = relationStr.Split(':')[0].Trim();
            WordNetEngine.SynSetRelation relation = (WordNetEngine.SynSetRelation)Enum.Parse(typeof(WordNetEngine.SynSetRelation), relationStr);

            // add related synset
            foreach (SynSet relatedSynset in selectedSynSet.GetRelatedSynSets(relation, false))
                synSets.Items.Add(relatedSynset);

            // selected synset
            if (synSets.Items.Count > 0)
                synSets.SelectedIndex = 0;
        }

        private void synSetRelations_DoubleClick(object sender, EventArgs e)
        {
            getRelatedSynSets_Click(sender, e);
        }

        private void word_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
                getSynSets_Click(sender, e);
        }

        private void test_Click(object sender, EventArgs e)
        {
            Thread t = new Thread(new ThreadStart(delegate()
                {
                    Invoke(new MethodInvoker(delegate() { Enabled = false; }));

                    // test all words
                    Dictionary<WordNetEngine.POS, Set<string>> words = _wordNetEngine.AllWords;
                    foreach (WordNetEngine.POS pos in words.Keys)
                        foreach (string word in words[pos])
                        {
                            // get synsets
                            Set<SynSet> synsets = _wordNetEngine.GetSynSets(word, pos);
                            if (synsets.Count == 0)
                                if (MessageBox.Show("Failed to get synset for " + pos + ":  " + word + ". Quit?", "Quit?", MessageBoxButtons.YesNo) == DialogResult.Yes)
                                    return;

                            // make sure there's a most common synset
                            if (_wordNetEngine.GetMostCommonSynSet(word, pos) == null)
                                throw new NullReferenceException("Failed to find most common synset");

                            // check each synset
                            foreach (SynSet synset in synsets)
                            {
                                // check lexically related words
                                synset.GetLexicallyRelatedWords();

                                // check relations
                                foreach (WordNetEngine.SynSetRelation relation in synset.SynSetRelations)
                                    synset.GetRelatedSynSets(relation, false);

                                // check lex file name
                                if (synset.LexicographerFileName == WordNetEngine.LexicographerFileName.None)
                                    throw new Exception("Invalid lex file name");
                            }
                        }

                    MessageBox.Show("Test completed. Everything looks okay.");

                    Invoke(new MethodInvoker(delegate() { Enabled = true; }));
                }));

            t.Start();
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void word_TextChanged(object sender, EventArgs e)
        {
            synsetID.TextChanged -= new EventHandler(synsetID_TextChanged);
            synsetID.Text = "";
            synsetID.TextChanged += new EventHandler(synsetID_TextChanged);
        }

        private void synsetID_TextChanged(object sender, EventArgs e)
        {
            word.TextChanged -= new EventHandler(word_TextChanged);
            word.Text = "";
            word.TextChanged += new EventHandler(word_TextChanged);
        }

        private void FindLinks_Click(object sender, EventArgs e)
        {
            LinkBox.Items.Clear();
            // retrive synsets
            Set<SynSet> synStartSet = null;
            try { synStartSet = _wordNetEngine.GetSynSets(StartWord.Text, (WordNetEngine.POS)pos.SelectedItem); }
            catch (Exception)
            {
                MessageBox.Show("Invalid Start SynSet ID");
                return;
            }
            Set<SynSet> synDestSet = null;
            try { synDestSet = _wordNetEngine.GetSynSets(DestWord.Text, (WordNetEngine.POS)pos.SelectedItem); }
            catch (Exception)
            {
                MessageBox.Show("Invalid Dest SynSet ID");
                return;
            }

            if (synStartSet.Count > 0)
            {
                WordNetEngine.SynSetRelation[] vlist = new WordNetEngine.SynSetRelation[2];
                vlist[0] = WordNetEngine.SynSetRelation.Hyponym;
                vlist[1] = WordNetEngine.SynSetRelation.InstanceHyponym;
                foreach (SynSet synSrcSet in synStartSet)
                {
                    foreach (SynSet synDstSet in synDestSet)
                    {
                        //synSets.Items.Add(synSet);
                        List<SynSet> linkageList = null;

                        linkageList = synSrcSet.GetShortestPathTo(synDstSet, vlist);
                        if ((linkageList != null) && (linkageList.Count > 0))
                        {
                            foreach (SynSet s in linkageList)
                            {
                                StringBuilder desc = new StringBuilder();

                                desc.Append("{");
                                bool prependComma = false;
                                foreach (string word in s.Words)
                                {
                                    desc.Append((prependComma ? ", " : "") + word);
                                    prependComma = true;
                                }

                                desc.Append("}");

                                LinkBox.Items.Add(desc.ToString());
                            }
                            LinkBox.Text = "true";
                            return;
                        }
                    }
                }
                LinkBox.Text = "false";
            }
            else
            {
                LinkBox.Text = "false";
                MessageBox.Show("No synsets found");
            }

        }

        private void findLCS_Click(object sender, EventArgs e)
        {
            int found = 0;
            LinkBox.Items.Clear();
            // retrive synsets
            Set<SynSet> synStartSet = null;
            try { synStartSet = _wordNetEngine.GetSynSets(StartWord.Text, (WordNetEngine.POS)pos.SelectedItem); }
            catch (Exception)
            {
                MessageBox.Show("Invalid Start SynSet ID");
                return;
            }
            Set<SynSet> synDestSet = null;
            try { synDestSet = _wordNetEngine.GetSynSets(DestWord.Text, (WordNetEngine.POS)pos.SelectedItem); }
            catch (Exception)
            {
                MessageBox.Show("Invalid Dest SynSet ID");
                return;
            }
            if (synStartSet.Count > 0)
            {
                WordNetEngine.SynSetRelation[] vlist = new WordNetEngine.SynSetRelation[1];
                 vlist[0] = WordNetEngine.SynSetRelation.Hypernym;
                //vlist[1] = WordNetEngine.SynSetRelation.InstanceHypernym;
                //vlist[2] = WordNetEngine.SynSetRelation.Hyponym;
                //vlist[3] = WordNetEngine.SynSetRelation.InstanceHyponym;
                foreach (SynSet synSrcSet in synStartSet)
                {
                    foreach (SynSet synDstSet in synDestSet)
                    {
                        //synSets.Items.Add(synSet);
                        List<SynSet> linkageList = null;

                        linkageList = synSrcSet.GetShortestPathTo(synDstSet, vlist);

                        SynSet s = synSrcSet.GetClosestMutuallyReachableSynset(synDstSet, vlist);
                        if (s != null)
                        {
                            StringBuilder desc = new StringBuilder();

                            desc.Append("{");
                            bool prependComma = false;
                            foreach (string word in s.Words)
                            {
                                desc.Append((prependComma ? ", " : "") + word);
                                prependComma = true;
                            }

                            desc.Append("}");

                            LinkBox.Items.Add(desc.ToString());
                            LinkBox.Text = desc.ToString();
                            found++;
                            //return;
                        }
                    }
                }
                if (found==0) LinkBox.Text = "false";
            }
            else
            {
                LinkBox.Text = "false";
               // MessageBox.Show("No synsets found");
            }

        }
    }
}
