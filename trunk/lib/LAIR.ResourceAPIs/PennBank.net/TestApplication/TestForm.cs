using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Threading;
using System.Text.RegularExpressions;
using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.ResourceAPIs.PennBank.PropBank;

namespace TestApplication
{
    /// <summary>
    /// Test application form
    /// </summary>
    public partial class TestForm : Form
    {
        #region static members
        private delegate void AddItemDelegate(string s, bool filter);
        #endregion

        private delegate void SetEnabledDelegate(bool enabled);

        private TreeBankEngine _treeBankEngine;
        private PropBankEngine _propBankEngine;
        private int _itemsInOutputList;
        private Regex _filterRE;

        /// <summary>
        /// Constructor
        /// </summary>
        public TestForm()
        {
            InitializeComponent();

            foreach (PropBankNodeLabel.NodeType t in Enum.GetValues(typeof(PropBankNodeLabel.NodeType)))
                nodeTypeCombo.Items.Add(t);

            foreach (PropBankNodeLabel.NodeFeature f in Enum.GetValues(typeof(PropBankNodeLabel.NodeFeature)))
                nodeFeatureCombo.Items.Add(f);
        }

        private void loadTbBtn_Click(object sender, EventArgs e)
        {
            string root = Directory.GetDirectoryRoot(".");
            _treeBankEngine = new TreeBankEngine(root + @"NLP\Resources\PennTreeBank_3\PARSED\MRG\WSJ",
                                                 root + @"NLP\Resources\Indexes\treebank_index");

            // populate MRG file combo box
            foreach (string mrgPath in _treeBankEngine.IndexedMrgFiles)
                mrgFileCombo.Items.Add(Path.GetFileName(mrgPath) + " (" + _treeBankEngine.GetSentenceNumbers(mrgPath).Count + " sentences)");

            treeBankGroupBox.Enabled = true;
            loadTbBtn.Enabled = false;
        }

        private void loadPbBtn_Click(object sender, EventArgs e)
        {
            string root = Directory.GetDirectoryRoot(".");
            _propBankEngine = new PropBankEngine(root + @"NLP\Resources\PennTreeBank_3\PARSED\MRG\WSJ",
                                                 root + @"NLP\Resources\PropBank\prop.txt",
                                                 root + @"NLP\Resources\PropBank\frames",
                                                 root + @"NLP\Resources\Indexes\propbank_index");
            // populate verb box
            verbCombo.Items.AddRange(new List<string>(_propBankEngine.AllVerbs).ToArray());

            propBankGroupBox.Enabled = true;
            loadPbBtn.Enabled = false;
        }

        #region treebank demo
        private void getTbSentences_Click(object sender, EventArgs e)
        {
            ClearList();
            
            string mrgFile = null;
            try
            {
                mrgFile = mrgFileCombo.Text;
                mrgFile = mrgFile.Substring(0, mrgFile.IndexOf(' '));
                mrgFile = _treeBankEngine.GetFullMrgPath(mrgFile);
            }
            catch (Exception)
            {
                MessageBox.Show("Invalid MRG file");
                return;
            }

            try
            {
                foreach (int sentNum in _treeBankEngine.GetSentenceNumbers(mrgFile))
                    AppendOutput(_treeBankEngine.GetParseTree(mrgFile, sentNum));
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString());
            }
        }

        private void testTBIndexBtn_Click(object sender, EventArgs e)
        {
            ClearList();

            Thread t = new Thread(new ThreadStart(delegate()
            {
                SetEnabled(false);
                foreach (string mrgFile in _treeBankEngine.IndexedMrgFiles)
                {
                    int sentsInFile = 0;
                    foreach (int sentNum in _treeBankEngine.GetSentenceNumbers(mrgFile))
                    {
                        TreeBankNode root = _treeBankEngine.GetParseTree(mrgFile, sentNum);
                        root.Test();

                        ++sentsInFile;

                        // if we're searching, display sentences that pass the filter
                        if (search.Checked)
                            AppendOutput(root);
                    }

                    // if we're not searching, just display the number of sentences in the article
                    if (!search.Checked)
                        AppendOutput(mrgFile + ":  " + sentsInFile + " sentences", true);
                }
                SetEnabled(true);
            }));
            t.Start();
        }

        private void displayParseTrees_CheckedChanged(object sender, EventArgs e)
        {
            displayPhraseHeads.Enabled = displayParseTrees.Checked;
        }

        private void leavesOnly_CheckedChanged(object sender, EventArgs e)
        {
            categoryTags.Enabled = leavesOnly.Checked;
        }
        #endregion

        #region propbank demo
        private void getVerbAttBtn_Click(object sender, EventArgs e)
        {
            ClearList();

            if (!_propBankEngine.Contains(verbCombo.Text))
            {
                MessageBox.Show("PropBank does not contain \"" + verbCombo.Text + "\"");
                return;
            }

            // get attestations for each verb
            foreach (VerbInfo vi in _propBankEngine.GetVerbInfo(verbCombo.Text))
                AppendOutput(_propBankEngine.GetPropBankTree(vi));
        }

        private void testAllVerbsBtn_Click(object sender, EventArgs e)
        {
            ClearList();

            // get all verb information
            Thread t = new Thread(new ThreadStart(delegate()
            {
                SetEnabled(false);

                // get all verbs
                foreach (string verb in _propBankEngine.AllVerbs)
                {
                    // get verb info for each verb
                    int entries = 0;
                    foreach (VerbInfo vi in _propBankEngine.GetVerbInfo(verb))
                    {
                        PropBankNode propBankRoot = _propBankEngine.GetPropBankTree(vi);
                        propBankRoot.Test();

                        ++entries;
                    }

                    AppendOutput("Found " + entries + " attestations for all senses of \"" + verb + "\"", true);
                }
                SetEnabled(true);
            }));
            t.Start();
        }

        private void getNodesByTypeBtn_Click(object sender, EventArgs e)
        {
            ClearList();

            if (!_propBankEngine.Contains(verbCombo.Text))
            {
                MessageBox.Show("PropBank does not contain \"" + verbCombo.Text + "\"");
                return;
            }

            // get all nodes with given type
            foreach (VerbInfo vi in _propBankEngine.GetVerbInfo(verbCombo.Text))
                foreach (PropBankNode node in _propBankEngine.GetPropBankTree(vi).GetDescendants((PropBankNodeLabel.NodeType)nodeTypeCombo.SelectedItem))
                    if (node.SurfaceText != "")
                        AppendOutput(node);
        }

        private void getNodesByFeatureBtn_Click(object sender, EventArgs e)
        {
            ClearList();

            if (!_propBankEngine.Contains(verbCombo.Text))
            {
                MessageBox.Show("PropBank does not contain \"" + verbCombo.Text + "\"");
                return;
            }

            // get all nodes with given feature
            foreach (VerbInfo vi in _propBankEngine.GetVerbInfo(verbCombo.Text))
                foreach (PropBankNode node in _propBankEngine.GetPropBankTree(vi).GetDescendants((PropBankNodeLabel.NodeFeature)nodeFeatureCombo.SelectedItem))
                    if (node.SurfaceText != "")
                        AppendOutput(node);
        }

        private void getFrameBtn_Click(object sender, EventArgs e)
        {
            string verb = verbCombo.Text;
            if (!_propBankEngine.Contains(verb))
            {
                MessageBox.Show("PropBank does not contain \"" + verb + "\"");
                return;
            }

            List<VerbInfo> vi = _propBankEngine.GetVerbInfo(verb);
            MessageBox.Show(vi[0].VerbFrame.ToString());
        }
        #endregion

        #region misc functions
        /// <summary>
        /// Appends a string to the output box
        /// </summary>
        /// <param name="s"></param>
        private void AppendOutput(string s, bool filterText)
        {
            if (filterText && _filterRE != null && !_filterRE.Match(s).Success)
                return;

            if (InvokeRequired)
            {
                Invoke(new AddItemDelegate(AppendOutput), s, filterText);
                return;
            }

            output.AppendText(s + Environment.NewLine);
            numItemsLbl.Text = ++_itemsInOutputList + " items";
        }

        /// <summary>
        /// Appends a TreeBank node to the output list
        /// </summary>
        /// <param name="n"></param>
        private void AppendOutput(TreeBankNode n)
        {
            string text = null;
            if (n is PropBankNode)
                text = n.FullLocation + ":  " + (n as PropBankNode).GetBracketedText(PropBankNode.BracketedOutputOptions.IgnoreBracketProbabilities);
            else
            {
                if (displayParseTrees.Checked)
                    text = n.FullLocation + Environment.NewLine + n.GetBracketedText(useCategoryMnemonic.Checked, displayPhraseHeads.Checked);
                else
                    text = n.FullLocation + ":  " + n.GetSurfaceText(categoryTags.Checked, useCategoryMnemonic.Checked);
            }

            if (_filterRE != null && !_filterRE.Match(text).Success)
                return;

            AppendOutput(text, false);
        }

        /// <summary>
        /// Clears item list
        /// </summary>
        private void ClearList()
        {
            numItemsLbl.Text = "0 items";
            output.Text = "";
            _itemsInOutputList = 0;
        }

        /// <summary>
        /// Sets form enabled/disabled
        /// </summary>
        /// <param name="enabled">Whether or not form should be enabled</param>
        private void SetEnabled(bool enabled)
        {
            if (InvokeRequired)
                Invoke(new SetEnabledDelegate(SetEnabled), enabled);
            else
                Enabled = enabled;
        }

        private void filter_TextChanged(object sender, EventArgs e)
        {
            try
            {
                string filterText = filter.Text;
                if (filterText == "")
                    _filterRE = null;
                else
                    _filterRE = new Regex(filter.Text);

                validFilter.Text = "Valid";
            }
            catch (Exception)
            {
                validFilter.Text = "Not valid";
                _filterRE = null;
            }
        }
        #endregion
    }
}