using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Threading;
using LAIR.ResourceAPIs.NomBank;
using System.Text.RegularExpressions;
using LAIR.Collections.Generic;

namespace TestApplication
{
    /// <summary>
    /// Test form for the application
    /// </summary>
    public partial class TestForm : Form
    {
        #region static members
        private delegate void AddItemDelegate(string s);
        private delegate void SetEnabledDelegate(bool enabled);
        #endregion

        /// <summary>
        /// Our NomBank engine
        /// </summary>
        private NomBankEngine _nomBankEngine;

        private Regex _filterRE;
        private int _itemsInOutputList;

        /// <summary>
        /// Constructor
        /// </summary>
        public TestForm()
        {
            InitializeComponent();

            // initialize engine
            string root = Directory.GetDirectoryRoot(".");

            _nomBankEngine = new NomBankEngine(root + @"NLP\Resources\PennTreeBank_3\parsed\mrg\wsj",
                                               root + @"NLP\Resources\nombank.1.0\nombank.1.0",
                                               root + @"NLP\Resources\nombank.1.0\frames",
                                               root + @"NLP\Resources\nombank.1.0\nombank-morph.dict.1.0",
                                               root + @"NLP\Resources\nombank.1.0\nomlex-plus.1.0",
                                               root + @"NLP\Resources\Indexes\nombank_index");
            // populate noun box
            foreach (string noun in _nomBankEngine.AllNouns)
                nounCombo.Items.Add(noun);

            // populate class box
            foreach (string nlClass in _nomBankEngine.NomLexEngine.Classes)
                classCombo.Items.Add(nlClass);

            // populate NomLex noun box
            List<string> nomLexNouns = new List<string>(_nomBankEngine.NomLexEngine.Nouns);
            nomLexNouns.Sort();
            foreach (string nomLexNoun in nomLexNouns)
                nomLexNounCombo.Items.Add(nomLexNoun);

            foreach (NomBankNodeLabel.NodeType t in Enum.GetValues(typeof(NomBankNodeLabel.NodeType)))
                nodeTypeCombo.Items.Add(t);

            foreach (NomBankNodeLabel.NodeFeature f in Enum.GetValues(typeof(NomBankNodeLabel.NodeFeature)))
                nodeFeatureCombo.Items.Add(f);
        }

        /// <summary>
        /// Adds a string to the list (used for cross-thread operation)
        /// </summary>
        /// <param name="s"></param>
        private void AddStringToList(string s)
        {
            if (_filterRE != null && !_filterRE.Match(s).Success)
                return;

            if (InvokeRequired)
            {
                Invoke(new AddItemDelegate(AddStringToList), s);
                return;
            }

            output.AppendText(s + Environment.NewLine);
            numItemsLbl.Text = ++_itemsInOutputList + " items";
        }

        private void getNounAttBtn_Click(object sender, EventArgs e)
        {
            ClearItemList();

            if (!_nomBankEngine.Contains(nounCombo.Text))
            {
                MessageBox.Show("NomBank does not contain \"" + nounCombo.Text + "\"");
                return;
            }

            // get attestations for each noun
            List<NomBankNode.BracketedOutputOptions> options = new List<NomBankNode.BracketedOutputOptions>();
            options.Add(NomBankNode.BracketedOutputOptions.IgnoreBracketProbabilities);
            if (includeNounSense.Checked)
                options.Add(NomBankNode.BracketedOutputOptions.IncludePredicateFrame);

            NomBankNode.BracketedOutputOptions[] optionsArray = options.ToArray();

            foreach (NounInfo ni in _nomBankEngine.GetNounInfo(nounCombo.Text))
            {
                NomBankNode tree = _nomBankEngine.GetNomBankTree(ni);
                if (tree == null)
                    continue;

                string text = tree.FullLocation + ":  " + tree.GetBracketedText(optionsArray);
                                                    
                AddStringToList(text);
            }
        }

        private void getFrameBtn_Click(object sender, EventArgs e)
        {
            string noun = nounCombo.Text;
            if (!_nomBankEngine.Contains(noun))
            {
                MessageBox.Show("Could not find exact match for \"" + noun + "\"");
                return;
            }

            List<NounInfo> ni = _nomBankEngine.GetNounInfo(noun);
            MessageBox.Show(ni[0].Frame.ToString());
        }

        private void getNodesByTypeBtn_Click(object sender, EventArgs e)
        {
            ClearItemList();

            if (!_nomBankEngine.Contains(nounCombo.Text))
            {
                MessageBox.Show("NomBank does not contain \"" + nounCombo.Text + "\"");
                return;
            }

            // get attestations for each noun
            List<NomBankNode.BracketedOutputOptions> options = new List<NomBankNode.BracketedOutputOptions>();
            options.Add(NomBankNode.BracketedOutputOptions.IgnoreBracketProbabilities);
            if (includeNounSense.Checked)
                options.Add(NomBankNode.BracketedOutputOptions.IncludePredicateFrame);

            NomBankNode.BracketedOutputOptions[] optionsArray = options.ToArray();

            // get all nodes with given type
            foreach (NounInfo ni in _nomBankEngine.GetNounInfo(nounCombo.Text))
            {
                NomBankNode tree = _nomBankEngine.GetNomBankTree(ni);
                if (tree == null)
                    continue;

                foreach (NomBankNode n in tree.GetDescendants((NomBankNodeLabel.NodeType)nodeTypeCombo.SelectedItem))
                    if (n.SurfaceText != "")
                        AddStringToList(n.GetBracketedText(optionsArray));
            }
        }

        private void getNodesByFeatureBtn_Click(object sender, EventArgs e)
        {
            ClearItemList();

            if (!_nomBankEngine.Contains(nounCombo.Text))
            {
                MessageBox.Show("NomBank does not contain \"" + nounCombo.Text + "\"");
                return;
            }

            // get attestations for each noun
            List<NomBankNode.BracketedOutputOptions> options = new List<NomBankNode.BracketedOutputOptions>();
            options.Add(NomBankNode.BracketedOutputOptions.IgnoreBracketProbabilities);
            if (includeNounSense.Checked)
                options.Add(NomBankNode.BracketedOutputOptions.IncludePredicateFrame);

            NomBankNode.BracketedOutputOptions[] optionsArray = options.ToArray();

            // get all nodes with given feature
            foreach (NounInfo ni in _nomBankEngine.GetNounInfo(nounCombo.Text))
            {
                NomBankNode tree = _nomBankEngine.GetNomBankTree(ni);
                if (tree == null)
                    continue;

                foreach (NomBankNode n in tree.GetDescendants((NomBankNodeLabel.NodeFeature)nodeFeatureCombo.SelectedItem))
                    if (n.SurfaceText != "")
                        AddStringToList(n.GetBracketedText(optionsArray));
            }
        }

        private void testAllNounsBtn_Click(object sender, EventArgs e)
        {
            ClearItemList();

            // get all noun information
            Thread t = new Thread(new ThreadStart(delegate()
            {
                SetNomBankGroupEnabled(false);

                // error logging
                string errorFile = "error_log.txt";
                if (File.Exists(errorFile))
                    File.Delete(errorFile);

                int numErrors = 0;

                // get all nouns
                foreach (string noun in _nomBankEngine.AllNouns)
                {
                    // get noun info for each noun
                    int entries = 0;
                    foreach (NounInfo ni in _nomBankEngine.GetNounInfo(noun))
                    {
                        try
                        {
                            // test tree
                            NomBankNode predTree = _nomBankEngine.GetNomBankTree(ni);
                            predTree.Test(_nomBankEngine);
                        }
                        catch (Exception ex)
                        {
                            // write error information
                            StreamWriter errorLog = new StreamWriter(errorFile, true);
                            errorLog.WriteLine("Error at " + ni.File + ", " + ni.SentenceNumber + ", " + ni.LeafNumber + ". Label information:  " + ni.LabeledNodeLocations + ". Exception:  " + ex + "\n");
                            errorLog.Close();
                            ++numErrors;
                        }

                        ++entries;
                    }

                    AddStringToList("Found " + entries + " attestations for all senses of \"" + noun + "\"");
                }

                if (numErrors > 0)
                    MessageBox.Show(numErrors + " errors were encountered. Check error log at \"" + errorFile + "\" for details");
                else
                    MessageBox.Show("No errors were found.");

                SetNomBankGroupEnabled(true);
            }));
            t.Start();
        }

        /// <summary>
        /// Sets enabled property on the NomBank group
        /// </summary>
        /// <param name="enabled">Whether or not the group should be enabled</param>
        private void SetNomBankGroupEnabled(bool enabled)
        {
            if (InvokeRequired)
                Invoke(new SetEnabledDelegate(SetNomBankGroupEnabled), enabled);
            else
                nomBankGroup.Enabled = enabled;
        }

        private void getNounsInClassBtn_Click(object sender, EventArgs e)
        {
            ClearItemList();

            Set<string> nomsInClass = null;
            try
            {
                nomsInClass = _nomBankEngine.NomLexEngine.GetNominalizations(classCombo.Text, allowAmbiguousNomsChk.Checked);
            }
            catch(Exception)
            {
                MessageBox.Show("Invalid NomLex class");
            }

            foreach (string noun in nomsInClass)
                AddStringToList(noun);
        }

        /// <summary>
        /// Clear item list
        /// </summary>
        private void ClearItemList()
        {
            numItemsLbl.Text = "0 items";
            output.Text = "";
            _itemsInOutputList = 0;
        }

        private void getClassesForNounBtn_Click(object sender, EventArgs e)
        {
            ClearItemList();

            Set<string> classes = null;
            try
            {
                classes = _nomBankEngine.NomLexEngine.GetClasses(nomLexNounCombo.Text);
            }
            catch (Exception)
            {
                MessageBox.Show("Invalid NomLex noun");
                return;
            }

            foreach (string nlClass in classes)
                AddStringToList(nlClass);
        }

        private void filter_TextChanged(object sender, EventArgs e)
        {
            string filterText = filter.Text;
            if (filterText == "")
                _filterRE = null;
            else
                try { _filterRE = new Regex(filter.Text); }
                catch (Exception) { _filterRE = null; }
        }
    }
}