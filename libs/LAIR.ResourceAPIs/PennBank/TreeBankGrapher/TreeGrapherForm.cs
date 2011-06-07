using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.GraphViz;
using System.IO;
using TreeBankGrapher.Properties;

namespace TreeBankGrapher
{
    /// <summary>
    /// Application for graphing trees
    /// </summary>
    public partial class TreeBankGrapher : Form
    {
        private TreeBankEngine _treeBankEngine;
        private Dot _dot;

        /// <summary>
        /// Constructor
        /// </summary>
        public TreeBankGrapher()
        {
            InitializeComponent();

            string root = Directory.GetDirectoryRoot(".");

            foreach (string outputFormatStr in Enum.GetNames(typeof(Dot.OutputFormat)))
                outputFormat.Items.Add((Dot.OutputFormat)Enum.Parse(typeof(Dot.OutputFormat), outputFormatStr));

            outputFormat.SelectedIndex = 0;
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void resetMachineSpecificPathsMenuItem_Click(object sender, EventArgs e)
        {
            Settings.Default.DotPath = "";
            Settings.Default.TreeBankPath = "";
            Settings.Default.TreeBankIndexPath = "";
            Settings.Default.Save();

            _dot = null;
            _treeBankEngine = null;
            mrgFile.Enabled = sentence.Enabled = viewTree.Enabled = false;
            loadTreeBank.Enabled = true;
        }

        #region treebank viewing
        private void loadTreeBank_Click(object sender, EventArgs e)
        {
            try
            {
                if (Directory.Exists(Settings.Default.TreeBankPath) &&
                    Directory.Exists(Settings.Default.TreeBankIndexPath))
                {
                    _treeBankEngine = new TreeBankEngine(Settings.Default.TreeBankPath,
                                                         Settings.Default.TreeBankIndexPath);
                }
                else
                {
                    folderBrowserDialog.Description = "Select TreeBank MRG directory path";
                    if (folderBrowserDialog.ShowDialog() == DialogResult.OK &&
                        Directory.Exists(folderBrowserDialog.SelectedPath))
                    {
                        string treeBankPath = folderBrowserDialog.SelectedPath;

                        folderBrowserDialog.Description = "Select TreeBank index directory";
                        if (folderBrowserDialog.ShowDialog() == DialogResult.OK)
                        {
                            string treeBankIndexPath = folderBrowserDialog.SelectedPath;
                            _treeBankEngine = new TreeBankEngine(treeBankPath, treeBankIndexPath);

                            Settings.Default.TreeBankPath = treeBankPath;
                            Settings.Default.TreeBankIndexPath = treeBankIndexPath;
                            Settings.Default.Save();
                        }
                    }
                }

                mrgFile.Items.Clear();
                foreach (string mrgFilePath in _treeBankEngine.IndexedMrgFiles)
                    mrgFile.Items.Add(Path.GetFileNameWithoutExtension(mrgFilePath));

                loadTreeBank.Enabled = false;
                mrgFile.Enabled = sentence.Enabled = viewTree.Enabled = true;
            }
            catch (Exception ex)
            {
                MessageBox.Show("Failed to load TreeBank index:  " + ex);
            }
        }

        private void mrgFile_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (mrgFile.SelectedItem != null)
            {
                sentence.Items.Clear();
                string mrgFilePath = _treeBankEngine.GetFullMrgPath(mrgFile.SelectedItem.ToString() + ".mrg");
                foreach (int sentNum in _treeBankEngine.GetSentenceNumbers(mrgFilePath))
                    sentence.Items.Add(sentNum);
            }
        }

        private void viewTree_Click(object sender, EventArgs e)
        {
            try
            {
                string mrgFilePath = _treeBankEngine.GetFullMrgPath(mrgFile.Text + ".mrg");
                treeDef.Text = _treeBankEngine.GetParseTree(mrgFilePath, int.Parse(sentence.Text)).GetBracketedText(true, false);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error:  " + ex);
            }
        }
        #endregion

        #region graph creation
        private void createGraphFromDef_Click(object sender, EventArgs e)
        {
            try
            {
                if (treeDef.Text != "")
                    CreateGraph(TreeBankEngine.ExtractNode(treeDef.Text, true));
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error:  " + ex);
            }
        }

        /// <summary>
        /// Creates a graph from a node, prompting use to save file
        /// </summary>
        /// <param name="n"></param>
        private void CreateGraph(TreeBankNode n)
        {
            saveFileDialog.Title = "Select output path for graph";
            if (saveFileDialog.ShowDialog() == DialogResult.OK)
            {
                if (_dot == null)
                {
                    try
                    {
                        _dot = new Dot(Settings.Default.DotPath);
                    }
                    catch (Exception)
                    {
                        openFileDialog.Title = "Select path to Dot executable";
                        if (openFileDialog.ShowDialog() != DialogResult.OK || !File.Exists(openFileDialog.FileName))
                            throw new Exception("Dot path not specified. Cannot create graph.");

                        _dot = new Dot(openFileDialog.FileName);

                        Settings.Default.DotPath = _dot.DotPath;
                        Settings.Default.Save();
                    }
                }

                if (outputFormat.SelectedItem == null)
                {
                    MessageBox.Show("No output format selected. Using PNG.");
                    outputFormat.SelectedItem = Dot.OutputFormat.PNG;
                }

                _dot.CreateGraph(n, (Dot.OutputFormat)outputFormat.SelectedItem, saveFileDialog.FileName);
            }
        }
        #endregion
    }
}
