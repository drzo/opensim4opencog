using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Threading;
using System.IO;
using System.Linq;

using LAIR.ResourceAPIs.FrameNet;
using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace TestApplication
{
    /// <summary>
    /// Form for the test application
    /// </summary>
    public partial class TestForm : Form
    {
        #region static members
        /// <summary>
        /// Delegate to add items from a different thread
        /// </summary>
        /// <param name="item"></param>
        private delegate void AddItemDelegate(string item);
        #endregion

        private FrameNetEngine _frameNetEngine;

        /// <summary>
        /// Gets selected frame relations, using all relations if none are selected.
        /// </summary>
        private IEnumerable<Frame.FrameRelation> SelectedRelations
        {
            get
            {
                if (relations.SelectedItems.Count == 0)
                {
                    MessageBox.Show("No relation selected, using all relations.");
                    for (int i = 0; i < relations.Items.Count; ++i)
                        relations.SetSelected(i, true);
                }

                return relations.SelectedItems.Cast<Frame.FrameRelation>();
            }
        }

        /// <summary>
        /// Gets selected frame relation direction
        /// </summary>
        private Frame.FrameRelationDirection SelectedRelationDirection
        {
            get { return sub.Checked ? Frame.FrameRelationDirection.Sub : (super.Checked ? Frame.FrameRelationDirection.Super : Frame.FrameRelationDirection.Both); }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public TestForm()
        {
            InitializeComponent();

            // init framenet
            string root = Directory.GetDirectoryRoot(".");
            _frameNetEngine = new FrameNetEngine(root + @"NLP\Resources\FrameNet1.3\basicXML\frXML",
                                                 root + @"NLP\Resources\FrameNet1.3\basicXML\luXML");

            // add frames
            foreach (Frame frame in _frameNetEngine.Frames)
            {
                frame1.Items.Add(frame);
                frame2.Items.Add(frame);
            }

            // select first frame in each combo
            frame1.SelectedIndex = frame2.SelectedIndex = 0;

            // add frame relations
            relations.Items.AddRange(Enum.GetValues(typeof(Frame.FrameRelation)).Cast<object>().ToArray());
        }

        private void getFramesForLexicalUnit_Click(object sender, EventArgs e)
        {
            ResetOutput();

            Set<Frame> frames = new Set<Frame>();
            foreach (Frame frame in _frameNetEngine.Frames)
                foreach (LexicalUnit lu in frame.LexicalUnits)
                    if (lu.ToString() == lexicalUnit.Text && !frames.Contains(frame))
                    {
                        AddItem(frame.Name);
                        frames.Add(frame);
                    }
        }

        private void getAttestationsForLexicalUnit_Click(object sender, EventArgs e)
        {
            ResetOutput();

            if (!_frameNetEngine.ContainsLexeme(lexicalUnit.Text))
            {
                MessageBox.Show("Invalid lexical unit");
                return;
            }

            string pos = lexicalUnitPOS.Text.Trim();
            if (pos == "")
                pos = null;

            int numAttestations = 0;
            foreach (Frame frame in _frameNetEngine.GetFramesForLexicalUnit(lexicalUnit.Text))
                foreach (Attestation a in _frameNetEngine.GetAttestationsForFrame(frame, pos))
                {
                    foreach (string line in a.ToString().Split(new char[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries))
                        AddItem(line);

                    AddItem("");

                    ++numAttestations;
                }

            numItemsLbl.Text = numAttestations + " attestations";
        }

        private void getLexicalUnitsForFrame1_Click(object sender, EventArgs e)
        {
            ResetOutput();

            if (frame1.SelectedItem == null)
            {
                MessageBox.Show("Please select frame 1");
                return;
            }

            // show LUs for frame
            Frame frame = frame1.SelectedItem as Frame;
            foreach (LexicalUnit lexicalUnit in frame.LexicalUnits)
                output.AppendText(lexicalUnit.ToString() + Environment.NewLine);

            numItemsLbl.Text = frame.LexicalUnits.Count.ToString() + " LUs";
        }

        private void getAttestationsForFrame1_Click(object sender, EventArgs e)
        {
            ResetOutput();

            if (frame1.SelectedItem == null)
            {
                MessageBox.Show("Please select frame 1");
                return;
            }

            // look up frame
            Frame frame = frame1.SelectedItem as Frame;

            // show all attestations for frame
            List<Attestation> attestations = _frameNetEngine.GetAttestationsForFrame(frame);
            foreach (Attestation attestation in attestations)
            {
                foreach (string line in attestation.ToString().Split(new char[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries))
                    AddItem(line);

                AddItem("");
            }

            numItemsLbl.Text = attestations.Count.ToString() + " attestations";
        }

        private void getFramesRelatedToFrame1_Click(object sender, EventArgs e)
        {
            ResetOutput();

            if (frame1.SelectedItem == null)
            {
                MessageBox.Show("Please select frame 1");
                return;
            }

            Frame frame = frame1.SelectedItem as Frame;

            // show related frames
            FrameSet relatedFrames = new FrameSet(false);
            foreach (Frame.FrameRelation relation in SelectedRelations)
                foreach (Frame relatedFrame in frame.GetRelatedFrames(relation, SelectedRelationDirection, recursive.Checked))
                    if (!relatedFrames.Contains(relatedFrame))
                    {
                        output.AppendText(relatedFrame.ToString() + Environment.NewLine);
                        relatedFrames.Add(relatedFrame);
                    }

            numItemsLbl.Text = relatedFrames.Count.ToString() + " related frames";
        }

        private void getFrame1Frame2FeMapping_Click(object sender, EventArgs e)
        {
            ResetOutput();

            if (frame1.SelectedItem == null)
            {
                MessageBox.Show("Please select frame 1");
                return;
            }

            if (frame2.SelectedItem == null)
            {
                MessageBox.Show("Please select frame 2");
                return;
            }

            // get frames
            Frame f1 = frame1.SelectedItem as Frame;
            Frame f2 = frame2.SelectedItem as Frame;

            // show FE relations
            int totalFEs = 0;
            foreach (Frame.FrameRelation relation in SelectedRelations)
                if (f1.GetRelatedFrames(relation, SelectedRelationDirection, recursive.Checked).Contains(f2))
                {
                    output.AppendText("FE mapping via " + relation + ":" + Environment.NewLine);
                    foreach (FrameElement fe1 in f1.FrameElements)
                        foreach (FrameElement fe2 in fe1.GetRelatedFrameElements(relation, SelectedRelationDirection, recursive.Checked))
                            if (fe2.Frame == f2)
                            {
                                output.AppendText("  " + fe1 + " -> " + fe2 + Environment.NewLine);
                                ++totalFEs;
                            }
                }

            numItemsLbl.Text = totalFEs + " related FEs shown";
        }

        private void getFe1Fe2Path_Click(object sender, EventArgs e)
        {
            ResetOutput();

            if (fe1.SelectedItem == null)
            {
                MessageBox.Show("Please select FE 1");
                return;
            }

            if (fe2.SelectedItem == null)
            {
                MessageBox.Show("Please select FE 2");
                return;
            }

            FrameElement sourceFE = fe1.SelectedItem as FrameElement;
            FrameElement destFE = fe2.SelectedItem as FrameElement;
            Set<Frame.FrameRelation> searchRelations = new Set<Frame.FrameRelation>(SelectedRelations.ToArray());
            List<FrameElement> fePath;
            List<Frame.FrameRelation> relationPath;
            if (sourceFE.GetShortestPathTo(destFE, searchRelations, SelectedRelationDirection, int.MaxValue, out fePath, out relationPath))
            {
                StringBuilder path = new StringBuilder();
                for (int i = 0; i < fePath.Count; ++i)
                    path.Append(fePath[i] + (relationPath.Count > i ? " -- " + relationPath[i] + " --> " : ""));

                AddItem(path.ToString());
            }
            else
                MessageBox.Show("No path exists");
        }

        private void checkFrames_Click(object sender, EventArgs e)
        {
            ResetOutput();

            Thread t = new Thread(new ThreadStart(delegate()
            {
                Invoke(new MethodInvoker(delegate() { Enabled = false; }));

                int totalAttestations = 0;

                // test getting all frames and all attestations
                foreach (Frame frame in _frameNetEngine.Frames)
                {
                    List<Attestation> attestations = _frameNetEngine.GetAttestationsForFrame(frame);
                    AddItem("Retrieved " + attestations.Count + " attestations for frame \"" + frame + "\"");

                    totalAttestations += attestations.Count;

                    // check all relations
                    foreach (Frame.FrameRelation relation in Enum.GetValues(typeof(Frame.FrameRelation)))
                    {
                        frame.GetRelatedFrames(relation, Frame.FrameRelationDirection.Both, true);
                        foreach (FrameElement fe in frame.FrameElements)
                            fe.GetRelatedFrameElements(relation, Frame.FrameRelationDirection.Both, true);
                    }
                }

                Invoke(new MethodInvoker(delegate() { Enabled = true; }));

                MessageBox.Show("Check succeeded. " + totalAttestations + " attestations retrieved for " + _frameNetEngine.Frames.Count() + " frames.");
            }));

            t.Start();
        }

        /// <summary>
        /// Adds item to output
        /// </summary>
        /// <param name="s">Item to add</param>
        private void AddItem(string s)
        {
            // invoke in form's main thread if needed
            if (InvokeRequired)
            {
                Invoke(new AddItemDelegate(AddItem), s);
                return;
            }

            output.AppendText(s + Environment.NewLine);

            int currentCount = int.Parse(numItemsLbl.Text.Substring(0, numItemsLbl.Text.IndexOf(' ')));
            numItemsLbl.Text = (currentCount + 1) + " items";
        }

        /// <summary>
        /// Resets the output list
        /// </summary>
        private void ResetOutput()
        {
            output.Clear();
            numItemsLbl.Text = "0 items";
        }

        /// <summary>
        /// Updates FE list for frame 1
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void frame1_SelectedIndexChanged(object sender, EventArgs e)
        {
            fe1.Items.Clear();

            foreach (FrameElement fe in (frame1.SelectedItem as Frame).FrameElements)
                fe1.Items.Add(fe);
        }

        /// <summary>
        /// Updates FE list for frame 2
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void frame2_SelectedIndexChanged(object sender, EventArgs e)
        {
            fe2.Items.Clear();

            foreach (FrameElement fe in (frame2.SelectedItem as Frame).FrameElements)
                fe2.Items.Add(fe);
        }
    }
}