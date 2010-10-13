using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Text.RegularExpressions;

using SemLinkEditor.Properties;
using LAIR.ResourceAPIs.SemLink;
using LAIR.ResourceAPIs.VerbNet;
using LAIR.ResourceAPIs.PennBank.PropBank;
using LAIR.Collections.Generic;
using LAIR.ResourceAPIs.FrameNet;

namespace SemLinkEditor
{
    public partial class SemLinkEditorForm : Form
    {
        #region static members
        /// <summary>
        /// Issue types
        /// </summary>
        private enum IssueType
        {
            /// <summary>
            /// Issues concern the mapping between VerbNet and PropBank
            /// </summary>
            PropBank,

            /// <summary>
            /// Issues concern the mapping between VerbNet and FrameNet
            /// </summary>
            FrameNet
        }
        #endregion

        private SemLinkEngine _semLink;
        private VerbNetEngine _verbNet;
        private PropBankEngine _propBank;
        private FrameNetEngine _frameNet;
        private List<string> _issues;
        private int _currentIssue;
        private string _issueFilePath;
        private IssueType _issueType;

        /// <summary>
        /// Gets the selected PropBank verb
        /// </summary>
        private string SelectedPropBankVerb
        {
            get { return propBankVerb.Text; }
        }

        /// <summary>
        /// Gets the selected PropBank role set
        /// </summary>
        private RoleSet SelectedPropBankRoleSet
        {
            get { return propBankRoleSets.SelectedItem as RoleSet; }
        }

        /// <summary>
        /// Gets the selected PropBank role
        /// </summary>
        private Role SelectedPropBankRole
        {
            get { return propBankArguments.SelectedItem as Role; }
        }

        /// <summary>
        /// Gets the selected VerbNet class
        /// </summary>
        private VerbClass SelectedVerbNetClass
        {
            get { return verbNetClasses.SelectedItem as VerbClass; }
        }

        /// <summary>
        /// Gets the selected VerbNet thematic role
        /// </summary>
        private VerbNetEngine.ThematicRole SelectedVerbNetThematicRole
        {
            get { return (VerbNetEngine.ThematicRole)verbNetRoles.SelectedItem; }
        }

        /// <summary>
        /// Gets the currently selected mapping
        /// </summary>
        private string SelectedCurrentMapping
        {
            get { return currentMapping.SelectedItem as string; }
        }

        /// <summary>
        /// Gets the currently selected frame
        /// </summary>
        private LAIR.ResourceAPIs.FrameNet.Frame SelectedFrame
        {
            get { return frames.SelectedItem as LAIR.ResourceAPIs.FrameNet.Frame; }
        }

        /// <summary>
        /// Gets the currently selected frame element
        /// </summary>
        private FrameElement SelectedFrameElement
        {
            get { return frameElements.SelectedItem as FrameElement; }
        }

        /// <summary>
        /// Gets the currently selected lexical unit
        /// </summary>
        private string SelectedLexicalUnit
        {
            get { return lexicalUnits.SelectedItem as string; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public SemLinkEditorForm()
        {
            InitializeComponent();

            // hard-code propbank for now
            string root = Directory.GetDirectoryRoot(".");
            _propBank = new PropBankEngine(root + @"NLP\Resources\PennTreeBank_3\PARSED\MRG\WSJ",
                                           root + @"NLP\Resources\PropBank\prop.txt",
                                           root + @"NLP\Resources\PropBank\frames",
                                           root + @"NLP\Resources\Indexes\propbank_index");

            _frameNet = new FrameNetEngine(root + @"NLP\Resources\FrameNet1.5", FrameNetEngine.Version.FrameNet_1_5);

            RoleSet.IncludeArgumentsInToString = false;

            // add propbank verbs
            foreach (string verb in _propBank.AllVerbs)
                propBankVerb.Items.Add(verb);

            // add framenet frames
            foreach (LAIR.ResourceAPIs.FrameNet.Frame frame in _frameNet.Frames)
                frames.Items.Add(frame);

            folderBrowser.SelectedPath = root + @"NLP\Resources";

            semLinkDirectory.Text = Settings.Default.SemLinkDirectory;
            verbNetDirectory.Text = Settings.Default.VerbNetDirectory;
        }

        #region link management
        private void propBankVerb_SelectedIndexChanged(object sender, EventArgs e)
        {
            propBankRoleSets.Items.Clear();
            propBankArguments.Items.Clear();
            verbNetClasses.Items.Clear();
            verbNetRoles.Items.Clear();
            currentMapping.Items.Clear();

            if (propBankVerb.SelectedIndex == -1)
                return;

            // show propbank role sets
            foreach (RoleSet roleSet in _propBank.GetFrame(SelectedPropBankVerb).RoleSets)
                propBankRoleSets.Items.Add(roleSet);

            // show verbnet classes
            ShowVerbNetClassesFor(SelectedPropBankVerb);
        }

        private void propBankRoleSets_SelectedIndexChanged(object sender, EventArgs e)
        {
            propBankArguments.Items.Clear();

            if (propBankRoleSets.SelectedIndex == -1)
                return;

            // show propbank arguments
            foreach (Role role in SelectedPropBankRoleSet)
                propBankArguments.Items.Add(role);
        }

        private void propBankArguments_SelectedIndexChanged(object sender, EventArgs e)
        {
            // show the verbnet linking from semlink
            currentMapping.Items.Clear();
            foreach (string verbNetRole in _semLink.GetVerbNetRolesForPropBank(SelectedPropBankVerb, SelectedPropBankRoleSet.ID, SelectedPropBankRole.Number))
                currentMapping.Items.Add("0." + verbNetRole);
        }

        private void verbNetClasses_SelectedIndexChanged(object sender, EventArgs e)
        {
            verbNetRoles.Items.Clear();

            if (verbNetClasses.SelectedIndex == -1)
                return;

            foreach (VerbNetEngine.ThematicRole role in SelectedVerbNetClass.GetThematicRoles(true))
                verbNetRoles.Items.Add(role);
        }

        private void verbNetClasses_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            output2.Clear();

            if (verbNetClasses.SelectedIndex == -1)
                return;

            foreach (string example in SelectedVerbNetClass.GetExamples(false))
                output2.AppendText(example + Environment.NewLine);
        }

        private void showClasses_Click(object sender, EventArgs e)
        {
            if (showClassesFor.Text.Contains("."))
            {
                // look up class in verbnet
                verbNetClasses.Items.Clear();
                VerbClass vnClass;
                if (_verbNet.TryGetClass(showClassesFor.Text, out vnClass))
                    verbNetClasses.Items.Add(vnClass);
            }
            else
                ShowVerbNetClassesFor(showClassesFor.Text);
        }

        private void classesForVerb_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
                showClasses_Click(sender, e);
        }

        private void frames_SelectedIndexChanged(object sender, EventArgs e)
        {
            frameElements.Items.Clear();
            verbNetClasses.Items.Clear();
            verbNetRoles.Items.Clear();
            currentMapping.Items.Clear();
            lexicalUnits.Items.Clear();

            if (frames.SelectedIndex == -1)
                return;

            // show frame elements
            foreach (FrameElement fe in SelectedFrame.FrameElements)
                frameElements.Items.Add(fe);

            // show verbal lexical units
            foreach (LexicalUnit lu in SelectedFrame.LexicalUnits.Where(lu => lu.POS == "V"))
                lexicalUnits.Items.Add(lu.ToString());
        }

        private void frameElements_SelectedIndexChanged(object sender, EventArgs e)
        {
            // show the verbnet linking from semlink
            currentMapping.Items.Clear();
            foreach (string verbNetRole in _semLink.GetVerbNetRolesForFrameNet(SelectedFrameElement.ToString()))
                currentMapping.Items.Add("0." + verbNetRole);
        }

        private void lexicalUnits_SelectedIndexChanged(object sender, EventArgs e)
        {            
            if (lexicalUnits.SelectedIndex == -1)
                return;

            ShowVerbNetClassesFor(SelectedLexicalUnit);
        }

        private void link_Click(object sender, EventArgs e)
        {
            // must have selected a verbnet role to link
            if (verbNetClasses.SelectedIndex == -1 || verbNetRoles.SelectedIndex == -1)
            {
                MessageBox.Show("Must first select a VerbNet role");
                return;
            }

            // make sure we know exactly what we're linking
            if (propBankArguments.SelectedIndex != -1 && frameElements.SelectedIndex != -1)
            {
                MessageBox.Show("Ambiguous linking:  to PropBank or FrameNet?");
                return;
            }

            // get verbnet role to be added
            string verbNetRoleToAdd = SelectedVerbNetClass.ID.Substring(2) + "." + SelectedVerbNetThematicRole;

            // link to propbank
            if (propBankArguments.SelectedIndex != -1)
            {
                // add mapping from selected propbank role to selected verbnet role
                _semLink.AddVerbNetRoleForPropBank(SelectedPropBankVerb + "." + SelectedPropBankRoleSet.ID + "." + SelectedPropBankRole.Number, verbNetRoleToAdd);

                // refresh display as if the user had clicked the propbank argument
                propBankArguments_SelectedIndexChanged(sender, e);
            }
            // link to framenet
            else if (frameElements.SelectedIndex != -1)
            {
                // add mapping from selected frame elemtn to selected verbnet role
                _semLink.AddVerbNetRoleForFrameNet(SelectedFrameElement.ToString(), verbNetRoleToAdd);

                // refresh display as if the user had clicked the frame element
                frameElements_SelectedIndexChanged(sender, e);
            }
        }

        private void unlink_Click(object sender, EventArgs e)
        {
            // must have selected a verbnet role to unlink
            if (currentMapping.SelectedIndex == -1)
            {
                MessageBox.Show("Must first select a mapping to remove");
                return;
            }

            // make sure we know exactly what we're unlinking
            if (propBankArguments.SelectedIndex != -1 && frameElements.SelectedIndex != -1)
            {
                MessageBox.Show("Ambiguous unlinking:  to PropBank or FrameNet?");
                return;
            }

            // get verbnet role to be removed
            string verbNetRoleToRemove = SelectedCurrentMapping.Substring(2);

            // remove from propbank
            if (propBankArguments.SelectedIndex != -1)
            {
                // remove link
                _semLink.RemoveVerbNetRoleForPropBank(SelectedPropBankVerb + "." + SelectedPropBankRoleSet.ID + "." + SelectedPropBankRole.Number, verbNetRoleToRemove);

                // refresh display as if the user had clicked the propbank argument
                propBankArguments_SelectedIndexChanged(sender, e);

                // focus on argument list
                propBankArguments.Focus();
            }
            else if (frameElements.SelectedIndex != -1)
            {
                // remove link
                _semLink.RemoveVerbNetRoleForFrameElement(SelectedFrameElement.ToString(), verbNetRoleToRemove);

                // refresh display as if the user had clicked the frame element
                frameElements_SelectedIndexChanged(sender, e);

                // focus on frame elements
                frameElements.Focus();
            }
        }
        #endregion

        #region issue management
        private void createIssueFile_Click(object sender, EventArgs e)
        {
            IssueForm issueForm = new IssueForm(_propBank, _verbNet, _frameNet, _semLink);
            issueForm.ShowDialog();
        }

        private void openIssueFile_Click(object sender, EventArgs e)
        {
            // get path to issue file
            _issueFilePath = null;
            if (openFileDialog.ShowDialog() == System.Windows.Forms.DialogResult.OK && File.Exists(openFileDialog.FileName))
                _issueFilePath = openFileDialog.FileName;
            else
                return;

            // get issue type
            if (MessageBox.Show("Are issues related to PropBank (Yes) or FrameNet (No)?", "Issue type", MessageBoxButtons.YesNo) == DialogResult.Yes)
                _issueType = IssueType.PropBank;
            else
                _issueType = IssueType.FrameNet;

            // read file
            _issues = new List<string>();
            _currentIssue = 0;
            foreach (string line in File.ReadAllLines(_issueFilePath))
                if (line.Trim() != "")
                    _issues.Add(line);

            _issues.Sort();

            ShowCurrentIssue();

            save.Enabled = true;
        }

        /// <summary>
        /// Shows the current issue
        /// </summary>
        private void ShowCurrentIssue()
        {
            output1.Clear();

            string[] parts = _issues[_currentIssue].Split('#');
            string[] selectionParts = parts[0].Split('|');

            if (_issueType == IssueType.PropBank)
            {
                // select the verb (always have this)
                string[] propBankRoleParts = selectionParts[0].Trim().Split('.');
                propBankVerb.SelectedItem = propBankRoleParts[0];

                // select the role set if available
                if (propBankRoleParts.Length > 1)
                    propBankRoleSets.SelectedItem = propBankRoleSets.Items.Cast<RoleSet>().Where(roleSet => roleSet.ID == int.Parse(propBankRoleParts[1])).First();

                // select the role if available
                if (propBankRoleParts.Length > 2)
                    propBankArguments.SelectedItem = propBankArguments.Items.Cast<Role>().Where(role => role.Number == int.Parse(propBankRoleParts[2])).First();
            }
            else if (_issueType == IssueType.FrameNet)
            {
                // select the frame
                string[] frameFEParts = selectionParts[0].Trim().Split('.');
                frames.SelectedItem = frames.Items.Cast<LAIR.ResourceAPIs.FrameNet.Frame>().Where(frame => frame.Name == frameFEParts[0]).First();

                // select the frame element if available
                if (frameFEParts.Length > 1)
                    frameElements.SelectedItem = frameElements.Items.Cast<FrameElement>().Where(fe => fe.ToString() == frameFEParts[0] + "." + frameFEParts[1]).First();
            }
            else
                throw new Exception("Unrecognized issue type");

            // show alternate verb classes if given
            if (selectionParts.Length > 1)
                ShowVerbNetClassesFor(selectionParts[1].Trim());

            // display message
            output1.AppendText(parts[1].Trim());
        }

        /// <summary>
        /// Shows VerbNet classes for a verb
        /// </summary>
        /// <param name="verb">Verb to show classes for</param>
        private void ShowVerbNetClassesFor(string verb)
        {
            verbNetClasses.Items.Clear();

            foreach (VerbClass verbClass in _verbNet.GetClassesFor(verb))
                verbNetClasses.Items.Add(verbClass);
        }

        /// <summary>
        /// Shows the previous issue
        /// </summary>
        private void PreviousIssue()
        {
            if (_issues == null)
            {
                MessageBox.Show("No issues loaded");
                return;
            }

            if (_currentIssue <= 0)
            {
                MessageBox.Show("At first issue");
                return;
            }

            --_currentIssue;

            ShowCurrentIssue();
        }

        /// <summary>
        /// Shows the next issue
        /// </summary>
        private void NextIssue()
        {
            if (_issues == null)
            {
                MessageBox.Show("No issues loaded");
                return;
            }

            if (_currentIssue >= _issues.Count - 1)
            {
                MessageBox.Show("At last issue");
                return;
            }

            ++_currentIssue;

            ShowCurrentIssue();
        }
        #endregion

        #region miscellaneous
        private void save_Click(object sender, EventArgs e)
        {
            // save current semlink
            if (MessageBox.Show("Overwrite existing SemLink?", "Overwrite?", MessageBoxButtons.YesNo) == DialogResult.Yes)
            {
                _semLink.SavePropBankVerbNetLinking(_semLink.PropBankVerbNetLinkingPath);
                _semLink.SaveFrameNetVerbNetLinking(_semLink.FrameNetVerbNetLinkingPath);
            }
            else
            {
                // save propbank mapping
                saveFileDialog.Title = "PropBank-VerbNet save path";
                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                    _semLink.SavePropBankVerbNetLinking(saveFileDialog.FileName);

                // save framenet mapping
                saveFileDialog.Title = "FrameNet-VerbNet save path";
                if (saveFileDialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                    _semLink.SaveFrameNetVerbNetLinking(saveFileDialog.FileName);

                saveFileDialog.Title = saveFileDialog.FileName = "";
            }

            // re-save issues file if the user wants to remove the issues they addressed
            if (_issues != null && MessageBox.Show("Remove addressed issues from issues file?", "Remove?", MessageBoxButtons.YesNo) == System.Windows.Forms.DialogResult.Yes)
            {
                StreamWriter issueFile = new StreamWriter(_issueFilePath);
                for (int i = _currentIssue; i < _issues.Count; ++i)
                    issueFile.WriteLine(_issues[i]);

                issueFile.Close();
            }
        }

        private void exit_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void SemLinkEditorForm_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Modifiers == Keys.Control && e.KeyCode == Keys.N)
            {
                NextIssue();
                e.SuppressKeyPress = true;
            }
            else if (e.Modifiers == Keys.Control && e.KeyCode == Keys.P)
            {
                PreviousIssue();
                e.SuppressKeyPress = true;
            }
            else if (e.KeyCode == Keys.Delete)
            {
                unlink_Click(sender, e);
                e.SuppressKeyPress = true;
            }
            else if (e.KeyCode == Keys.Insert)
            {
                link_Click(sender, e);
                e.SuppressKeyPress = true;
            }
        }

        private void semLinkBrowse_Click(object sender, EventArgs e)
        {
            if (folderBrowser.ShowDialog() == DialogResult.OK && Directory.Exists(folderBrowser.SelectedPath))
            {
                semLinkDirectory.Text = Settings.Default.SemLinkDirectory = folderBrowser.SelectedPath;
                Settings.Default.Save();
            }
        }

        private void verbNetBrowse_Click(object sender, EventArgs e)
        {
            if (folderBrowser.ShowDialog() == DialogResult.OK && Directory.Exists(folderBrowser.SelectedPath))
            {
                verbNetDirectory.Text = Settings.Default.VerbNetDirectory = folderBrowser.SelectedPath;
                Settings.Default.Save();
            }
        }

        private void semLinkDirectory_TextChanged(object sender, EventArgs e)
        {
            _semLink = new SemLinkEngine(semLinkDirectory.Text);
        }

        private void verbNetDirectory_TextChanged(object sender, EventArgs e)
        {
            _verbNet = new VerbNetEngine(verbNetDirectory.Text);
            VerbClass.DisplayVerbsInToString = true;
        }

        private void SemLinkEditorForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            DialogResult result = MessageBox.Show("Save before exiting?", "Save?", MessageBoxButtons.YesNoCancel);
            if (result == System.Windows.Forms.DialogResult.Yes)
                save_Click(sender, e);
            else if (result == System.Windows.Forms.DialogResult.No)
            { }
            else
                e.Cancel = true;
        }
        #endregion
    }
}