using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;

using LAIR.ResourceAPIs.PennBank.PropBank;
using LAIR.ResourceAPIs.VerbNet;
using LAIR.ResourceAPIs.SemLink;
using LAIR.Collections.Generic;
using LAIR.ResourceAPIs.FrameNet;

namespace SemLinkEditor
{
    public partial class IssueForm : Form
    {
        PropBankEngine _propBank;
        VerbNetEngine _verbNet;
        FrameNetEngine _frameNet;
        SemLinkEngine _semLink;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="propBank">PropBank to analyze</param>
        /// <param name="verbNet">VerbNet to analyze</param>
        /// <param name="frameNet">FrameNet to analyze</param>
        /// <param name="semLink">SemLink to analyze</param>
        public IssueForm(PropBankEngine propBank, VerbNetEngine verbNet, FrameNetEngine frameNet, SemLinkEngine semLink)
        {
            InitializeComponent();

            _propBank = propBank;
            _verbNet = verbNet;
            _frameNet = frameNet;
            _semLink = semLink;
        }

        private void issueFileBrowse_Click(object sender, EventArgs e)
        {
            if (saveFileDialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                issueFilePath.Text = saveFileDialog.FileName;
        }

        private void issueFilePath_TextChanged(object sender, EventArgs e)
        {
            create.Enabled = issueFilePath.Text != "";
        }

        private void create_Click(object sender, EventArgs e)
        {
            // open issue file
            StreamWriter issueFile = new StreamWriter(issueFilePath.Text);

            // find verbs that aren't in semlink but exist in some verb class
            if (pbVerbsInVnNotSl.Checked)
                foreach (string verb in _propBank.AllVerbs)
                    if (!_semLink.ContainsPropBankVerb(verb) && _verbNet.ContainsVerb(verb))
                        issueFile.WriteLine(verb + " # Not mapped to VerbNet but exists in VerbNet.");

            // find verbs mapped to a class that doesn't exist or to a class that doesn't contain them
            if (pbVerbsInvalidVnClass.Checked)
            {
                VerbClass verbClass;
                foreach (string verb in _semLink.PropBankVerbs)
                    foreach (string verbClassID in _semLink.GetVerbClassesForPropBankVerb(verb))
                        if (!_verbNet.TryGetClass("0." + verbClassID, out verbClass))
                            issueFile.WriteLine(verb + " # Mapped to non-existent class " + verbClassID + ".");
                        else if (!verbClass.Contains(verb))
                        {
                            Set<VerbClass> inClasses = _verbNet.GetClassesFor(verb);
                            if (inClasses.Count == 0)
                                issueFile.WriteLine(verb + " # Not in class " + verbClassID + " nor anywhere else.");
                            else
                                issueFile.WriteLine(verb + " # Not in class " + verbClassID + " but in some class.");
                        }
            }

            // find propbank verbs not in verbnet
            if (pbVerbsNotInVN.Checked)
                foreach (string verb in _propBank.AllVerbs)
                    if (!_verbNet.ContainsVerb(verb))
                        issueFile.WriteLine(verb + " # PropBank verb is not in VerbNet.");

            // find all re- verbs that are not in semlink but whose non-prefixed form is in verbnet (we can probably map these
            if (propBankReVerbs.Checked)
                foreach (string verb in _propBank.AllVerbs)
                    if (verb.StartsWith("re") && !_semLink.ContainsPropBankVerb(verb) && _verbNet.ContainsVerb(verb.Substring(2)))
                        issueFile.WriteLine(verb + "|" + verb.Substring(2) + " # PropBank verb present in VerbNet without prefix.");

            // find invalid propbank roles
            if(invalidPropBankRoles.Checked)
                foreach (string propBankRole in _semLink.PropBankRoles)
                {
                    string[] parts = propBankRole.Split('.');
                    string verb = parts[0];
                    int roleSetID = int.Parse(parts[1]);
                    int arg = int.Parse(parts[2]);
                    if (!_propBank.Contains(verb) || !_propBank.GetFrame(verb).Contains(roleSetID) || !_propBank.GetFrame(verb).GetRoleSet(roleSetID).Contains(arg))
                        issueFile.WriteLine(propBankRole);
                }

            // find invalid verbnet thematic roles
            if (invalidVnRole.Checked)
                if (!includePB.Checked && !includeFN.Checked)
                    MessageBox.Show("Must include either PropBank or FrameNet to checked invalid VerbNet roles");
                else
                {
                    // find verbnet roles that have a bad thematic role
                    Set<string> badVerbNetRoles = new Set<string>(false);
                    foreach (string vnRole in _semLink.GetVerbNetRoles(includePB.Checked, includeFN.Checked))
                    {
                        // check if the current role is bad
                        bool badVnRole = false;
                        int lastPeriodIndex = vnRole.LastIndexOf(".");
                        string classID = "0." + vnRole.Substring(0, lastPeriodIndex);
                        string role = vnRole.Substring(lastPeriodIndex + 1);

                        // make sure we can get to the class
                        VerbClass verbClass;
                        if (_verbNet.TryGetClass(classID, out verbClass))
                        {
                            try
                            {
                                // check role
                                VerbNetEngine.ThematicRole thematicRole = (VerbNetEngine.ThematicRole)Enum.Parse(typeof(VerbNetEngine.ThematicRole), role);
                                if (!verbClass.ContainsThematicRole(thematicRole, true))
                                    badVnRole = true;
                            }
                            catch (Exception)
                            {
                                badVnRole = true;
                            }
                        }
                        else
                            badVnRole = true;

                        if (badVnRole)
                            badVerbNetRoles.Add(vnRole);
                    }

                    // now find mappings to the bad roles
                    Set<string> badPropBankRoles = new Set<string>(false);
                    Set<string> badFrameElements = new Set<string>(false);
                    foreach (string badVerbNetRole in badVerbNetRoles)
                    {
                        // find any propbank role mapped to the given verbnet role
                        if (includePB.Checked)
                            foreach (string propBankRole in _semLink.PropBankRoles)
                                foreach (string vnRole in _semLink.GetVerbNetRolesForPropBank(propBankRole))
                                    if (vnRole == badVerbNetRole)
                                        badPropBankRoles.Add(propBankRole);

                        // find any frame element mapped to the given verbnet role
                        if (includeFN.Checked)
                            foreach (string frameElement in _semLink.FrameElements)
                                foreach (string vnRole in _semLink.GetVerbNetRolesForFrameNet(frameElement))
                                    if (vnRole == badVerbNetRole)
                                        badFrameElements.Add(frameElement);
                    }

                    // log bad propbank roles
                    foreach (string badPropBankRole in badPropBankRoles)
                        issueFile.WriteLine(badPropBankRole + " # PropBank role " + badPropBankRole + " maps to invalid VerbNet role.");

                    // log bad frame elements
                    foreach (string badFrameElement in badFrameElements)
                        issueFile.WriteLine(badFrameElement + " # Frame element " + badFrameElement + " maps to invalid VerbNet role.");
                }

            // frames mapped to non-existent or non-containing verbnet class
            if (fnFramesInvalidVnClass.Checked)
            {
                VerbClass verbClass;
                foreach (string frame in _semLink.FrameNetFrames)
                    foreach (string verbClassID in _semLink.GetVerbClassesForFrameNetFrame(frame))
                        if (!_verbNet.TryGetClass("0." + verbClassID, out verbClass))
                            issueFile.WriteLine(frame + " # Mapped to non-existent class " + verbClassID + ".");
                        else
                            // check if class contains any of the verbal lexical units in the frame
                            if (_frameNet.GetFrame(frame).LexicalUnits.Where(lu => lu.POS == "V" && verbClass.Contains(lu.ToString())).Count() == 0)
                            {
                                Set<VerbClass> inClasses = _verbNet.GetClassesFor(_frameNet.GetFrame(frame).LexicalUnits.Where(lu => lu.POS == "V").Select(lu => lu.ToString()));
                                if (inClasses.Count == 0)
                                    issueFile.WriteLine(frame + " # Not in class " + verbClassID + " nor anywhere else.");
                                else
                                    issueFile.WriteLine(frame + " # Not in class " + verbClassID + " but in some class.");
                            }
            }

            // invalid frame or frame elements
            if (invalidFrameElements.Checked)
            {
                List<string> issues = new List<string>();
                foreach (string frameElement in _semLink.FrameElements)
                {
                    string[] parts = frameElement.Split('.');
                    string frameName = parts[0];
                    string fe = parts[1];
                    LAIR.ResourceAPIs.FrameNet.Frame frame;
                    if (!_frameNet.TryGetFrame(frameName, out frame))
                        issues.Add(frameElement + " # Invalid frame.");
                    else if (!frame.FrameElements.Contains(fe))
                        issues.Add(frameElement + " # Invalid frame element.");
                }

                issues.Sort();
                foreach (string issue in issues)
                    issueFile.WriteLine(issue);
            }
            
            issueFile.Close();

            Close();
        }

        private void invalidVnRole_CheckedChanged(object sender, EventArgs e)
        {
            includePB.Enabled = includeFN.Enabled = invalidVnRole.Checked;
        }

        private void cancel_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}
