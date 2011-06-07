using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;

using LAIR.ResourceAPIs.SemLink;
using LAIR.Collections.Generic;

namespace TestApplication
{
    /// <summary>
    /// Test form for the SemLink API
    /// </summary>
    public partial class TestForm : Form
    {
        private SemLinkEngine _semLinkEngine;

        /// <summary>
        /// Gets currently selected mapping extension
        /// </summary>
        private SemLinkEngine.VerbNetExtension SelectedExtension
        {
            get
            {
                if (super.Checked)
                    return SemLinkEngine.VerbNetExtension.SuperClass;
                else if (sub.Checked)
                    return SemLinkEngine.VerbNetExtension.SubClass;
                else if (both.Checked)
                    return SemLinkEngine.VerbNetExtension.SuperAndSubClass;
                else
                    return SemLinkEngine.VerbNetExtension.None;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public TestForm()
        {
            InitializeComponent();

            _semLinkEngine = new SemLinkEngine(Directory.GetDirectoryRoot(".") + @"NLP\Resources\semlink");
        }

        private void linkPbVn_Click(object sender, EventArgs e)
        {
            output.Clear();

            if (verb.Text != "" && roleSet.Text != "" && verbRole.Text != "")
            {
                output.AppendText("PropBank -> VerbNet mapping:  " + Environment.NewLine);
                foreach (string verbNetRole in _semLinkEngine.GetVerbNetRolesForPropBank(verb.Text, int.Parse(roleSet.Text), int.Parse(verbRole.Text)))
                    output.AppendText("  " + verbNetRole + Environment.NewLine);
            }
            else if (vnClass.Text != "" && vnRole.Text != "")
            {
                output.AppendText("VerbNet -> PropBank mapping:  " + Environment.NewLine);
                foreach (string propBankRole in _semLinkEngine.GetPropBankRolesForVerbNet(vnClass.Text, vnRole.Text, SelectedExtension))
                    output.AppendText("  " + propBankRole + Environment.NewLine);
            }
        }

        private void linkVnFn_Click(object sender, EventArgs e)
        {
            output.Clear();

            if (vnClass.Text != "" && vnRole.Text != "")
            {
                output.AppendText("VerbNet -> FrameNet mapping:" + Environment.NewLine);
                foreach (string fe in _semLinkEngine.GetFrameElementsForVerbNet(vnClass.Text, vnRole.Text, SelectedExtension))
                    output.AppendText("  " + fe + Environment.NewLine);
            }
            else if (frame.Text != "" && frameElement.Text != "")
            {
                output.AppendText("FrameNet -> VerbNet mapping:" + Environment.NewLine);
                foreach (string role in _semLinkEngine.GetVerbNetRolesForFrameNet(frame.Text, frameElement.Text))
                    output.AppendText("  " + role + Environment.NewLine);
            }
        }

        private void linkPbFn_Click(object sender, EventArgs e)
        {
            output.Clear();
            if (verb.Text != "" && verbRole.Text != "")
            {
                output.AppendText("PropBank -> FrameNet mapping:" + Environment.NewLine);
                foreach (string fe in _semLinkEngine.GetFrameElementsForPropBank(verb.Text, int.Parse(roleSet.Text), int.Parse(verbRole.Text), SelectedExtension))
                    output.AppendText("  " + fe + Environment.NewLine);
            }
            else if (frame.Text != "" && frameElement.Text != "")
            {
                output.AppendText("FrameNet -> PropBank mapping:" + Environment.NewLine);
                foreach (string pbRole in _semLinkEngine.GetPropBankRolesForFrameNet(frame.Text, frameElement.Text, SelectedExtension))
                    output.AppendText("  " + pbRole + Environment.NewLine);
            }
        }
    }
}
