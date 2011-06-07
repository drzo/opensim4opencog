using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Text.RegularExpressions;
using System.Collections;
using System.IO;

using TestProject.Properties;
using LAIR.ResourceAPIs.Wikipedia;

namespace TestProject
{
    /// <summary>
    /// Demonstrates the Wikipedia API
    /// </summary>
    public partial class TestForm : Form
    {
        private Page _page;          // current page displayed
        private FastMirror _mirror;  // mirror database

        /// <summary>
        /// Constructor
        /// </summary>
        public TestForm()
        {
            InitializeComponent();
        }

        private void getPageBtn_Click(object sender, EventArgs e)
        {
            FetchPage(titleBox.Text);
        } 

        /// <summary>
        /// Gets a page from the database
        /// </summary>
        /// <param name="title">Title of the page</param>
        private void FetchPage(string title)
        {            
            if (mainRadio.Checked)
                _page = _mirror.MainDB.LookupPage(WikiDB.Namespace.Main, title, true);
            else
                _page = _mirror.LookupPage(WikiDB.Namespace.Main, title, true);

            if (_page == null)
            {
                MessageBox.Show("No page found");
                titleBox.Text = "";
                headingTree.Nodes.Clear();
                linkList.Items.Clear();
                return;
            }

            TreeNode docNode = new TreeNode(title);
            docNode.Expand();
            foreach (Section s in _page.Sections)
                AddSectionToTree(s, docNode);

            headingTree.Nodes.Clear();
            headingTree.Nodes.Add(docNode);
            linkList.Items.Clear();

            List<WikiLink> links = _page.WikiLinks;
            links.Sort();
            foreach (WikiLink wl in links)
            {
                bool contained = false;
                foreach (WikiLink wl2 in linkList.Items)
                {
                    if (wl.DestPageURL == wl2.DestPageURL)
                        contained = true;
                }

                if (!contained)
                    linkList.Items.Add(wl);
            }
            linkLbl.Text = "Wiki links (" + linkList.Items.Count + ")";            
        }

        private void AddSectionToTree(Section s, TreeNode n)
        {
            TreeNode secNode = new TreeNode(s.Name + " (ID:  " + s.ID + ", start line:  " + s.SectionStart + ", end line:  " + s.SectionEnd + ")");
            n.Nodes.Add(secNode);

            foreach (Section sub in s.SubSections)
                AddSectionToTree(sub, secNode);
        }

        private string SectionString(Section sect)
        {
            string spaces = "";
            for (int i = 0; i < sect.Depth; ++i)
                spaces += "  ";

            string str = spaces + sect.Name + Environment.NewLine;

            foreach (Section sub in sect.SubSections)
                str += SectionString(sub);

            return str;
        }

        private void linkList_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            if (linkList.SelectedItem == null)
                return;

            WikiLink link = (WikiLink)linkList.SelectedItem;
            titleBox.Text = link.DestPageURL;

            FetchPage(link.DestPageURL);
        }

        private void writeMirrorBtn_Click(object sender, EventArgs e)
        {
            if (_page == null)
                return;

            _mirror.WritePage(_page);
        }

        private void TestForm_Load(object sender, EventArgs e)
        {
            try
            {
                ConnectionSetupForm csf = new ConnectionSetupForm();
                if (csf.ShowDialog() == DialogResult.OK)
                {
                    _mirror = new FastMirror(csf.Server, csf.MainDatabase, csf.User, csf.Password,
                                             csf.Server, csf.MirrorDatabase, csf.User, csf.Password);
                    _mirror.Connect();
                }
                else
                {
                    Close();
                    return;
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show("Failed to connect:  " + ex);
                return;
            }
        }

        private void showSecLayoutBtn_Click(object sender, EventArgs e)
        {
            if (_page != null)
                MessageBox.Show(_page.GetSectionLayout(false, true));
        }

        #region consistency checking
        private void checkConsistBtn_Click(object sender, EventArgs e)
        {
            Page p1 = _mirror.MainDB.LookupPage(WikiDB.Namespace.Main, titleBox.Text, false);
            Page p2 = _mirror.LookupPage(WikiDB.Namespace.Main, titleBox.Text, false);

            // compare lines
            CompareLines(p1.Lines, p2.Lines);

            // compare sections
            if (p1.Sections.Count != p2.Sections.Count)
                Error("Section mismatch");
            for (int i = 0; i < p1.Sections.Count; ++i)
                CompareSecs((Section)p1.Sections[i], (Section)p2.Sections[i]);
            
            // compare TF
            if (p1.TermFrequencies.Count != p2.TermFrequencies.Count)
                Error("TF mismatch");

            Dictionary<string, double>.Enumerator p1Freqs = p1.TermFrequencies.GetEnumerator();
            Dictionary<string, double>.Enumerator p2Freqs = p2.TermFrequencies.GetEnumerator();

            while (p1Freqs.MoveNext())
            {
                if (!p2Freqs.MoveNext())
                    Error("TF Mismatch");

                if (!p1.TermFrequencies.ContainsKey(p2Freqs.Current.Key) ||
                    !p2.TermFrequencies.ContainsKey(p1Freqs.Current.Key) ||
                    p1.TermFrequencies[p2Freqs.Current.Key] != p2Freqs.Current.Value ||
                    p2.TermFrequencies[p1Freqs.Current.Key] != p1Freqs.Current.Value)
                    Error("TF Mismatch");
            }
            
            // compare links
            List<WikiLink> p1Links = p1.WikiLinks;
            List<WikiLink> p2Links = p2.WikiLinks;
            CompareLinks(p1Links, p2Links);

            MessageBox.Show("Check succeeded");
        }

        private void CompareSecs(Section s1, Section s2)
        {
            if (s1 != s2)
                Error("Section mismatch");

            CompareLines(s1.Lines, s2.Lines);
            if (s1.ID != s2.ID)
                Error("Section ID mismatch");

            CompareLinks(s1.WikiLinks, s2.WikiLinks);

            if (s1.SubSections.Count != s2.SubSections.Count)
                Error("Subsection mismatch");
            for (int i = 0; i < s1.SubSections.Count; ++i)
                CompareSecs(s1.SubSections[i], s2.SubSections[i]);            
        }

        private void CompareLinks(List<WikiLink> l1, List<WikiLink> l2)
        {
            if (l1.Count != l2.Count)
                Error("Link mismatch");
            for (int i = 0; i < l1.Count; ++i)
            {
                if (l1[i] != l2[i])
                    Error("Link mismatch");
            }
        }

        private void Error(string s)
        {
            throw new Exception(s);
        }

        private void CompareLines(List<string> l1, List<string> l2)
        {
            if (l1.Count != l2.Count)
                Error("Line number mismatch");

            for (int i = 0; i < l1.Count; ++i)
            {
                if (l1[i] != l2[i])
                    Error("Line value mismatch");
            }
        }
        #endregion

        private void speedTestBtn_Click(object sender, EventArgs e)
        {
            // look up the first thousand pages from each database
            List<string> titles = _mirror.MainDB.GetTitleRange(WikiDB.Namespace.Main, 0, 1000, false);

            _mirror.MainDB.ClearPageCache();
            DateTime mainStart = DateTime.Now;
            foreach (string title in titles)
            {
                Page p = _mirror.MainDB.LookupPage(WikiDB.Namespace.Main, title, false);
                if (p == null)
                    throw new Exception("Failed to find page");
            }
            DateTime mainEnd = DateTime.Now;

            _mirror.ClearPageCache();
            DateTime mirrorStart = DateTime.Now;
            foreach (string title in titles)
            {
                Page p = _mirror.LookupPage(WikiDB.Namespace.Main, title, false);
                if (p == null)
                    throw new Exception("Failed to find page");
            }
            DateTime mirrorEnd = DateTime.Now;

            MessageBox.Show("Main started at " + mainStart + ", finished at " + mainEnd + "\n" +
                            "Mirror started at " + mirrorStart + ", finished at " + mirrorEnd);
        }

        private void markupBox_TextChanged(object sender, EventArgs e)
        {
            string text = "";
            if (File.Exists(markupBox.Text))
            {
                StreamReader sr = new StreamReader(markupBox.Text);
                text = sr.ReadToEnd();
                sr.Close();
            }
            else
                text = markupBox.Text;

            string noMarkup = WikiMarkup.RemoveIrrelevantMarkup(text);
            noMarkup = WikiMarkup.ProcessMarkup(noMarkup);
            resultLbl.Text = "Result:  " + noMarkup;
        }

        private void headingTree_DoubleClick(object sender, EventArgs e)
        {
            string s = headingTree.SelectedNode.Text;
            Regex idRE = new Regex(@"ID:\s*(?<ID>([^,]+)),");
            Match m = idRE.Match(s);
            if (m.Success)
            {
                string id = m.Groups["ID"].Value;
                string secText = _page.GetSection(id).Text;
                MessageBox.Show(secText);
            }
        }
    }
}