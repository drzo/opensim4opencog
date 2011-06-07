using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Collections;
using System.Threading;
using System.IO;
using System.Text.RegularExpressions;
using LAIR.ResourceAPIs.Wikipedia;

namespace MirrorTables
{
    /// <summary>
    /// Mirror table creation
    /// </summary>
    public partial class MirrorTablesForm : Form
    {
        // delegate for cross-thread control manipuation
        private delegate void SetEnabledDel(Control c, bool enabled);

        // reference to fast mirror
        private FastMirror _fm;  

        /// <summary>
        /// Constructor
        /// </summary>
        public MirrorTablesForm()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Starts the creation of the mirrored database from the vanilla imported Wikipedia data
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void startCreationBtn_Click(object sender, EventArgs e)
        {
            Thread t = new Thread(new ThreadStart(delegate()
            {
                try
                {
                    // get page range and block size
                    int start = int.Parse(startIDBox.Text);
                    int num = int.Parse(numPagesBox.Text);
                    int blockSize = int.Parse(blockSizeBox.Text);

                    // write title index
                    if (titleIndex.Checked)
                        _fm.WriteTitleIndex(start, num, blockSize, WikiDB.Namespace.Main, !ignoreRedirectsChk.Checked);

                    // write page index
                    if (pageIndex.Checked)
                        _fm.WritePageIndex(start, num, blockSize, WikiDB.Namespace.Main, !ignoreRedirectsChk.Checked);

                    MessageBox.Show("Succeeded.  Last title written was \"" + _fm.LastTitleWritten + "\"");                
                }
                catch (Exception ex)
                {
                    MessageBox.Show("Error:  " + ex);
                }

                SetEnabledDel setEn = new SetEnabledDel(SetEnabled);
                Invoke(setEn, startCreationBtn, true);
                Invoke(setEn, stopCreationBtn, false);
            }));

            t.Start();
            startCreationBtn.Enabled = false;
            stopCreationBtn.Enabled = true;
        }

        private void SetEnabled(Control c, bool enabled)
        {
            c.Enabled = enabled;
        }

        private void stopCreationBtn_Click(object sender, EventArgs e)
        {
            _fm.StopWriting = true;
            startCreationBtn.Enabled = true;
            stopCreationBtn.Enabled = false;
        }

        private void statusBtn_Click(object sender, EventArgs e)
        {
            MessageBox.Show("ID:  " + _fm.LastIDWritten + ", Title:  " + _fm.LastTitleWritten);
        }

        private void startDumpBtn_Click(object sender, EventArgs e)
        {
            Thread t = new Thread(new ThreadStart(delegate()
            {
                int start = int.Parse(startIDBox.Text);
                int num = int.Parse(numPagesBox.Text);
                int blockSize = int.Parse(blockSizeBox.Text);

                try
                {
                    // dump pages to lemur index
                    _fm.DumpToLemur(start, num, blockSize, WikiDB.Namespace.Main, dumpDirBox.Text, !ignoreRedirectsChk.Checked);
                    MessageBox.Show("Pages dumped.  Last page written was " + _fm.LastTitleWritten);
                }
                catch (Exception ex)
                {
                    MessageBox.Show("Error:  " + ex);
                }

                SetEnabledDel setEn = new SetEnabledDel(SetEnabled);
                Invoke(setEn, startDumpBtn, true);
                Invoke(setEn, stopDumpBtn, false);
            }));

            t.Start();
            startDumpBtn.Enabled = false;
            stopDumpBtn.Enabled = true;
        }

        private void stopDumpBtn_Click(object sender, EventArgs e)
        {
            _fm.StopWriting = true;
            startDumpBtn.Enabled = true;
            stopDumpBtn.Enabled = false;
        }

        private void dumpBrowseBtn_Click(object sender, EventArgs e)
        {
            dumpDirBox.Text = browseForDirectory(Directory.GetCurrentDirectory());
        }

        private void MirrorTablesForm_Load(object sender, EventArgs e)
        {
            ConnectionSetupForm csf = new ConnectionSetupForm();
            if (csf.ShowDialog() != DialogResult.OK)
            {
                Close();
                return;
            }

            _fm = new FastMirror(csf.Server, csf.MainDatabase, csf.User, csf.Password,
                                 csf.Server, csf.MirrorDatabase, csf.User, csf.Password);
            try { _fm.Connect(); }
            catch (Exception ex)
            {
                MessageBox.Show("Failed to connect to MySQL server:  " + ex);
                Close();
            }
        }

        private string browseForFile(string initialDirectory, string filter)
        {
            openFile.InitialDirectory = initialDirectory;
            openFile.Filter = filter;

            if (openFile.ShowDialog() != DialogResult.OK)
                return "";

            return openFile.FileName;
        }

        private string browseForDirectory(string initialDirectory)
        {
            folderBrowser.SelectedPath = initialDirectory;
            if (folderBrowser.ShowDialog() != DialogResult.OK)
                return "";
            return folderBrowser.SelectedPath;
        }

        private string saveFileAs(string initialDirectory, string filter)
        {
            saveFile.InitialDirectory = initialDirectory;
            saveFile.Filter = filter;
            if (saveFile.ShowDialog() != DialogResult.OK)
                return "";
            return saveFile.FileName;            
        }

        private void lemurTitleListBrowse_Click(object sender, EventArgs e)
        {
            lemurTitleListBox.Text = browseForFile(Directory.GetCurrentDirectory(), "All files | *");
        }

        private void dumpDirBrowse_Click(object sender, EventArgs e)
        {
            lemurDumpTitleList.Text = browseForFile(Directory.GetCurrentDirectory(), "All files | *");
        }

        private void reportFileBrowseBtn_Click(object sender, EventArgs e)
        {
            reportFileBox.Text = saveFileAs(Directory.GetCurrentDirectory(), "All files | *");
        }

        private void startCheckBtn_Click(object sender, EventArgs e)
        {
            List<string> inMainDBNotInMirrorDB = new List<string>();
            List<string> inMirrorDBNotDump = new List<string>();
            List<string> inDumpNotLemur = new List<string>();

            string line;
            StreamWriter reportWriter, logWriter;
            StreamReader fileReader;

            logWriter = new StreamWriter("consistency_check_log.txt");
            logWriter.AutoFlush = true;

            // load dumped titles
            Hashtable lemurDumpTitles = new Hashtable();
            fileReader = new StreamReader(lemurDumpTitleList.Text);
            while ((line = fileReader.ReadLine()) != null)
                lemurDumpTitles.Add(line, null);

            // load lemur index titles
            Hashtable lemurIndexTitles = new Hashtable();
            fileReader = new StreamReader(lemurTitleListBox.Text);
            while((line = fileReader.ReadLine()) != null)
                lemurIndexTitles.Add(line, null);

            // check consistency
            int checkStart = int.Parse(startIDBox.Text);
            int checkNum = int.Parse(numPagesBox.Text);
            int checkEnd = checkStart + checkNum - 1;
            int checkBlockSize = int.Parse(blockSizeBox.Text);

            try
            {
                for (int i = checkStart; i < checkEnd; i += checkBlockSize)
                {
                    logWriter.WriteLine("Completed " + (i + checkBlockSize - 1));

                    List<string> titles;
                    try
                    {
                        // get titles from main DB
                        titles = _fm.MainDB.GetTitleRange(WikiDB.Namespace.Main, i, checkBlockSize, !ignoreRedirectsChk.Checked);
                    }
                    catch (Exception ex)
                    {
                        throw new Exception("Failed at start ID " + i + ":  " + ex);
                    }

                    foreach (string title in titles)
                    {
                        try
                        {
                            /* the Wikipedia database column "is_redirect" isn't always correct, check it with the mirror database "redirects_to",
                             * which is based on a parse of the page (much more accurate */
                            if (_fm.IsRedirect(WikiDB.Namespace.Main, title) && ignoreRedirectsChk.Checked)
                            {
                                logWriter.WriteLine("Inconsistent is_redirect column for page \"" + title + "\"");
                                continue;
                            }

                            // check for title in mirror DB
                            if (!_fm.PageExists(WikiDB.Namespace.Main, title))
                                inMainDBNotInMirrorDB.Add(title);
                            // check for title in dump
                            else if (!lemurDumpTitles.Contains(title))
                                inMirrorDBNotDump.Add(title);
                            // check for title in lemur index
                            else if (!lemurIndexTitles.Contains(title))
                                inDumpNotLemur.Add(title);
                        }
                        catch (Exception ex)
                        {
                            throw new Exception("Failed at start ID " + i + ", title \"" + title + "\":  " + ex);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                logWriter.WriteLine("Failed.");
                MessageBox.Show("Failed.  Partial report will be written.  Error:  " + ex);
            }

            // write report
            reportWriter = new StreamWriter(reportFileBox.Text);
            reportWriter.WriteLine("[In main DB but not in mirror DB]");
            foreach (string title in inMainDBNotInMirrorDB)
                reportWriter.WriteLine("  " + title);

            reportWriter.WriteLine("[In mirror DB but not in dump]");
            foreach (string title in inMirrorDBNotDump)
                reportWriter.WriteLine("  " + title);

            reportWriter.WriteLine("[In dump but not in Lemur index]");
            foreach (string title in inDumpNotLemur)
                reportWriter.WriteLine("  " + title);

            reportWriter.Close();
            logWriter.Close();
        }

        private void getDumpedTitlesBtn_Click(object sender, EventArgs e)
        {
            string titleFile = saveFileAs(Directory.GetCurrentDirectory(), "All files | *");
            StreamWriter titleWriter = new StreamWriter(titleFile);

            // load dumped titles
            StreamReader fileReader;
            string line;
            string docnoTag = "<DOCNO>";
            string[] dumpFiles = Directory.GetFiles(dumpDirBox.Text);
            foreach (string dumpFile in dumpFiles)
            {
                fileReader = new StreamReader(dumpFile);

                while ((line = fileReader.ReadLine()) != null)
                {
                    if (line.Length >= docnoTag.Length &&
                        line.Substring(0, docnoTag.Length) == docnoTag)
                    {
                        string title = line.Substring(docnoTag.Length);
                        title = title.Substring(0, title.LastIndexOf("</DOCNO>"));
                        titleWriter.WriteLine(title);
                    }
                }
            }

            titleWriter.Close();
        }
    }
}