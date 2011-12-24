using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;

using LAIR.ResourceAPIs.VerbNet;

namespace TestApplication
{
    public partial class TestForm : Form
    {
        private VerbNetEngine _verbNetEngine;

        public TestForm()
        {
            InitializeComponent();

            string root = Directory.GetDirectoryRoot(".");
            _verbNetEngine = new VerbNetEngine(root + @"NLP\Resources\verbnet-3.1");

            foreach (VerbClass verbClass in _verbNetEngine.RootVerbClass.GetChildren(true))
                classCombo.Items.Add(verbClass);
        }

        private void getVerbsInClassBtn_Click(object sender, EventArgs e)
        {
            ClearList();

            VerbClass verbClass = classCombo.SelectedItem as VerbClass;
            if (verbClass != null)
                foreach (string verb in verbClass.GetVerbs(recursiveChk.Checked))
                    items.Items.Add(verb);
        }

        private void ClearList()
        {
            items.Items.Clear();
        }

        private void getClassesBtn_Click(object sender, EventArgs e)
        {
            ClearList();

            foreach (VerbClass verbClass in _verbNetEngine.GetClassesFor(verbBox.Text))
                items.Items.Add(verbClass);
        }

        private void itemList_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            object selectedItem = items.SelectedItem;

            ClearList();

            // show verbs in clicked class
            if (selectedItem is VerbClass)
            {
                VerbClass verbClass = selectedItem as VerbClass;
                if (verbClass != null)
                    foreach (string verb in verbClass.GetVerbs(recursiveChk.Checked))
                        items.Items.Add(verb);
            }
            // show classes for clicked verb
            else if (selectedItem is string)
            {
                string verb = (string)selectedItem;
                foreach (VerbClass verbClass in _verbNetEngine.GetClassesFor(verb))
                    items.Items.Add(verbClass);
            }
        }
    }
}