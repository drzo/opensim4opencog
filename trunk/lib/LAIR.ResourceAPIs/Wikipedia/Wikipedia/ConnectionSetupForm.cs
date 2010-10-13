using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace LAIR.ResourceAPIs.Wikipedia
{
    /// <summary>
    /// Connection setup form
    /// </summary>
    public partial class ConnectionSetupForm : Form
    {
        /// <summary>
        /// Gets or sets the server
        /// </summary>
        public string Server
        {
            get { return serverBox.Text; }
            set { serverBox.Text = value; }
        }

        /// <summary>
        /// Gets or sets the port
        /// </summary>
        public int Port
        {
            get { try { return int.Parse(portBox.Text); } catch (Exception) { return -1; } }
            set { portBox.Text = value.ToString(); }
        }

        /// <summary>
        /// Gets or sets the database name
        /// </summary>
        public string MainDatabase
        {
            get { return mainDatabaseBox.Text; }
            set { mainDatabaseBox.Text = value; }
        }

        /// <summary>
        /// Gets or sets the mirror database name
        /// </summary>
        public string MirrorDatabase
        {
            get { return mirrorDatabaseBox.Text; }
            set { mirrorDatabaseBox.Text = value; }
        }

        /// <summary>
        /// Gets or sets the user
        /// </summary>
        public string User
        {
            get { return userBox.Text; }
            set { userBox.Text = value; }
        }

        /// <summary>
        /// Gets or sets the password
        /// </summary>
        public string Password
        {
            get { return passwordBox.Text; }
            set { passwordBox.Text = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public ConnectionSetupForm()
        {
            InitializeComponent();
        }

        private void okBtn_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.OK;
            Close();
        }

        private void cancelBtn_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
            Close();
        }

        private void ConnectionSetupForm_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
                okBtn_Click(sender, e);
        }

        private void mainDatabaseBox_TextChanged(object sender, EventArgs e)
        {
            mirrorDatabaseBox.Text = mainDatabaseBox.Text;
        }
    }
}