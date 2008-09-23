using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace cogbot.DotCYC
{
    using org.opencyc.api;
    using org.opencyc.cycobject;
    public partial class CycConnectionForm : Form
    {
        public CycAccess cycAccess = null;
        public bool wasConnected = false;       
        public CycConnectionForm()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            btnConnect.Enabled = false;
            if (!IsConnected())
                connect();
            else
                disconnect();
            btnConnect.Enabled = true;
        }

        private bool IsConnected()
        {
            if (cycAccess != null) wasConnected = !cycAccess.isClosed();
            else wasConnected = false;
            if (wasConnected) btnConnect.Text = "Disconnect";
            else btnConnect.Text = "Connect";
            return wasConnected;
        }

        private void btnEval_Click(object sender, EventArgs e)
        {
            try
            {
                txtCycOutput.Text = "" + getCycAccess().converseObject(txtEvalString.Text);
            } catch (Exception ee)
            {
                txtCycOutput.Text = ee.ToString();
            }
        }

        public CycAccess getCycAccess()
        {
            if (!IsConnected()) connect();
            return cycAccess;
        }

        private void connect()
        {
            btnConnect.Text = "Disconnect";
            if (IsConnected()) return;
            cycAccess = new CycAccess(cycServerAddress.Text, Int16.Parse(cycBasePort.Text));
            IsConnected();
        }
        private void disconnect()
        {
            btnConnect.Text = "Connect";
            if (!IsConnected()) return;
            if (cycAccess != null)
            {
                cycAccess.getCycConnection().close();
                cycAccess = null;
            }
            wasConnected = false;
        }

        private void CycConnectionForm_Load(object sender, EventArgs e)
        {

        }

        private void CycConnectionForm_Load_1(object sender, EventArgs e)
        {

        }

    }
}