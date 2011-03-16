#if MONO
#else
//#define MICROSOFT
#endif
using System;
using System.Windows.Forms;
using cogbot;
using MushDLR223.Utilities;

namespace CycWorldModule.DotCYC
{
    using org.opencyc.api;
    using System.Reflection;
    public partial class CycConnectionForm : Form
    {
        //  WinformREPL.REPLForm replForm = null;
        static private CycAccess m_cycAccess = null;
        public CycAccess cycAccess
        {
            get
            {
                return getCycAccess();
            }
        }
        public bool wasConnected = false;
        public CycConnectionForm()
        {
            InitializeComponent();
#if MICROSOFT
            // add this line to the form's constructor after InitializeComponent() 
          hMenu = GetSystemMenu(this.Handle, false);
#endif
            // replForm = new WinformREPL.REPLForm();
            // replForm.Show(); 
        }

        private void btnConnect_Click(object sender, EventArgs e)
        {
            try { btnConnect.Enabled = false; }
            catch (Exception) { }
            if (!IsConnected())
                connect();
            else
                disconnect();
            try { btnConnect.Enabled = true; }
            catch (Exception) { }
        }

        private bool IsConnected()
        {
            if (m_cycAccess != null) wasConnected = !m_cycAccess.isClosed();
            else wasConnected = false;
            if (wasConnected) btnConnect.Text = "Disconnect";
            else btnConnect.Text = "Connect";
            return wasConnected;
        }

        private void btnEval_Click(object sender, EventArgs e)
        {
            try
            {
                txtCycOutput.Text = "" + cycAccess.converseObject(txtEvalString.Text);
            }
            catch (Exception ee)
            {
                txtCycOutput.Text = ee.ToString();
            }
        }

        public CycAccess getCycAccess()
        {
            if (!IsConnected()) connect();
            return m_cycAccess;
        }

        private void connect()
        {
            if (IsConnected()) return;
            try
            {
                m_cycAccess = new CycAccess(cycServerAddress.Text, Int16.Parse(cycBasePort.Text));
                m_cycAccess.getCycConnection().traceOn();
                m_cycAccess.find("isa");
                //   m_cycAccess.getCycConnection().converse("()");
                m_cycAccess.getCycConnection().traceOff();
                //  CycAccess.genls.ToString();
            }
            catch (Exception ee)
            {
                DLRConsole.DebugWriteLine("" + ee);
                SimCyclifier.Trace();
                txtCycOutput.Text = ee.ToString();
            }
            if (m_cycAccess != null)
            {
                ClientManager.addSetting("cycHostName", m_cycAccess.getHostName());
                ClientManager.addSetting("cycBasePort", "" + m_cycAccess.getBasePort());
            }
            wasConnected = IsConnected();
        }
        private void disconnect()
        {
            if (m_cycAccess != null)
            {
                m_cycAccess.getCycConnection().close();
                m_cycAccess = null;
            }
            wasConnected = IsConnected();
        }

        private void CycConnectionForm_Load(object sender, EventArgs e)
        {
            wasConnected = IsConnected();
        }

#if MICROSOFT
        private const uint SC_CLOSE = 0xf060;
        private const uint MF_GRAYED = 0x01;
        private IntPtr hMenu;

        [DllImport("user32.dll")]
        private static extern IntPtr GetSystemMenu(IntPtr hWnd, bool bRevert);

        [DllImport("user32.dll")]
        private static extern int EnableMenuItem(IntPtr hMenu, uint wIDEnableItem, uint wEnable);

#endif
        private void CycConnectionForm_NoClose(object sender, EventArgs e)
        {
#if MICROSOFT
            EnableMenuItem(hMenu, SC_CLOSE, MF_GRAYED);
#endif
        }

        public void Reactivate()
        {
            this.Show();
            //if (this.WindowState == FormWindowState.Minimized)              
            this.WindowState = FormWindowState.Normal;
            this.Visible = true;
            this.Activate();
        }

        private void txtCycOutput_TextChanged(object sender, EventArgs e)
        {

        }

        private String objToStrimg(object o)
        {
            Type t = o.GetType();
            if (t.IsEnum)
            {
                //System.Reflection.MemberInfo[] mi = t.GetMembers();
                return Enum.GetName(t, o);
            }
            if (t.IsValueType)
            {
                return o.ToString();
                //
                //return ValueType GetName(t, o);
            }
            System.Reflection.MemberInfo[] mi = t.GetMembers();
            for (int i = 0; i < mi.Length; i++)
            {
                System.Reflection.MemberInfo m = mi[i];
                if (m.MemberType == MemberTypes.Field)
                {
                    t.GetField(m.Name).GetValue(o);

                }
            }
            return o.ToString();
        }
    }
}


