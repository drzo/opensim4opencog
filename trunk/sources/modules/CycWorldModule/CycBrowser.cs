// 
// Radegast Metaverse Client
// Copyright (c) 2009, Radegast Development Team
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of the application "Radegast", nor the names of its
//       contributors may be used to endorse or promote products derived from
//       this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// $Id: MapConsole.cs 293 2009-09-30 12:54:20Z latifer@gmail.com $
//
﻿using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Text.RegularExpressions;
using System.Threading;
using System.Windows.Forms;
﻿using cogbot.TheOpenSims;
﻿using CycWorldModule.DotCYC;
﻿using OpenMetaverse;
﻿using org.opencyc.api;
﻿using org.opencyc.cycobject;
﻿using Radegast;

namespace CycWorldModule
{
    [PermissionSet(SecurityAction.Demand, Name = "FullTrust")]
    [ComVisibleAttribute(true)]
    public partial class CycBrowser : UserControl
    {
        RadegastInstance instance;
        GridClient client { get { return instance.Client; } }
        bool Active { get { return client.Network.Connected; } }
        WebBrowser map;
    //    Regex slscheme = new Regex("^secondlife://(.+)/([0-9]+)/([0-9]+)");
        bool InTeleport = false;
        public string cycURL
        {
            get
            {
                var v = SimCyclifier.cycAccess;
                if (v==null)
                {
                    v = CycAccess.sharedCycAccessInstance;
                }
                if (v!=null)
                {
                    string s = v.getHostName();
                    int bp = v.getBasePort();
                    if (bp > 0)
                    {
                        s += ":" + (bp + 2);
                    }
                    else
                    {
                        s += ":3602";
                    }
                    return string.Format("http://{0}/cgi-bin/cyccgi/cg?cb-start", s);
                }
                return "http://cycserver:3602/cgi-bin/cyccgi/cg?cb-start";
            }
        }

        public CycBrowser(RadegastInstance i)
        {
            InitializeComponent();
            Disposed += new EventHandler(frmMap_Disposed);

            instance = i;
            try
            {
                map = new WebBrowser();
                map.Dock = DockStyle.Fill;
                map.AllowWebBrowserDrop = false;
                map.Navigate(cycURL);
                map.WebBrowserShortcutsEnabled = true;
                map.ScriptErrorsSuppressed = false;
                map.ObjectForScripting = this;
                map.AllowNavigation = true;
                if (instance.MonoRuntime)
                {
                    map.Navigating += new WebBrowserNavigatingEventHandler(map_Navigating);
                }
                pnlMap.Controls.Add(map);
            }
            catch (Exception e)
            {
                Logger.Log(e.Message, Helpers.LogLevel.Warning, client, e);
                pnlMap.Visible = false;
                map = null;
            }

            // Register callbacks
            //TODO client.Network.OnCurrentSimChanged += new NetworkManager.CurrentSimChangedCallback(Network_OnCurrentSimChanged);
        }

        void frmMap_Disposed(object sender, EventArgs e)
        {
            // Unregister callbacks
            //TODO client.Network.OnCurrentSimChanged -= new NetworkManager.CurrentSimChangedCallback(Network_OnCurrentSimChanged);

            if (map != null)
            {
                map.Dispose();
                map = null;
            }
        }
        #region NetworkEvents

        void Network_OnCurrentSimChanged(Simulator PreviousSimulator)
        {
            if (InvokeRequired)
            {
                BeginInvoke(new MethodInvoker(delegate()
                    {
                        Network_OnCurrentSimChanged(PreviousSimulator);
                    }
                ));
                return;
            }

            gotoRegion(client.Network.CurrentSim.Name, (int)client.Self.SimPosition.X, (int)client.Self.SimPosition.Y);
        }

        #endregion NetworkEvents

        void map_Navigating(object sender, WebBrowserNavigatingEventArgs e)
        {
            e.Cancel = true;
            Regex r = new Regex(@"^(http://slurl.com/secondlife/|secondlife://)([^/]+)/(\d+)/(\d+)(/(\d+))?");
            Match m = r.Match(e.Url.ToString());

            if (m.Groups.Count > 3)
            {

            }
        }

        #region JavascriptHooks
        void gotoRegion(string regionName, int simX, int simY)
        {
            if (!Visible || map == null || map.Document == null) return;
            if (instance.MonoRuntime)
            {
                map.Document.InvokeScript(string.Format("gReg = \"{0}\"; gSimX = {1}; gSimY = {2}; monosucks", regionName, simX, simY));
            }
            else
            {
                map.Document.InvokeScript("gotoRegion", new object[] { regionName, simX, simY });
            }
        }

        #endregion

        internal void Show(object target)
        {
            if (target is CycNart)
            {
                CycNart nart = target as CycNart;
                var i = SimCyclifier.cycAccess.converseInt(CycList.list(new CycSymbol("NART-ID"), nart));
                ShowURL("cb-cf&nart" + i);
            }
            else if (target is CycConstant)
            {
                CycConstant constant = target as CycConstant;
                var i = SimCyclifier.cycAccess.converseInt("(CONSTANT-INTERNAL-ID " + constant.cyclifyWithEscapeChars() + ")");
                ShowURL("cb-cf&c" + i);
            }
            else if (target is CycAssertion)
            {
                CycAssertion constant = target as CycAssertion;
                int i = constant.hashCode();
                ShowURL("cb-cf&c" + i);
            }
        }

        private void ShowURL(string s)
        {
            map.Navigate(string.Format("{0}|{1}", cycURL, s));
        }
    }
}
