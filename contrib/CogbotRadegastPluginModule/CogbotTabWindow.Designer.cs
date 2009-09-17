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
// $Id: ConferenceIMTabWindow.Designer.cs 42 2009-06-14 12:51:18Z latifer $
//
using Radegast;

namespace CogbotRadegastPluginModule
{
    partial class CogbotTabWindow
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CogbotTabWindow));
            this.rtbChat = new System.Windows.Forms.RichTextBox();
            this.cbxInput = new System.Windows.Forms.ComboBox();
            this.btnSay = new System.Windows.Forms.Button();
            this.btnShout = new System.Windows.Forms.Button();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.lvwObjects = new System.Windows.Forms.ListView();
            this.ExtraContextMenu = new RadegastContextMenuStrip(this.components);
            this.avatarToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxAnim = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxProfile = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxPay = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxMaster = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxStartIM = new System.Windows.Forms.ToolStripMenuItem();
            this.setCameraToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.avatarToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxSource = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxTextures = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxFollow = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxAttach = new System.Windows.Forms.ToolStripMenuItem();
            this.ctxPoint = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem33 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem34 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem35 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem36 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem37 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem38 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem39 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem40 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem41 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem44 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem45 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem46 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem49 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem43 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem71 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem42 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.tbtnStartIM = new System.Windows.Forms.ToolStripButton();
            this.tbtnProfile = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbtnFollow = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.tbtnTextures = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.tbtnAttach = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
            this.tbtnMaster = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator5 = new System.Windows.Forms.ToolStripSeparator();
            this.tbtnAnim = new System.Windows.Forms.ToolStripButton();
            this.pnlMovement = new System.Windows.Forms.Panel();
            this.btnMoveBack = new System.Windows.Forms.Button();
            this.btnFwd = new System.Windows.Forms.Button();
            this.btnTurnRight = new System.Windows.Forms.Button();
            this.btnTurnLeft = new System.Windows.Forms.Button();
            this.panel1 = new System.Windows.Forms.Panel();
            this.folderToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.ContextToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.scriptToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.notecardToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.textureToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.gestureToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.simTypeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.objectToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.rezToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.lSLTextToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.gestureToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.notecardToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.textureToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.callingCardToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.soundToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.animationToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem2 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem3 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem4 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem5 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem6 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem7 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem8 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem9 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem10 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem11 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem12 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem13 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem14 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem15 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem16 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem17 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem18 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem19 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem20 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem21 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem22 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem23 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem24 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem25 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem26 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem27 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem28 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem29 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem30 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem31 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem32 = new System.Windows.Forms.ToolStripMenuItem();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.ExtraContextMenu.SuspendLayout();
            this.toolStrip1.SuspendLayout();
            this.pnlMovement.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // rtbChat
            // 
            this.rtbChat.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.rtbChat.BackColor = System.Drawing.Color.White;
            this.rtbChat.HideSelection = false;
            this.rtbChat.Location = new System.Drawing.Point(3, 0);
            this.rtbChat.Name = "rtbChat";
            this.rtbChat.ReadOnly = true;
            this.rtbChat.ScrollBars = System.Windows.Forms.RichTextBoxScrollBars.ForcedBoth;
            this.rtbChat.ShowSelectionMargin = true;
            this.rtbChat.Size = new System.Drawing.Size(400, 310);
            this.rtbChat.TabIndex = 5;
            this.rtbChat.Text = "";
            this.rtbChat.LinkClicked += new System.Windows.Forms.LinkClickedEventHandler(this.rtbChat_LinkClicked);
            // 
            // cbxInput
            // 
            this.cbxInput.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.cbxInput.Enabled = false;
            this.cbxInput.FormattingEnabled = true;
            this.cbxInput.Location = new System.Drawing.Point(0, 0);
            this.cbxInput.Name = "cbxInput";
            this.cbxInput.Size = new System.Drawing.Size(352, 21);
            this.cbxInput.TabIndex = 3;
            this.cbxInput.KeyUp += new System.Windows.Forms.KeyEventHandler(this.cbxInput_KeyUp);
            this.cbxInput.KeyDown += new System.Windows.Forms.KeyEventHandler(this.cbxInput_KeyDown);
            this.cbxInput.TextChanged += new System.EventHandler(this.cbxInput_TextChanged);
            // 
            // btnSay
            // 
            this.btnSay.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnSay.Enabled = false;
            this.btnSay.Location = new System.Drawing.Point(358, 0);
            this.btnSay.Name = "btnSay";
            this.btnSay.Size = new System.Drawing.Size(76, 24);
            this.btnSay.TabIndex = 5;
            this.btnSay.Text = "Execute";
            this.btnSay.UseVisualStyleBackColor = true;
            this.btnSay.Click += new System.EventHandler(this.btnSay_Click);
            // 
            // btnShout
            // 
            this.btnShout.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnShout.Enabled = false;
            this.btnShout.Location = new System.Drawing.Point(440, 0);
            this.btnShout.Name = "btnShout";
            this.btnShout.Size = new System.Drawing.Size(76, 24);
            this.btnShout.TabIndex = 4;
            this.btnShout.Text = "Shout";
            this.btnShout.UseVisualStyleBackColor = true;
            this.btnShout.Click += new System.EventHandler(this.btnShout_Click);
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.rtbChat);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.lvwObjects);
            this.splitContainer1.Panel2.Controls.Add(this.toolStrip1);
            this.splitContainer1.Panel2.Controls.Add(this.pnlMovement);
            this.splitContainer1.Size = new System.Drawing.Size(516, 310);
            this.splitContainer1.SplitterDistance = 400;
            this.splitContainer1.TabIndex = 7;
            // 
            // lvwObjects
            // 
            this.lvwObjects.AllowDrop = true;
            this.lvwObjects.ContextMenuStrip = this.ExtraContextMenu;
            this.lvwObjects.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lvwObjects.FullRowSelect = true;
            this.lvwObjects.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None;
            this.lvwObjects.HideSelection = false;
            this.lvwObjects.LabelWrap = false;
            this.lvwObjects.Location = new System.Drawing.Point(0, 0);
            this.lvwObjects.MultiSelect = false;
            this.lvwObjects.Name = "lvwObjects";
            this.lvwObjects.Size = new System.Drawing.Size(77, 273);
            this.lvwObjects.TabIndex = 10;
            this.lvwObjects.UseCompatibleStateImageBehavior = false;
            this.lvwObjects.View = System.Windows.Forms.View.List;
            this.lvwObjects.SelectedIndexChanged += new System.EventHandler(this.lvwObjects_SelectedIndexChanged);
            this.lvwObjects.DragDrop += new System.Windows.Forms.DragEventHandler(this.lvwObjects_DragDrop);
            this.lvwObjects.DragOver += new System.Windows.Forms.DragEventHandler(this.lvwObjects_DragOver);
            // 
            // ExtraContextMenu
            // 
            this.ExtraContextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.avatarToolStripMenuItem,
            this.avatarToolStripMenuItem1,
            this.toolStripMenuItem33,
            this.toolStripMenuItem40,
            this.toolStripMenuItem44,
            this.toolStripMenuItem45,
            this.toolStripMenuItem46,
            this.toolStripMenuItem49,
            this.toolStripMenuItem43,
            this.toolStripMenuItem71,
            this.toolStripMenuItem42});
            this.ExtraContextMenu.Name = "avatarContext";
            this.ExtraContextMenu.Size = new System.Drawing.Size(153, 268);
            this.ExtraContextMenu.Opening += new System.ComponentModel.CancelEventHandler(this.avatarContext_Opening);
            // 
            // avatarToolStripMenuItem
            // 
            this.avatarToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ctxAnim,
            this.ctxProfile,
            this.ctxPay,
            this.ctxMaster,
            this.ctxStartIM,
            this.setCameraToolStripMenuItem});
            this.avatarToolStripMenuItem.Name = "avatarToolStripMenuItem";
            this.avatarToolStripMenuItem.Size = new System.Drawing.Size(152, 22);
            this.avatarToolStripMenuItem.Text = "Avatar";
            // 
            // ctxAnim
            // 
            this.ctxAnim.Name = "ctxAnim";
            this.ctxAnim.Size = new System.Drawing.Size(159, 22);
            this.ctxAnim.Text = "Animations";
            this.ctxAnim.Click += new System.EventHandler(this.ctxAnim_Click);
            // 
            // ctxProfile
            // 
            this.ctxProfile.Name = "ctxProfile";
            this.ctxProfile.Size = new System.Drawing.Size(159, 22);
            this.ctxProfile.Text = "Profile";
            // 
            // ctxPay
            // 
            this.ctxPay.Enabled = false;
            this.ctxPay.Name = "ctxPay";
            this.ctxPay.Size = new System.Drawing.Size(159, 22);
            this.ctxPay.Text = "Pay";
            // 
            // ctxMaster
            // 
            this.ctxMaster.Name = "ctxMaster";
            this.ctxMaster.Size = new System.Drawing.Size(159, 22);
            this.ctxMaster.Text = "Master controls";
            // 
            // ctxStartIM
            // 
            this.ctxStartIM.Name = "ctxStartIM";
            this.ctxStartIM.Size = new System.Drawing.Size(159, 22);
            this.ctxStartIM.Text = "Start IM";
            // 
            // setCameraToolStripMenuItem
            // 
            this.setCameraToolStripMenuItem.Name = "setCameraToolStripMenuItem";
            this.setCameraToolStripMenuItem.Size = new System.Drawing.Size(159, 22);
            this.setCameraToolStripMenuItem.Text = "Set Camera";
            this.setCameraToolStripMenuItem.Click += new System.EventHandler(this.setCameraToolStripMenuItem_Click);
            // 
            // avatarToolStripMenuItem1
            // 
            this.avatarToolStripMenuItem1.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ctxSource,
            this.ctxTextures,
            this.ctxFollow,
            this.ctxAttach,
            this.ctxPoint});
            this.avatarToolStripMenuItem1.Name = "avatarToolStripMenuItem1";
            this.avatarToolStripMenuItem1.Size = new System.Drawing.Size(152, 22);
            this.avatarToolStripMenuItem1.Text = "Primitive";
            // 
            // ctxSource
            // 
            this.ctxSource.Name = "ctxSource";
            this.ctxSource.Size = new System.Drawing.Size(150, 22);
            this.ctxSource.Text = "Set as source";
            // 
            // ctxTextures
            // 
            this.ctxTextures.Name = "ctxTextures";
            this.ctxTextures.Size = new System.Drawing.Size(150, 22);
            this.ctxTextures.Text = "Textures";
            // 
            // ctxFollow
            // 
            this.ctxFollow.Name = "ctxFollow";
            this.ctxFollow.Size = new System.Drawing.Size(150, 22);
            this.ctxFollow.Text = "Follow";
            // 
            // ctxAttach
            // 
            this.ctxAttach.Name = "ctxAttach";
            this.ctxAttach.Size = new System.Drawing.Size(150, 22);
            this.ctxAttach.Text = "Attachments";
            // 
            // ctxPoint
            // 
            this.ctxPoint.Name = "ctxPoint";
            this.ctxPoint.Size = new System.Drawing.Size(150, 22);
            this.ctxPoint.Text = "Point at";
            this.ctxPoint.Click += new System.EventHandler(this.ctxPoint_Click);
            // 
            // toolStripMenuItem33
            // 
            this.toolStripMenuItem33.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem34});
            this.toolStripMenuItem33.Name = "toolStripMenuItem33";
            this.toolStripMenuItem33.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem33.Text = "Folder";
            // 
            // toolStripMenuItem34
            // 
            this.toolStripMenuItem34.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem35,
            this.toolStripMenuItem36,
            this.toolStripMenuItem37,
            this.toolStripMenuItem38,
            this.toolStripMenuItem39});
            this.toolStripMenuItem34.Name = "toolStripMenuItem34";
            this.toolStripMenuItem34.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem34.Text = "New Item...";
            // 
            // toolStripMenuItem35
            // 
            this.toolStripMenuItem35.Name = "toolStripMenuItem35";
            this.toolStripMenuItem35.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem35.Text = "LSLText";
            // 
            // toolStripMenuItem36
            // 
            this.toolStripMenuItem36.Name = "toolStripMenuItem36";
            this.toolStripMenuItem36.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem36.Text = "Notecard";
            // 
            // toolStripMenuItem37
            // 
            this.toolStripMenuItem37.Name = "toolStripMenuItem37";
            this.toolStripMenuItem37.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem37.Text = "Texture";
            // 
            // toolStripMenuItem38
            // 
            this.toolStripMenuItem38.Name = "toolStripMenuItem38";
            this.toolStripMenuItem38.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem38.Text = "Gesture";
            // 
            // toolStripMenuItem39
            // 
            this.toolStripMenuItem39.Name = "toolStripMenuItem39";
            this.toolStripMenuItem39.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem39.Text = "SimType";
            // 
            // toolStripMenuItem40
            // 
            this.toolStripMenuItem40.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem41});
            this.toolStripMenuItem40.Name = "toolStripMenuItem40";
            this.toolStripMenuItem40.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem40.Text = "Object";
            // 
            // toolStripMenuItem41
            // 
            this.toolStripMenuItem41.Name = "toolStripMenuItem41";
            this.toolStripMenuItem41.Size = new System.Drawing.Size(111, 22);
            this.toolStripMenuItem41.Text = "Rez..";
            // 
            // toolStripMenuItem44
            // 
            this.toolStripMenuItem44.Name = "toolStripMenuItem44";
            this.toolStripMenuItem44.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem44.Text = "Notecard";
            // 
            // toolStripMenuItem45
            // 
            this.toolStripMenuItem45.Name = "toolStripMenuItem45";
            this.toolStripMenuItem45.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem45.Text = "Texture";
            // 
            // toolStripMenuItem46
            // 
            this.toolStripMenuItem46.Name = "toolStripMenuItem46";
            this.toolStripMenuItem46.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem46.Text = "CallingCard";
            // 
            // toolStripMenuItem49
            // 
            this.toolStripMenuItem49.Name = "toolStripMenuItem49";
            this.toolStripMenuItem49.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem49.Text = "Sound";
            // 
            // toolStripMenuItem43
            // 
            this.toolStripMenuItem43.Name = "toolStripMenuItem43";
            this.toolStripMenuItem43.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem43.Text = "Animation";
            // 
            // toolStripMenuItem71
            // 
            this.toolStripMenuItem71.Name = "toolStripMenuItem71";
            this.toolStripMenuItem71.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem71.Text = "Gesture";
            // 
            // toolStripMenuItem42
            // 
            this.toolStripMenuItem42.Name = "toolStripMenuItem42";
            this.toolStripMenuItem42.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem42.Text = "LSLText";
            // 
            // toolStrip1
            // 
            this.toolStrip1.Dock = System.Windows.Forms.DockStyle.Right;
            this.toolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.tbtnStartIM,
            this.tbtnProfile,
            this.toolStripSeparator1,
            this.tbtnFollow,
            this.toolStripSeparator2,
            this.tbtnTextures,
            this.toolStripSeparator3,
            this.tbtnAttach,
            this.toolStripSeparator4,
            this.tbtnMaster,
            this.toolStripSeparator5,
            this.tbtnAnim});
            this.toolStrip1.Location = new System.Drawing.Point(77, 0);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.System;
            this.toolStrip1.Size = new System.Drawing.Size(35, 273);
            this.toolStrip1.TabIndex = 9;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // tbtnStartIM
            // 
            this.tbtnStartIM.AutoToolTip = false;
            this.tbtnStartIM.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbtnStartIM.Enabled = false;
            this.tbtnStartIM.Image = ((System.Drawing.Image)(resources.GetObject("tbtnStartIM.Image")));
            this.tbtnStartIM.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtnStartIM.Name = "tbtnStartIM";
            this.tbtnStartIM.Size = new System.Drawing.Size(32, 20);
            this.tbtnStartIM.ToolTipText = "Start IM";
            this.tbtnStartIM.Click += new System.EventHandler(this.tbtnStartIM_Click);
            // 
            // tbtnProfile
            // 
            this.tbtnProfile.AutoToolTip = false;
            this.tbtnProfile.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbtnProfile.Enabled = false;
            this.tbtnProfile.Image = ((System.Drawing.Image)(resources.GetObject("tbtnProfile.Image")));
            this.tbtnProfile.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtnProfile.Name = "tbtnProfile";
            this.tbtnProfile.Size = new System.Drawing.Size(32, 20);
            this.tbtnProfile.ToolTipText = "View Profile";
            this.tbtnProfile.Click += new System.EventHandler(this.tbtnProfile_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(32, 6);
            // 
            // tbtnFollow
            // 
            this.tbtnFollow.AutoToolTip = false;
            this.tbtnFollow.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbtnFollow.Enabled = false;
            this.tbtnFollow.Image = ((System.Drawing.Image)(resources.GetObject("tbtnFollow.Image")));
            this.tbtnFollow.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtnFollow.Name = "tbtnFollow";
            this.tbtnFollow.Size = new System.Drawing.Size(32, 20);
            this.tbtnFollow.ToolTipText = "Follow";
            this.tbtnFollow.Click += new System.EventHandler(this.tbtnFollow_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(32, 6);
            // 
            // tbtnTextures
            // 
            this.tbtnTextures.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbtnTextures.Enabled = false;
            this.tbtnTextures.Image = ((System.Drawing.Image)(resources.GetObject("tbtnTextures.Image")));
            this.tbtnTextures.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtnTextures.Name = "tbtnTextures";
            this.tbtnTextures.Size = new System.Drawing.Size(32, 17);
            this.tbtnTextures.Text = "Txtr";
            this.tbtnTextures.Click += new System.EventHandler(this.dumpOufitBtn_Click);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(32, 6);
            // 
            // tbtnAttach
            // 
            this.tbtnAttach.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbtnAttach.Enabled = false;
            this.tbtnAttach.Image = ((System.Drawing.Image)(resources.GetObject("tbtnAttach.Image")));
            this.tbtnAttach.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtnAttach.Name = "tbtnAttach";
            this.tbtnAttach.Size = new System.Drawing.Size(32, 17);
            this.tbtnAttach.Text = "Attn";
            this.tbtnAttach.ToolTipText = "List avatar attachments";
            this.tbtnAttach.Click += new System.EventHandler(this.tbtnAttach_Click);
            // 
            // toolStripSeparator4
            // 
            this.toolStripSeparator4.Name = "toolStripSeparator4";
            this.toolStripSeparator4.Size = new System.Drawing.Size(32, 6);
            // 
            // tbtnMaster
            // 
            this.tbtnMaster.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbtnMaster.Enabled = false;
            this.tbtnMaster.Image = ((System.Drawing.Image)(resources.GetObject("tbtnMaster.Image")));
            this.tbtnMaster.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtnMaster.Name = "tbtnMaster";
            this.tbtnMaster.Size = new System.Drawing.Size(32, 17);
            this.tbtnMaster.Text = "Mstr";
            this.tbtnMaster.Click += new System.EventHandler(this.tbtnMaster_Click);
            // 
            // toolStripSeparator5
            // 
            this.toolStripSeparator5.Name = "toolStripSeparator5";
            this.toolStripSeparator5.Size = new System.Drawing.Size(32, 6);
            // 
            // tbtnAnim
            // 
            this.tbtnAnim.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbtnAnim.Enabled = false;
            this.tbtnAnim.Image = ((System.Drawing.Image)(resources.GetObject("tbtnAnim.Image")));
            this.tbtnAnim.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtnAnim.Name = "tbtnAnim";
            this.tbtnAnim.Size = new System.Drawing.Size(32, 17);
            this.tbtnAnim.Text = "Anim";
            this.tbtnAnim.ToolTipText = "List Avatar Animatoions";
            this.tbtnAnim.Click += new System.EventHandler(this.tbtnAnim_Click);
            // 
            // pnlMovement
            // 
            this.pnlMovement.Controls.Add(this.btnMoveBack);
            this.pnlMovement.Controls.Add(this.btnFwd);
            this.pnlMovement.Controls.Add(this.btnTurnRight);
            this.pnlMovement.Controls.Add(this.btnTurnLeft);
            this.pnlMovement.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.pnlMovement.Location = new System.Drawing.Point(0, 273);
            this.pnlMovement.Name = "pnlMovement";
            this.pnlMovement.Size = new System.Drawing.Size(112, 37);
            this.pnlMovement.TabIndex = 11;
            this.pnlMovement.Click += new System.EventHandler(this.pnlMovement_Click);
            // 
            // btnMoveBack
            // 
            this.btnMoveBack.Font = new System.Drawing.Font("Tahoma", 6F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnMoveBack.Location = new System.Drawing.Point(36, 15);
            this.btnMoveBack.Margin = new System.Windows.Forms.Padding(0);
            this.btnMoveBack.Name = "btnMoveBack";
            this.btnMoveBack.Size = new System.Drawing.Size(31, 19);
            this.btnMoveBack.TabIndex = 0;
            this.btnMoveBack.Text = "R";
            this.btnMoveBack.UseVisualStyleBackColor = true;
            this.btnMoveBack.MouseDown += new System.Windows.Forms.MouseEventHandler(this.btnMoveBack_MouseDown);
            this.btnMoveBack.MouseUp += new System.Windows.Forms.MouseEventHandler(this.btnMoveBack_MouseUp);
            // 
            // btnFwd
            // 
            this.btnFwd.Font = new System.Drawing.Font("Tahoma", 6F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnFwd.Location = new System.Drawing.Point(36, 0);
            this.btnFwd.Margin = new System.Windows.Forms.Padding(0);
            this.btnFwd.Name = "btnFwd";
            this.btnFwd.Size = new System.Drawing.Size(31, 19);
            this.btnFwd.TabIndex = 0;
            this.btnFwd.Text = "^";
            this.btnFwd.UseVisualStyleBackColor = true;
            this.btnFwd.MouseDown += new System.Windows.Forms.MouseEventHandler(this.btnFwd_MouseDown);
            this.btnFwd.MouseUp += new System.Windows.Forms.MouseEventHandler(this.btnFwd_MouseUp);
            // 
            // btnTurnRight
            // 
            this.btnTurnRight.Font = new System.Drawing.Font("Tahoma", 6F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnTurnRight.Location = new System.Drawing.Point(67, 15);
            this.btnTurnRight.Margin = new System.Windows.Forms.Padding(0);
            this.btnTurnRight.Name = "btnTurnRight";
            this.btnTurnRight.Size = new System.Drawing.Size(31, 19);
            this.btnTurnRight.TabIndex = 0;
            this.btnTurnRight.Text = ">>";
            this.btnTurnRight.UseVisualStyleBackColor = true;
            this.btnTurnRight.MouseDown += new System.Windows.Forms.MouseEventHandler(this.btnTurnRight_MouseDown);
            this.btnTurnRight.MouseUp += new System.Windows.Forms.MouseEventHandler(this.btnTurnRight_MouseUp);
            // 
            // btnTurnLeft
            // 
            this.btnTurnLeft.Font = new System.Drawing.Font("Tahoma", 6F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnTurnLeft.Location = new System.Drawing.Point(5, 15);
            this.btnTurnLeft.Margin = new System.Windows.Forms.Padding(0);
            this.btnTurnLeft.Name = "btnTurnLeft";
            this.btnTurnLeft.Size = new System.Drawing.Size(31, 19);
            this.btnTurnLeft.TabIndex = 0;
            this.btnTurnLeft.Text = "<<";
            this.btnTurnLeft.UseVisualStyleBackColor = true;
            this.btnTurnLeft.MouseDown += new System.Windows.Forms.MouseEventHandler(this.btnTurnLeft_MouseDown);
            this.btnTurnLeft.MouseUp += new System.Windows.Forms.MouseEventHandler(this.btnTurnLeft_MouseUp);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.cbxInput);
            this.panel1.Controls.Add(this.btnSay);
            this.panel1.Controls.Add(this.btnShout);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(0, 310);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(516, 24);
            this.panel1.TabIndex = 8;
            // 
            // folderToolStripMenuItem
            // 
            this.folderToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ContextToolStripMenuItem});
            this.folderToolStripMenuItem.Name = "folderToolStripMenuItem";
            this.folderToolStripMenuItem.Size = new System.Drawing.Size(139, 22);
            this.folderToolStripMenuItem.Text = "Folder";
            // 
            // ContextToolStripMenuItem
            // 
            this.ContextToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.scriptToolStripMenuItem,
            this.notecardToolStripMenuItem,
            this.textureToolStripMenuItem,
            this.gestureToolStripMenuItem,
            this.simTypeToolStripMenuItem});
            this.ContextToolStripMenuItem.Name = "ContextToolStripMenuItem";
            this.ContextToolStripMenuItem.Size = new System.Drawing.Size(118, 22);
            this.ContextToolStripMenuItem.Text = "New...";
            // 
            // scriptToolStripMenuItem
            // 
            this.scriptToolStripMenuItem.Name = "scriptToolStripMenuItem";
            this.scriptToolStripMenuItem.Size = new System.Drawing.Size(129, 22);
            this.scriptToolStripMenuItem.Text = "LSLText";
            // 
            // notecardToolStripMenuItem
            // 
            this.notecardToolStripMenuItem.Name = "notecardToolStripMenuItem";
            this.notecardToolStripMenuItem.Size = new System.Drawing.Size(129, 22);
            this.notecardToolStripMenuItem.Text = "Notecard";
            // 
            // textureToolStripMenuItem
            // 
            this.textureToolStripMenuItem.Name = "textureToolStripMenuItem";
            this.textureToolStripMenuItem.Size = new System.Drawing.Size(129, 22);
            this.textureToolStripMenuItem.Text = "Texture";
            // 
            // gestureToolStripMenuItem
            // 
            this.gestureToolStripMenuItem.Name = "gestureToolStripMenuItem";
            this.gestureToolStripMenuItem.Size = new System.Drawing.Size(129, 22);
            this.gestureToolStripMenuItem.Text = "Gesture";
            // 
            // simTypeToolStripMenuItem
            // 
            this.simTypeToolStripMenuItem.Name = "simTypeToolStripMenuItem";
            this.simTypeToolStripMenuItem.Size = new System.Drawing.Size(129, 22);
            this.simTypeToolStripMenuItem.Text = "SimType";
            // 
            // objectToolStripMenuItem
            // 
            this.objectToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.rezToolStripMenuItem});
            this.objectToolStripMenuItem.Name = "objectToolStripMenuItem";
            this.objectToolStripMenuItem.Size = new System.Drawing.Size(139, 22);
            this.objectToolStripMenuItem.Text = "Object";
            // 
            // rezToolStripMenuItem
            // 
            this.rezToolStripMenuItem.Name = "rezToolStripMenuItem";
            this.rezToolStripMenuItem.Size = new System.Drawing.Size(111, 22);
            this.rezToolStripMenuItem.Text = "Rez..";
            // 
            // lSLTextToolStripMenuItem
            // 
            this.lSLTextToolStripMenuItem.Name = "lSLTextToolStripMenuItem";
            this.lSLTextToolStripMenuItem.Size = new System.Drawing.Size(139, 22);
            this.lSLTextToolStripMenuItem.Text = "LSLText";
            // 
            // gestureToolStripMenuItem1
            // 
            this.gestureToolStripMenuItem1.Name = "gestureToolStripMenuItem1";
            this.gestureToolStripMenuItem1.Size = new System.Drawing.Size(139, 22);
            this.gestureToolStripMenuItem1.Text = "Gesture";
            // 
            // notecardToolStripMenuItem1
            // 
            this.notecardToolStripMenuItem1.Name = "notecardToolStripMenuItem1";
            this.notecardToolStripMenuItem1.Size = new System.Drawing.Size(139, 22);
            this.notecardToolStripMenuItem1.Text = "Notecard";
            // 
            // textureToolStripMenuItem1
            // 
            this.textureToolStripMenuItem1.Name = "textureToolStripMenuItem1";
            this.textureToolStripMenuItem1.Size = new System.Drawing.Size(139, 22);
            this.textureToolStripMenuItem1.Text = "Texture";
            // 
            // callingCardToolStripMenuItem
            // 
            this.callingCardToolStripMenuItem.Name = "callingCardToolStripMenuItem";
            this.callingCardToolStripMenuItem.Size = new System.Drawing.Size(139, 22);
            this.callingCardToolStripMenuItem.Text = "CallingCard";
            // 
            // soundToolStripMenuItem
            // 
            this.soundToolStripMenuItem.Name = "soundToolStripMenuItem";
            this.soundToolStripMenuItem.Size = new System.Drawing.Size(139, 22);
            this.soundToolStripMenuItem.Text = "Sound";
            // 
            // animationToolStripMenuItem
            // 
            this.animationToolStripMenuItem.Name = "animationToolStripMenuItem";
            this.animationToolStripMenuItem.Size = new System.Drawing.Size(139, 22);
            this.animationToolStripMenuItem.Text = "Animation";
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem2});
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem1.Text = "Folder";
            // 
            // toolStripMenuItem2
            // 
            this.toolStripMenuItem2.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem3,
            this.toolStripMenuItem4,
            this.toolStripMenuItem5,
            this.toolStripMenuItem6,
            this.toolStripMenuItem7});
            this.toolStripMenuItem2.Name = "toolStripMenuItem2";
            this.toolStripMenuItem2.Size = new System.Drawing.Size(118, 22);
            this.toolStripMenuItem2.Text = "New...";
            // 
            // toolStripMenuItem3
            // 
            this.toolStripMenuItem3.Name = "toolStripMenuItem3";
            this.toolStripMenuItem3.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem3.Text = "LSLText";
            // 
            // toolStripMenuItem4
            // 
            this.toolStripMenuItem4.Name = "toolStripMenuItem4";
            this.toolStripMenuItem4.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem4.Text = "Notecard";
            // 
            // toolStripMenuItem5
            // 
            this.toolStripMenuItem5.Name = "toolStripMenuItem5";
            this.toolStripMenuItem5.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem5.Text = "Texture";
            // 
            // toolStripMenuItem6
            // 
            this.toolStripMenuItem6.Name = "toolStripMenuItem6";
            this.toolStripMenuItem6.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem6.Text = "Gesture";
            // 
            // toolStripMenuItem7
            // 
            this.toolStripMenuItem7.Name = "toolStripMenuItem7";
            this.toolStripMenuItem7.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem7.Text = "SimType";
            // 
            // toolStripMenuItem8
            // 
            this.toolStripMenuItem8.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem9});
            this.toolStripMenuItem8.Name = "toolStripMenuItem8";
            this.toolStripMenuItem8.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem8.Text = "Object";
            // 
            // toolStripMenuItem9
            // 
            this.toolStripMenuItem9.Name = "toolStripMenuItem9";
            this.toolStripMenuItem9.Size = new System.Drawing.Size(111, 22);
            this.toolStripMenuItem9.Text = "Rez..";
            // 
            // toolStripMenuItem10
            // 
            this.toolStripMenuItem10.Name = "toolStripMenuItem10";
            this.toolStripMenuItem10.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem10.Text = "LSLText";
            // 
            // toolStripMenuItem11
            // 
            this.toolStripMenuItem11.Name = "toolStripMenuItem11";
            this.toolStripMenuItem11.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem11.Text = "Gesture";
            // 
            // toolStripMenuItem12
            // 
            this.toolStripMenuItem12.Name = "toolStripMenuItem12";
            this.toolStripMenuItem12.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem12.Text = "Notecard";
            // 
            // toolStripMenuItem13
            // 
            this.toolStripMenuItem13.Name = "toolStripMenuItem13";
            this.toolStripMenuItem13.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem13.Text = "Texture";
            // 
            // toolStripMenuItem14
            // 
            this.toolStripMenuItem14.Name = "toolStripMenuItem14";
            this.toolStripMenuItem14.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem14.Text = "CallingCard";
            // 
            // toolStripMenuItem15
            // 
            this.toolStripMenuItem15.Name = "toolStripMenuItem15";
            this.toolStripMenuItem15.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem15.Text = "Sound";
            // 
            // toolStripMenuItem16
            // 
            this.toolStripMenuItem16.Name = "toolStripMenuItem16";
            this.toolStripMenuItem16.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem16.Text = "Animation";
            // 
            // toolStripMenuItem17
            // 
            this.toolStripMenuItem17.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem18});
            this.toolStripMenuItem17.Name = "toolStripMenuItem17";
            this.toolStripMenuItem17.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem17.Text = "Folder";
            // 
            // toolStripMenuItem18
            // 
            this.toolStripMenuItem18.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem19,
            this.toolStripMenuItem20,
            this.toolStripMenuItem21,
            this.toolStripMenuItem22,
            this.toolStripMenuItem23});
            this.toolStripMenuItem18.Name = "toolStripMenuItem18";
            this.toolStripMenuItem18.Size = new System.Drawing.Size(118, 22);
            this.toolStripMenuItem18.Text = "New...";
            // 
            // toolStripMenuItem19
            // 
            this.toolStripMenuItem19.Name = "toolStripMenuItem19";
            this.toolStripMenuItem19.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem19.Text = "LSLText";
            // 
            // toolStripMenuItem20
            // 
            this.toolStripMenuItem20.Name = "toolStripMenuItem20";
            this.toolStripMenuItem20.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem20.Text = "Notecard";
            // 
            // toolStripMenuItem21
            // 
            this.toolStripMenuItem21.Name = "toolStripMenuItem21";
            this.toolStripMenuItem21.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem21.Text = "Texture";
            // 
            // toolStripMenuItem22
            // 
            this.toolStripMenuItem22.Name = "toolStripMenuItem22";
            this.toolStripMenuItem22.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem22.Text = "Gesture";
            // 
            // toolStripMenuItem23
            // 
            this.toolStripMenuItem23.Name = "toolStripMenuItem23";
            this.toolStripMenuItem23.Size = new System.Drawing.Size(129, 22);
            this.toolStripMenuItem23.Text = "SimType";
            // 
            // toolStripMenuItem24
            // 
            this.toolStripMenuItem24.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem25});
            this.toolStripMenuItem24.Name = "toolStripMenuItem24";
            this.toolStripMenuItem24.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem24.Text = "Object";
            // 
            // toolStripMenuItem25
            // 
            this.toolStripMenuItem25.Name = "toolStripMenuItem25";
            this.toolStripMenuItem25.Size = new System.Drawing.Size(111, 22);
            this.toolStripMenuItem25.Text = "Rez..";
            // 
            // toolStripMenuItem26
            // 
            this.toolStripMenuItem26.Name = "toolStripMenuItem26";
            this.toolStripMenuItem26.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem26.Text = "LSLText";
            // 
            // toolStripMenuItem27
            // 
            this.toolStripMenuItem27.Name = "toolStripMenuItem27";
            this.toolStripMenuItem27.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem27.Text = "Gesture";
            // 
            // toolStripMenuItem28
            // 
            this.toolStripMenuItem28.Name = "toolStripMenuItem28";
            this.toolStripMenuItem28.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem28.Text = "Notecard";
            // 
            // toolStripMenuItem29
            // 
            this.toolStripMenuItem29.Name = "toolStripMenuItem29";
            this.toolStripMenuItem29.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem29.Text = "Texture";
            // 
            // toolStripMenuItem30
            // 
            this.toolStripMenuItem30.Name = "toolStripMenuItem30";
            this.toolStripMenuItem30.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem30.Text = "CallingCard";
            // 
            // toolStripMenuItem31
            // 
            this.toolStripMenuItem31.Name = "toolStripMenuItem31";
            this.toolStripMenuItem31.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem31.Text = "Sound";
            // 
            // toolStripMenuItem32
            // 
            this.toolStripMenuItem32.Name = "toolStripMenuItem32";
            this.toolStripMenuItem32.Size = new System.Drawing.Size(139, 22);
            this.toolStripMenuItem32.Text = "Animation";
            // 
            // CogbotTabWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.panel1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Name = "CogbotTabWindow";
            this.Size = new System.Drawing.Size(516, 334);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.Panel2.PerformLayout();
            this.splitContainer1.ResumeLayout(false);
            this.ExtraContextMenu.ResumeLayout(false);
            this.toolStrip1.ResumeLayout(false);
            this.toolStrip1.PerformLayout();
            this.pnlMovement.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.RichTextBox rtbChat;
        private System.Windows.Forms.ComboBox cbxInput;
        private System.Windows.Forms.Button btnSay;
        private System.Windows.Forms.Button btnShout;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.ToolStripButton tbtnStartIM;
        private System.Windows.Forms.ToolStripButton tbtnFollow;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ListView lvwObjects;
        private System.Windows.Forms.ToolStripButton tbtnProfile;
        private System.Windows.Forms.ToolStripButton tbtnTextures;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripButton tbtnMaster;
        private System.Windows.Forms.ToolStripButton tbtnAttach;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
        private System.Windows.Forms.Panel pnlMovement;
        private System.Windows.Forms.Button btnTurnLeft;
        private System.Windows.Forms.Button btnTurnRight;
        private System.Windows.Forms.Button btnFwd;
        private System.Windows.Forms.Button btnMoveBack;
        private System.Windows.Forms.ToolStripButton tbtnAnim;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator5;
        private RadegastContextMenuStrip ExtraContextMenu;
        private System.Windows.Forms.ToolStripMenuItem folderToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem ContextToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem scriptToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem notecardToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem textureToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem gestureToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem simTypeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem objectToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem rezToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem lSLTextToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem gestureToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem notecardToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem textureToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem callingCardToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem soundToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem animationToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem2;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem3;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem4;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem5;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem6;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem7;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem8;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem9;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem10;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem11;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem12;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem13;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem14;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem15;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem16;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem17;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem18;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem19;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem20;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem21;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem22;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem23;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem24;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem25;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem26;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem27;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem28;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem29;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem30;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem31;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem32;
        private System.Windows.Forms.ToolStripMenuItem avatarToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem ctxProfile;
        private System.Windows.Forms.ToolStripMenuItem ctxPay;
        private System.Windows.Forms.ToolStripMenuItem ctxMaster;
        private System.Windows.Forms.ToolStripMenuItem ctxStartIM;
        private System.Windows.Forms.ToolStripMenuItem avatarToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem ctxSource;
        private System.Windows.Forms.ToolStripMenuItem ctxTextures;
        private System.Windows.Forms.ToolStripMenuItem ctxFollow;
        private System.Windows.Forms.ToolStripMenuItem ctxAttach;
        private System.Windows.Forms.ToolStripMenuItem ctxPoint;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem33;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem34;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem35;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem36;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem37;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem38;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem39;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem40;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem41;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem44;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem45;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem46;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem49;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem43;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem71;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem42;
        private System.Windows.Forms.ToolStripMenuItem setCameraToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem ctxAnim;
    }
}
