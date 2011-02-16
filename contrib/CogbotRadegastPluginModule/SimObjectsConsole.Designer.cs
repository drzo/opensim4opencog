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
// $Id: SimObjectsConsole.Designer.cs 271 2009-09-27 11:45:39Z latifer@gmail.com $
//
using Radegast;

namespace CogbotRadegastPluginModule
{
    partial class SimObjectsConsole
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SimObjectsConsole));
            this.btnRefresh = new System.Windows.Forms.Button();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.lblStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.gbxInWorld = new System.Windows.Forms.GroupBox();
            this.txtSearch = new System.Windows.Forms.TextBox();
            this.rbName = new System.Windows.Forms.RadioButton();
            this.rbDistance = new System.Windows.Forms.RadioButton();
            this.nudRadius = new System.Windows.Forms.NumericUpDown();
            this.btnClear = new System.Windows.Forms.Button();
            this.lstPrims = new Radegast.ListViewNoFlicker();
            this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
            this.ctxMenuObjects = new Radegast.RadegastContextMenuStrip(this.components);
            this.tabSimObjeks = new System.Windows.Forms.TabControl();
            this.tabSimOjbectDetail = new System.Windows.Forms.TabPage();
            this.grpPrimInfo = new System.Windows.Forms.FlowLayoutPanel();
            this.gbxObjectDetails = new System.Windows.Forms.GroupBox();
            this.chkOwnerMove = new System.Windows.Forms.CheckBox();
            this.cbNextOwnTransfer = new System.Windows.Forms.CheckBox();
            this.cbNextOwnCopy = new System.Windows.Forms.CheckBox();
            this.cbOwnerTransfer = new System.Windows.Forms.CheckBox();
            this.cbNextOwnModify = new System.Windows.Forms.CheckBox();
            this.cbOwnerCopy = new System.Windows.Forms.CheckBox();
            this.cbOwnerModify = new System.Windows.Forms.CheckBox();
            this.txtPrims = new System.Windows.Forms.TextBox();
            this.txtCreator = new Radegast.AgentNameTextBox(instance);
            this.txtOwner = new Radegast.AgentNameTextBox(instance);
            this.txtHover = new System.Windows.Forms.TextBox();
            this.txtDescription = new System.Windows.Forms.TextBox();
            this.txtObjectName = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.lblName = new System.Windows.Forms.Label();
            this.Filters = new System.Windows.Forms.TabPage();
            this.searchOptions = new System.Windows.Forms.FlowLayoutPanel();
            this.statusStrip1.SuspendLayout();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.gbxInWorld.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudRadius)).BeginInit();
            this.tabSimObjeks.SuspendLayout();
            this.tabSimOjbectDetail.SuspendLayout();
            this.grpPrimInfo.SuspendLayout();
            this.gbxObjectDetails.SuspendLayout();
            this.Filters.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnRefresh
            // 
            this.btnRefresh.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnRefresh.Location = new System.Drawing.Point(721, 442);
            this.btnRefresh.Name = "btnRefresh";
            this.btnRefresh.Size = new System.Drawing.Size(100, 23);
            this.btnRefresh.TabIndex = 11;
            this.btnRefresh.Text = "Refresh";
            this.btnRefresh.UseVisualStyleBackColor = true;
            this.btnRefresh.Click += new System.EventHandler(this.btnRefresh_Click);
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.lblStatus});
            this.statusStrip1.Location = new System.Drawing.Point(0, 468);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(833, 22);
            this.statusStrip1.TabIndex = 15;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // lblStatus
            // 
            this.lblStatus.Name = "lblStatus";
            this.lblStatus.Size = new System.Drawing.Size(59, 17);
            this.lblStatus.Text = "Tracking...";
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.gbxInWorld);
            this.splitContainer1.Panel1.Controls.Add(this.lstPrims);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.tabSimObjeks);
            this.splitContainer1.Size = new System.Drawing.Size(833, 468);
            this.splitContainer1.SplitterDistance = 422;
            this.splitContainer1.TabIndex = 19;
            // 
            // gbxInWorld
            // 
            this.gbxInWorld.Controls.Add(this.txtSearch);
            this.gbxInWorld.Controls.Add(this.rbName);
            this.gbxInWorld.Controls.Add(this.rbDistance);
            this.gbxInWorld.Controls.Add(this.nudRadius);
            this.gbxInWorld.Controls.Add(this.btnClear);
            this.gbxInWorld.Dock = System.Windows.Forms.DockStyle.Top;
            this.gbxInWorld.Location = new System.Drawing.Point(0, 0);
            this.gbxInWorld.Name = "gbxInWorld";
            this.gbxInWorld.Size = new System.Drawing.Size(422, 37);
            this.gbxInWorld.TabIndex = 19;
            this.gbxInWorld.TabStop = false;
            this.gbxInWorld.Text = "Sort by";
            // 
            // txtSearch
            // 
            this.txtSearch.Location = new System.Drawing.Point(55, 12);
            this.txtSearch.Name = "txtSearch";
            this.txtSearch.Size = new System.Drawing.Size(158, 21);
            this.txtSearch.TabIndex = 0;
            this.txtSearch.TextChanged += new System.EventHandler(this.txtSearch_TextChanged);
            // 
            // rbName
            // 
            this.rbName.AutoSize = true;
            this.rbName.Location = new System.Drawing.Point(3, 16);
            this.rbName.Name = "rbName";
            this.rbName.Size = new System.Drawing.Size(52, 17);
            this.rbName.TabIndex = 0;
            this.rbName.Text = "Name";
            this.rbName.UseVisualStyleBackColor = true;
            this.rbName.CheckedChanged += new System.EventHandler(this.rbName_CheckedChanged);
            // 
            // rbDistance
            // 
            this.rbDistance.AutoSize = true;
            this.rbDistance.Checked = true;
            this.rbDistance.Location = new System.Drawing.Point(325, 12);
            this.rbDistance.Name = "rbDistance";
            this.rbDistance.Size = new System.Drawing.Size(58, 17);
            this.rbDistance.TabIndex = 0;
            this.rbDistance.TabStop = true;
            this.rbDistance.Text = "meters";
            this.rbDistance.UseVisualStyleBackColor = true;
            this.rbDistance.CheckedChanged += new System.EventHandler(this.rbDistance_CheckedChanged);
            // 
            // nudRadius
            // 
            this.nudRadius.Increment = new decimal(new int[] {
            5,
            0,
            0,
            0});
            this.nudRadius.Location = new System.Drawing.Point(268, 12);
            this.nudRadius.Maximum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
            this.nudRadius.Name = "nudRadius";
            this.nudRadius.Size = new System.Drawing.Size(51, 21);
            this.nudRadius.TabIndex = 3;
            this.nudRadius.ThousandsSeparator = true;
            this.nudRadius.Value = new decimal(new int[] {
            10,
            0,
            0,
            0});
            // 
            // btnClear
            // 
            this.btnClear.Location = new System.Drawing.Point(219, 10);
            this.btnClear.Name = "btnClear";
            this.btnClear.Size = new System.Drawing.Size(43, 23);
            this.btnClear.TabIndex = 2;
            this.btnClear.Text = "Clear";
            this.btnClear.UseVisualStyleBackColor = true;
            // 
            // lstPrims
            // 
            this.lstPrims.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.lstPrims.AutoArrange = false;
            this.lstPrims.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1});
            this.lstPrims.ContextMenuStrip = this.ctxMenuObjects;
            this.lstPrims.FullRowSelect = true;
            this.lstPrims.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None;
            this.lstPrims.HideSelection = false;
            this.lstPrims.LabelWrap = false;
            this.lstPrims.Location = new System.Drawing.Point(-3, 41);
            this.lstPrims.Name = "lstPrims";
            this.lstPrims.ShowGroups = false;
            this.lstPrims.Size = new System.Drawing.Size(422, 429);
            this.lstPrims.TabIndex = 11;
            this.lstPrims.UseCompatibleStateImageBehavior = false;
            this.lstPrims.View = System.Windows.Forms.View.Details;
            this.lstPrims.SelectedIndexChanged += new System.EventHandler(this.lstPrims_SelectedIndexChanged);
            this.lstPrims.ItemSelectionChanged += new System.Windows.Forms.ListViewItemSelectionChangedEventHandler(this.lstPrims_ItemSelectionChanged);
            // 
            // columnHeader1
            // 
            this.columnHeader1.Width = 340;
            // 
            // ctxMenuObjects
            // 
            this.ctxMenuObjects.Name = "ctxMenuObjects";
            this.ctxMenuObjects.Size = new System.Drawing.Size(61, 4);
            this.ctxMenuObjects.Opening += new System.ComponentModel.CancelEventHandler(this.ctxMenuObjects_Opening);
            // 
            // tabSimObjeks
            // 
            this.tabSimObjeks.Controls.Add(this.tabSimOjbectDetail);
            this.tabSimObjeks.Controls.Add(this.Filters);
            this.tabSimObjeks.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabSimObjeks.Location = new System.Drawing.Point(0, 0);
            this.tabSimObjeks.Name = "tabSimObjeks";
            this.tabSimObjeks.SelectedIndex = 0;
            this.tabSimObjeks.Size = new System.Drawing.Size(407, 468);
            this.tabSimObjeks.TabIndex = 0;
            // 
            // tabSimOjbectDetail
            // 
            this.tabSimOjbectDetail.Controls.Add(this.grpPrimInfo);
            this.tabSimOjbectDetail.Location = new System.Drawing.Point(4, 22);
            this.tabSimOjbectDetail.Name = "tabSimOjbectDetail";
            this.tabSimOjbectDetail.Padding = new System.Windows.Forms.Padding(3);
            this.tabSimOjbectDetail.Size = new System.Drawing.Size(399, 442);
            this.tabSimOjbectDetail.TabIndex = 0;
            this.tabSimOjbectDetail.Text = "Object";
            this.tabSimOjbectDetail.UseVisualStyleBackColor = true;
            // 
            // grpPrimInfo
            // 
            this.grpPrimInfo.Controls.Add(this.gbxObjectDetails);
            this.grpPrimInfo.Dock = System.Windows.Forms.DockStyle.Fill;
            this.grpPrimInfo.FlowDirection = System.Windows.Forms.FlowDirection.TopDown;
            this.grpPrimInfo.Location = new System.Drawing.Point(3, 3);
            this.grpPrimInfo.Name = "grpPrimInfo";
            this.grpPrimInfo.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.grpPrimInfo.Size = new System.Drawing.Size(393, 436);
            this.grpPrimInfo.TabIndex = 20;
            this.grpPrimInfo.Text = "Object Info";
            // 
            // gbxObjectDetails
            // 
            this.gbxObjectDetails.Controls.Add(this.chkOwnerMove);
            this.gbxObjectDetails.Controls.Add(this.cbNextOwnTransfer);
            this.gbxObjectDetails.Controls.Add(this.cbNextOwnCopy);
            this.gbxObjectDetails.Controls.Add(this.cbOwnerTransfer);
            this.gbxObjectDetails.Controls.Add(this.cbNextOwnModify);
            this.gbxObjectDetails.Controls.Add(this.cbOwnerCopy);
            this.gbxObjectDetails.Controls.Add(this.cbOwnerModify);
            this.gbxObjectDetails.Controls.Add(this.txtPrims);
            this.gbxObjectDetails.Controls.Add(this.txtCreator);
            this.gbxObjectDetails.Controls.Add(this.txtOwner);
            this.gbxObjectDetails.Controls.Add(this.txtHover);
            this.gbxObjectDetails.Controls.Add(this.txtDescription);
            this.gbxObjectDetails.Controls.Add(this.txtObjectName);
            this.gbxObjectDetails.Controls.Add(this.label8);
            this.gbxObjectDetails.Controls.Add(this.label7);
            this.gbxObjectDetails.Controls.Add(this.label6);
            this.gbxObjectDetails.Controls.Add(this.label5);
            this.gbxObjectDetails.Controls.Add(this.label4);
            this.gbxObjectDetails.Controls.Add(this.label3);
            this.gbxObjectDetails.Controls.Add(this.label2);
            this.gbxObjectDetails.Controls.Add(this.lblName);
            this.gbxObjectDetails.Location = new System.Drawing.Point(3, 3);
            this.gbxObjectDetails.Name = "gbxObjectDetails";
            this.gbxObjectDetails.Size = new System.Drawing.Size(310, 187);
            this.gbxObjectDetails.TabIndex = 18;
            this.gbxObjectDetails.TabStop = false;
            this.gbxObjectDetails.Text = "Object details";
            // 
            // chkOwnerMove
            // 
            this.chkOwnerMove.AutoSize = true;
            this.chkOwnerMove.Location = new System.Drawing.Point(250, 143);
            this.chkOwnerMove.Name = "chkOwnerMove";
            this.chkOwnerMove.Size = new System.Drawing.Size(52, 17);
            this.chkOwnerMove.TabIndex = 7;
            this.chkOwnerMove.Text = "Move";
            this.chkOwnerMove.UseVisualStyleBackColor = true;
            // 
            // cbNextOwnTransfer
            // 
            this.cbNextOwnTransfer.AutoSize = true;
            this.cbNextOwnTransfer.Location = new System.Drawing.Point(195, 164);
            this.cbNextOwnTransfer.Name = "cbNextOwnTransfer";
            this.cbNextOwnTransfer.Size = new System.Drawing.Size(54, 17);
            this.cbNextOwnTransfer.TabIndex = 6;
            this.cbNextOwnTransfer.Text = "Resell";
            this.cbNextOwnTransfer.UseVisualStyleBackColor = true;
            // 
            // cbNextOwnCopy
            // 
            this.cbNextOwnCopy.AutoSize = true;
            this.cbNextOwnCopy.Location = new System.Drawing.Point(140, 164);
            this.cbNextOwnCopy.Name = "cbNextOwnCopy";
            this.cbNextOwnCopy.Size = new System.Drawing.Size(51, 17);
            this.cbNextOwnCopy.TabIndex = 6;
            this.cbNextOwnCopy.Text = "Copy";
            this.cbNextOwnCopy.UseVisualStyleBackColor = true;
            // 
            // cbOwnerTransfer
            // 
            this.cbOwnerTransfer.AutoSize = true;
            this.cbOwnerTransfer.Location = new System.Drawing.Point(195, 143);
            this.cbOwnerTransfer.Name = "cbOwnerTransfer";
            this.cbOwnerTransfer.Size = new System.Drawing.Size(54, 17);
            this.cbOwnerTransfer.TabIndex = 6;
            this.cbOwnerTransfer.Text = "Resell";
            this.cbOwnerTransfer.UseVisualStyleBackColor = true;
            // 
            // cbNextOwnModify
            // 
            this.cbNextOwnModify.AutoSize = true;
            this.cbNextOwnModify.Location = new System.Drawing.Point(90, 164);
            this.cbNextOwnModify.Name = "cbNextOwnModify";
            this.cbNextOwnModify.Size = new System.Drawing.Size(46, 17);
            this.cbNextOwnModify.TabIndex = 6;
            this.cbNextOwnModify.Text = "Mod";
            this.cbNextOwnModify.UseVisualStyleBackColor = true;
            // 
            // cbOwnerCopy
            // 
            this.cbOwnerCopy.AutoSize = true;
            this.cbOwnerCopy.Location = new System.Drawing.Point(140, 143);
            this.cbOwnerCopy.Name = "cbOwnerCopy";
            this.cbOwnerCopy.Size = new System.Drawing.Size(51, 17);
            this.cbOwnerCopy.TabIndex = 6;
            this.cbOwnerCopy.Text = "Copy";
            this.cbOwnerCopy.UseVisualStyleBackColor = true;
            // 
            // cbOwnerModify
            // 
            this.cbOwnerModify.AutoSize = true;
            this.cbOwnerModify.Location = new System.Drawing.Point(90, 143);
            this.cbOwnerModify.Name = "cbOwnerModify";
            this.cbOwnerModify.Size = new System.Drawing.Size(46, 17);
            this.cbOwnerModify.TabIndex = 6;
            this.cbOwnerModify.Text = "Mod";
            this.cbOwnerModify.UseVisualStyleBackColor = true;
            // 
            // txtPrims
            // 
            this.txtPrims.BackColor = System.Drawing.SystemColors.Window;
            this.txtPrims.Location = new System.Drawing.Point(195, 116);
            this.txtPrims.Name = "txtPrims";
            this.txtPrims.ReadOnly = true;
            this.txtPrims.Size = new System.Drawing.Size(54, 21);
            this.txtPrims.TabIndex = 5;
            // 
            // txtCreator
            // 
            this.txtCreator.AgentID = ((OpenMetaverse.UUID)(resources.GetObject("txtCreator.AgentID")));
            this.txtCreator.BackColor = System.Drawing.SystemColors.Window;
            this.txtCreator.Location = new System.Drawing.Point(61, 116);
            this.txtCreator.Name = "txtCreator";
            this.txtCreator.ReadOnly = true;
            this.txtCreator.Size = new System.Drawing.Size(130, 21);
            this.txtCreator.TabIndex = 5;
            // 
            // txtOwner
            // 
            this.txtOwner.AgentID = ((OpenMetaverse.UUID)(resources.GetObject("txtOwner.AgentID")));
            this.txtOwner.BackColor = System.Drawing.SystemColors.Window;
            this.txtOwner.Location = new System.Drawing.Point(61, 91);
            this.txtOwner.Name = "txtOwner";
            this.txtOwner.ReadOnly = true;
            this.txtOwner.Size = new System.Drawing.Size(130, 21);
            this.txtOwner.TabIndex = 4;
            // 
            // txtHover
            // 
            this.txtHover.Location = new System.Drawing.Point(61, 66);
            this.txtHover.Name = "txtHover";
            this.txtHover.Size = new System.Drawing.Size(188, 21);
            this.txtHover.TabIndex = 3;
            // 
            // txtDescription
            // 
            this.txtDescription.Location = new System.Drawing.Point(61, 41);
            this.txtDescription.Name = "txtDescription";
            this.txtDescription.Size = new System.Drawing.Size(188, 21);
            this.txtDescription.TabIndex = 2;
            // 
            // txtObjectName
            // 
            this.txtObjectName.Location = new System.Drawing.Point(61, 16);
            this.txtObjectName.Name = "txtObjectName";
            this.txtObjectName.Size = new System.Drawing.Size(188, 21);
            this.txtObjectName.TabIndex = 1;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(6, 144);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(70, 13);
            this.label8.TabIndex = 0;
            this.label8.Text = "Owner perm.";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(6, 165);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(84, 13);
            this.label7.TabIndex = 0;
            this.label7.Text = "Next own perm.";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(217, 94);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(32, 13);
            this.label6.TabIndex = 0;
            this.label6.Text = "Prims";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(6, 119);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(44, 13);
            this.label5.TabIndex = 0;
            this.label5.Text = "Creator";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(6, 94);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(39, 13);
            this.label4.TabIndex = 0;
            this.label4.Text = "Owner";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(6, 69);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(56, 13);
            this.label3.TabIndex = 0;
            this.label3.Text = "Hovertext";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 44);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(34, 13);
            this.label2.TabIndex = 0;
            this.label2.Text = "Desc.";
            // 
            // lblName
            // 
            this.lblName.AutoSize = true;
            this.lblName.Location = new System.Drawing.Point(6, 18);
            this.lblName.Name = "lblName";
            this.lblName.Size = new System.Drawing.Size(34, 13);
            this.lblName.TabIndex = 0;
            this.lblName.Text = "Name";
            // 
            // Filters
            // 
            this.Filters.Controls.Add(this.searchOptions);
            this.Filters.Location = new System.Drawing.Point(4, 22);
            this.Filters.Name = "Filters";
            this.Filters.Padding = new System.Windows.Forms.Padding(3);
            this.Filters.Size = new System.Drawing.Size(399, 442);
            this.Filters.TabIndex = 1;
            this.Filters.Text = "Filters";
            this.Filters.UseVisualStyleBackColor = true;
            // 
            // searchOptions
            // 
            this.searchOptions.AutoSize = true;
            this.searchOptions.Dock = System.Windows.Forms.DockStyle.Fill;
            this.searchOptions.FlowDirection = System.Windows.Forms.FlowDirection.BottomUp;
            this.searchOptions.Location = new System.Drawing.Point(3, 3);
            this.searchOptions.Name = "searchOptions";
            this.searchOptions.Size = new System.Drawing.Size(393, 436);
            this.searchOptions.TabIndex = 18;
            // 
            // SimObjectsConsole
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.btnRefresh);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.MinimumSize = new System.Drawing.Size(508, 417);
            this.Name = "SimObjectsConsole";
            this.Size = new System.Drawing.Size(833, 490);
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.ResumeLayout(false);
            this.gbxInWorld.ResumeLayout(false);
            this.gbxInWorld.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nudRadius)).EndInit();
            this.tabSimObjeks.ResumeLayout(false);
            this.tabSimOjbectDetail.ResumeLayout(false);
            this.grpPrimInfo.ResumeLayout(false);
            this.gbxObjectDetails.ResumeLayout(false);
            this.gbxObjectDetails.PerformLayout();
            this.Filters.ResumeLayout(false);
            this.Filters.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button btnRefresh;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripStatusLabel lblStatus;
        private RadegastContextMenuStrip ctxMenuObjects;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private ListViewNoFlicker lstPrims;
        private System.Windows.Forms.ColumnHeader columnHeader1;
        private System.Windows.Forms.TabControl tabSimObjeks;
        private System.Windows.Forms.TabPage tabSimOjbectDetail;
        private System.Windows.Forms.TabPage Filters;
        private System.Windows.Forms.FlowLayoutPanel searchOptions;
        private System.Windows.Forms.GroupBox gbxInWorld;
        private System.Windows.Forms.TextBox txtSearch;
        private System.Windows.Forms.RadioButton rbName;
        private System.Windows.Forms.RadioButton rbDistance;
        private System.Windows.Forms.NumericUpDown nudRadius;
        private System.Windows.Forms.Button btnClear;
        private System.Windows.Forms.FlowLayoutPanel grpPrimInfo;
        private System.Windows.Forms.GroupBox gbxObjectDetails;
        private System.Windows.Forms.CheckBox cbNextOwnTransfer;
        private System.Windows.Forms.CheckBox cbNextOwnCopy;
        private System.Windows.Forms.CheckBox cbOwnerTransfer;
        private System.Windows.Forms.CheckBox cbNextOwnModify;
        private System.Windows.Forms.CheckBox cbOwnerCopy;
        private System.Windows.Forms.CheckBox cbOwnerModify;
        private System.Windows.Forms.TextBox txtPrims;
        private AgentNameTextBox txtCreator;
        private AgentNameTextBox txtOwner;
        private System.Windows.Forms.TextBox txtHover;
        private System.Windows.Forms.TextBox txtDescription;
        private System.Windows.Forms.TextBox txtObjectName;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label lblName;
        private System.Windows.Forms.CheckBox chkOwnerMove;
    
    }
}