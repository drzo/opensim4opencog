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
// $Id: SimAspectConsole.Designer.cs 181 2009-09-03 01:32:49Z latifer@gmail.com $
//
﻿namespace cogbot.GUI
{
    partial class SimAspectConsole
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
                if (InventoryUpdate != null)
                {
                    InventoryUpdate.Abort();
                    InventoryUpdate = null;
                }

                if (_EditTimer != null)
                {
                    _EditTimer.Dispose();
                    _EditTimer = null;
                }

                if (TreeUpdateTimer != null)
                {
                    TreeUpdateTimer.Dispose();
                    TreeUpdateTimer = null;
                }

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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SimAspectConsole));
            this.invTree = new System.Windows.Forms.TreeView();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.tstripInventory = new System.Windows.Forms.ToolStrip();
            this.tlabelStatus = new System.Windows.Forms.ToolStripLabel();
            this.tbtnFile = new System.Windows.Forms.ToolStripDropDownButton();
            this.reloadInventoryToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.saveAllTToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.aspectsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.tbtbSort = new System.Windows.Forms.ToolStripDropDownButton();
            this.tbtbSortByName = new System.Windows.Forms.ToolStripMenuItem();
            this.tbtnSortByDate = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbtbFoldersByName = new System.Windows.Forms.ToolStripMenuItem();
            this.tbtnSystemFoldersFirst = new System.Windows.Forms.ToolStripMenuItem();
            this.ExtraContextMenu = new System.Windows.Forms.ToolStripDropDownButton();
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
            this.pnlDetail = new System.Windows.Forms.Panel();
            this.panel1 = new System.Windows.Forms.Panel();
            this.btnProfile = new System.Windows.Forms.Button();
            this.txtCreated = new System.Windows.Forms.TextBox();
            this.txtAssetID = new System.Windows.Forms.TextBox();
            this.lblCreated = new System.Windows.Forms.Label();
            this.txtItemName = new System.Windows.Forms.TextBox();
            this.lblAsset = new System.Windows.Forms.Label();
            this.lblCreator = new System.Windows.Forms.Label();
            this.lblItemName = new System.Windows.Forms.Label();
            this.ctxInv = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.txtCreator = new Radegast.AgentNameTextBox();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.tstripInventory.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // invTree
            // 
            this.invTree.AllowDrop = true;
            this.invTree.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(62)))), ((int)(((byte)(62)))), ((int)(((byte)(62)))));
            this.invTree.Dock = System.Windows.Forms.DockStyle.Fill;
            this.invTree.ForeColor = System.Drawing.Color.White;
            this.invTree.LabelEdit = true;
            this.invTree.LineColor = System.Drawing.Color.White;
            this.invTree.Location = new System.Drawing.Point(0, 25);
            this.invTree.Name = "invTree";
            this.invTree.Size = new System.Drawing.Size(331, 458);
            this.invTree.TabIndex = 0;
            this.invTree.AfterLabelEdit += new System.Windows.Forms.NodeLabelEditEventHandler(this.invTree_AfterLabelEdit);
            this.invTree.DragDrop += new System.Windows.Forms.DragEventHandler(this.invTree_DragDrop);
            this.invTree.DragEnter += new System.Windows.Forms.DragEventHandler(this.invTree_DragEnter);
            this.invTree.KeyUp += new System.Windows.Forms.KeyEventHandler(this.invTree_KeyUp);
            this.invTree.BeforeLabelEdit += new System.Windows.Forms.NodeLabelEditEventHandler(this.invTree_BeforeLabelEdit);
            this.invTree.ItemDrag += new System.Windows.Forms.ItemDragEventHandler(this.invTree_ItemDrag);
            this.invTree.DragOver += new System.Windows.Forms.DragEventHandler(this.invTree_DragOver);
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.invTree);
            this.splitContainer1.Panel1.Controls.Add(this.tstripInventory);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.pnlDetail);
            this.splitContainer1.Panel2.Controls.Add(this.panel1);
            this.splitContainer1.Size = new System.Drawing.Size(756, 483);
            this.splitContainer1.SplitterDistance = 331;
            this.splitContainer1.TabIndex = 1;
            // 
            // tstripInventory
            // 
            this.tstripInventory.GripMargin = new System.Windows.Forms.Padding(0);
            this.tstripInventory.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.tstripInventory.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.tlabelStatus,
            this.tbtnFile,
            this.tbtbSort,
            this.ExtraContextMenu});
            this.tstripInventory.Location = new System.Drawing.Point(0, 0);
            this.tstripInventory.Name = "tstripInventory";
            this.tstripInventory.Size = new System.Drawing.Size(331, 25);
            this.tstripInventory.TabIndex = 1;
            this.tstripInventory.Text = "toolStrip1";
            // 
            // tlabelStatus
            // 
            this.tlabelStatus.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
            this.tlabelStatus.Name = "tlabelStatus";
            this.tlabelStatus.Size = new System.Drawing.Size(56, 22);
            this.tlabelStatus.Text = "Loading...";
            this.tlabelStatus.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // tbtnFile
            // 
            this.tbtnFile.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbtnFile.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.reloadInventoryToolStripMenuItem,
            this.saveAllTToolStripMenuItem,
            this.aspectsToolStripMenuItem});
            this.tbtnFile.Image = ((System.Drawing.Image)(resources.GetObject("tbtnFile.Image")));
            this.tbtnFile.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtnFile.Name = "tbtnFile";
            this.tbtnFile.Size = new System.Drawing.Size(58, 22);
            this.tbtnFile.Text = "Aspects";
            // 
            // reloadInventoryToolStripMenuItem
            // 
            this.reloadInventoryToolStripMenuItem.Name = "reloadInventoryToolStripMenuItem";
            this.reloadInventoryToolStripMenuItem.Size = new System.Drawing.Size(169, 22);
            this.reloadInventoryToolStripMenuItem.Text = "Reload Inventory";
            this.reloadInventoryToolStripMenuItem.ToolTipText = "Clears inventory cache, and downloads whole inventory from server again";
            this.reloadInventoryToolStripMenuItem.Click += new System.EventHandler(this.reloadInventoryToolStripMenuItem_Click);
            // 
            // saveAllTToolStripMenuItem
            // 
            this.saveAllTToolStripMenuItem.Enabled = false;
            this.saveAllTToolStripMenuItem.Name = "saveAllTToolStripMenuItem";
            this.saveAllTToolStripMenuItem.Size = new System.Drawing.Size(169, 22);
            this.saveAllTToolStripMenuItem.Text = "Save all text";
            this.saveAllTToolStripMenuItem.ToolTipText = "Saves all notecards and scripts to folder on local disk";
            this.saveAllTToolStripMenuItem.Click += new System.EventHandler(this.saveAllTToolStripMenuItem_Click);
            // 
            // aspectsToolStripMenuItem
            // 
            this.aspectsToolStripMenuItem.Name = "aspectsToolStripMenuItem";
            this.aspectsToolStripMenuItem.Size = new System.Drawing.Size(169, 22);
            this.aspectsToolStripMenuItem.Text = "Aspects";
            // 
            // tbtbSort
            // 
            this.tbtbSort.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbtbSort.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.tbtbSortByName,
            this.tbtnSortByDate,
            this.toolStripMenuItem1,
            this.tbtbFoldersByName,
            this.tbtnSystemFoldersFirst});
            this.tbtbSort.Image = ((System.Drawing.Image)(resources.GetObject("tbtbSort.Image")));
            this.tbtbSort.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbtbSort.Name = "tbtbSort";
            this.tbtbSort.Size = new System.Drawing.Size(40, 22);
            this.tbtbSort.Text = "Sort";
            // 
            // tbtbSortByName
            // 
            this.tbtbSortByName.Name = "tbtbSortByName";
            this.tbtbSortByName.Size = new System.Drawing.Size(200, 22);
            this.tbtbSortByName.Text = "By name";
            // 
            // tbtnSortByDate
            // 
            this.tbtnSortByDate.Name = "tbtnSortByDate";
            this.tbtnSortByDate.Size = new System.Drawing.Size(200, 22);
            this.tbtnSortByDate.Text = "By date";
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(197, 6);
            // 
            // tbtbFoldersByName
            // 
            this.tbtbFoldersByName.Name = "tbtbFoldersByName";
            this.tbtbFoldersByName.Size = new System.Drawing.Size(200, 22);
            this.tbtbFoldersByName.Text = "Folders always by name";
            // 
            // tbtnSystemFoldersFirst
            // 
            this.tbtnSystemFoldersFirst.Name = "tbtnSystemFoldersFirst";
            this.tbtnSystemFoldersFirst.Size = new System.Drawing.Size(200, 22);
            this.tbtnSystemFoldersFirst.Text = "System folders on top";
            // 
            // ExtraContextMenu
            // 
            this.ExtraContextMenu.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.ExtraContextMenu.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.folderToolStripMenuItem,
            this.objectToolStripMenuItem,
            this.lSLTextToolStripMenuItem,
            this.gestureToolStripMenuItem1,
            this.notecardToolStripMenuItem1,
            this.textureToolStripMenuItem1,
            this.callingCardToolStripMenuItem,
            this.soundToolStripMenuItem,
            this.animationToolStripMenuItem});
            this.ExtraContextMenu.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ExtraContextMenu.Name = "ExtraContextMenu";
            this.ExtraContextMenu.Size = new System.Drawing.Size(48, 22);
            this.ExtraContextMenu.Text = "Plugin";
            this.ExtraContextMenu.ToolTipText = "Plugin";
            this.ExtraContextMenu.Visible = false;
            this.ExtraContextMenu.Click += new System.EventHandler(this.ExtraContextMenu_Click);
            // 
            // folderToolStripMenuItem
            // 
            this.folderToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ContextToolStripMenuItem});
            this.folderToolStripMenuItem.Name = "folderToolStripMenuItem";
            this.folderToolStripMenuItem.Size = new System.Drawing.Size(139, 22);
            this.folderToolStripMenuItem.Text = "Folder";
            this.folderToolStripMenuItem.Click += new System.EventHandler(this.folderToolStripMenuItem_Click);
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
            this.simTypeToolStripMenuItem.Click += new System.EventHandler(this.simTypeToolStripMenuItem_Click);
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
            this.rezToolStripMenuItem.Click += new System.EventHandler(this.rezToolStripMenuItem_Click);
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
            // pnlDetail
            // 
            this.pnlDetail.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlDetail.Location = new System.Drawing.Point(0, 0);
            this.pnlDetail.Name = "pnlDetail";
            this.pnlDetail.Size = new System.Drawing.Size(421, 336);
            this.pnlDetail.TabIndex = 1;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.btnProfile);
            this.panel1.Controls.Add(this.txtCreator);
            this.panel1.Controls.Add(this.txtCreated);
            this.panel1.Controls.Add(this.txtAssetID);
            this.panel1.Controls.Add(this.lblCreated);
            this.panel1.Controls.Add(this.txtItemName);
            this.panel1.Controls.Add(this.lblAsset);
            this.panel1.Controls.Add(this.lblCreator);
            this.panel1.Controls.Add(this.lblItemName);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(0, 336);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(421, 147);
            this.panel1.TabIndex = 0;
            // 
            // btnProfile
            // 
            this.btnProfile.AccessibleDescription = "Open profile";
            this.btnProfile.Enabled = false;
            this.btnProfile.Image = ((System.Drawing.Image)(resources.GetObject("btnProfile.Image")));
            this.btnProfile.Location = new System.Drawing.Point(54, 36);
            this.btnProfile.Name = "btnProfile";
            this.btnProfile.Size = new System.Drawing.Size(26, 23);
            this.btnProfile.TabIndex = 2;
            this.btnProfile.UseVisualStyleBackColor = true;
            this.btnProfile.Click += new System.EventHandler(this.btnProfile_Click);
            // 
            // txtCreated
            // 
            this.txtCreated.Location = new System.Drawing.Point(80, 62);
            this.txtCreated.Name = "txtCreated";
            this.txtCreated.ReadOnly = true;
            this.txtCreated.Size = new System.Drawing.Size(144, 20);
            this.txtCreated.TabIndex = 1;
            // 
            // txtAssetID
            // 
            this.txtAssetID.Location = new System.Drawing.Point(80, 88);
            this.txtAssetID.Name = "txtAssetID";
            this.txtAssetID.ReadOnly = true;
            this.txtAssetID.Size = new System.Drawing.Size(338, 20);
            this.txtAssetID.TabIndex = 1;
            // 
            // lblCreated
            // 
            this.lblCreated.AutoSize = true;
            this.lblCreated.Location = new System.Drawing.Point(3, 62);
            this.lblCreated.Name = "lblCreated";
            this.lblCreated.Size = new System.Drawing.Size(44, 13);
            this.lblCreated.TabIndex = 0;
            this.lblCreated.Text = "Created";
            // 
            // txtItemName
            // 
            this.txtItemName.Location = new System.Drawing.Point(80, 10);
            this.txtItemName.Name = "txtItemName";
            this.txtItemName.Size = new System.Drawing.Size(338, 20);
            this.txtItemName.TabIndex = 1;
            // 
            // lblAsset
            // 
            this.lblAsset.AutoSize = true;
            this.lblAsset.Location = new System.Drawing.Point(3, 88);
            this.lblAsset.Name = "lblAsset";
            this.lblAsset.Size = new System.Drawing.Size(47, 13);
            this.lblAsset.TabIndex = 0;
            this.lblAsset.Text = "Asset ID";
            // 
            // lblCreator
            // 
            this.lblCreator.AutoSize = true;
            this.lblCreator.Location = new System.Drawing.Point(3, 36);
            this.lblCreator.Name = "lblCreator";
            this.lblCreator.Size = new System.Drawing.Size(41, 13);
            this.lblCreator.TabIndex = 0;
            this.lblCreator.Text = "Creator";
            // 
            // lblItemName
            // 
            this.lblItemName.AutoSize = true;
            this.lblItemName.Location = new System.Drawing.Point(3, 10);
            this.lblItemName.Name = "lblItemName";
            this.lblItemName.Size = new System.Drawing.Size(27, 13);
            this.lblItemName.TabIndex = 0;
            this.lblItemName.Text = "Item";
            // 
            // ctxInv
            // 
            this.ctxInv.Name = "folderContext";
            this.ctxInv.ShowImageMargin = false;
            this.ctxInv.Size = new System.Drawing.Size(36, 4);
            this.ctxInv.Text = "Inventory Folder";
            // 
            // txtCreator
            // 
            this.txtCreator.AgentID = ((OpenMetaverse.UUID)(resources.GetObject("txtCreator.AgentID")));
            this.txtCreator.BackColor = System.Drawing.SystemColors.Window;
            this.txtCreator.Location = new System.Drawing.Point(80, 36);
            this.txtCreator.Name = "txtCreator";
            this.txtCreator.ReadOnly = true;
            this.txtCreator.Size = new System.Drawing.Size(338, 20);
            this.txtCreator.TabIndex = 1;
            // 
            // SimAspectConsole
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.splitContainer1);
            this.Name = "SimAspectConsole";
            this.Size = new System.Drawing.Size(756, 483);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel1.PerformLayout();
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.ResumeLayout(false);
            this.tstripInventory.ResumeLayout(false);
            this.tstripInventory.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.ContextMenuStrip ctxInv;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.TextBox txtItemName;
        private System.Windows.Forms.Label lblCreator;
        private System.Windows.Forms.Label lblItemName;
        private Radegast.AgentNameTextBox txtCreator;
        private System.Windows.Forms.TextBox txtAssetID;
        private System.Windows.Forms.Label lblAsset;
        private System.Windows.Forms.Panel pnlDetail;
        private System.Windows.Forms.Button btnProfile;
        private System.Windows.Forms.TextBox txtCreated;
        private System.Windows.Forms.Label lblCreated;
        private System.Windows.Forms.ToolStrip tstripInventory;
        private System.Windows.Forms.ToolStripLabel tlabelStatus;
        private System.Windows.Forms.ToolStripDropDownButton tbtnFile;
        private System.Windows.Forms.ToolStripMenuItem saveAllTToolStripMenuItem;
        private System.Windows.Forms.ToolStripDropDownButton tbtbSort;
        private System.Windows.Forms.ToolStripMenuItem tbtbSortByName;
        private System.Windows.Forms.ToolStripMenuItem tbtnSortByDate;
        private System.Windows.Forms.ToolStripSeparator toolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem tbtbFoldersByName;
        private System.Windows.Forms.ToolStripMenuItem tbtnSystemFoldersFirst;
        private System.Windows.Forms.ToolStripMenuItem reloadInventoryToolStripMenuItem;
        public System.Windows.Forms.TreeView invTree;
        private System.Windows.Forms.ToolStripMenuItem aspectsToolStripMenuItem;
        public System.Windows.Forms.ToolStripDropDownButton ExtraContextMenu;
        private System.Windows.Forms.ToolStripMenuItem folderToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem ContextToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem scriptToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem notecardToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem textureToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem gestureToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem objectToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem rezToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem lSLTextToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem gestureToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem notecardToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem textureToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem callingCardToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem soundToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem animationToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem simTypeToolStripMenuItem;
    }
}
