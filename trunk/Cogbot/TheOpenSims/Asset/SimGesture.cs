using System;
using System.Collections.Generic;
using System.Text;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    internal class SimGesture : SimAsset
    {
        static readonly Dictionary<UUID,SimGesture> AnimationGestures = new Dictionary<UUID, SimGesture>();

        public SimGesture(UUID uuid, string name)
            : base(uuid, name)
        {
        }

        protected override void SaveFile(string tmpname)
        {
            WriteLine("Not implemented save gesture file " + tmpname);
        }

        public AssetGesture GetGesture()
        {
            return (AssetGesture)ServerAsset;
        }

        public override Asset ServerAsset
        {
            get { return _ServerAsset; }
            set
            {
                _ServerAsset = value;
                GetParts();
                if (value != null)
                {
                    AssetType = value.AssetType;
                    AssetID = value.AssetID;
                }
            }
        }

        private byte[] _TypeData;
        public override byte[] AssetData
        {
            get
            {
                if (_TypeData != null) return _TypeData;
                if (ServerAsset == null) return null;
                return ServerAsset.AssetData;
            }
            set
            {
                if (_TypeData!=null)
                {
                    
                }
                _TypeData = value;
                if (ServerAsset == null)
                {
                    if (AssetID != UUID.Zero)
                    {
                        ServerAsset = new AssetGesture(AssetID, value);
                    }
                    return;
                }
                else
                {
                    ServerAsset.AssetData = value;
                }
            }
        }

        private void tbtnReupload_Click(object sender, EventArgs e)
        {
            AssetGesture gestureAsset = GetGesture();
            GridClient client = WorldObjects.GridMaster.client;
            InventoryItem gesture = Item;
            UpdateStatus("Creating new item...");

            client.Inventory.RequestCreateItem(gesture.ParentUUID, "Copy of " + gesture.Name, gesture.Description, AssetType.Gesture, UUID.Random(), InventoryType.Gesture, PermissionMask.All,
                delegate(bool success, InventoryItem item)
                {
                    if (success)
                    {
                        UpdateStatus("Uploading data...");

                        client.Inventory.RequestUploadGestureAsset(gestureAsset.AssetData, item.UUID,
                            delegate(bool assetSuccess, string status, UUID itemID, UUID assetID)
                            {
                                if (assetSuccess)
                                {
                                    gesture.AssetUUID = assetID;
                                    UpdateStatus("OK");
                                }
                                else
                                {
                                    UpdateStatus("Asset failed");
                                }
                            }
                        );
                    }
                    else
                    {
                        UpdateStatus("Inv. failed");
                    }
                }
            );
        }

        private void UpdateStatus(string s)
        {
           // throw new NotImplementedException();
        }

        protected override string GuessAssetName()
        {
            if (Item != null)
            {
                return Item.Name;
            }
            String s = "";
            if (ServerAsset == null) return null;
            ServerAsset.Decode();
            AssetGesture gestureAsset = GetGesture();
            for (int i = 0; i < gestureAsset.Sequence.Count; i++)
            {
                s += (gestureAsset.Sequence[i].ToString().Trim() + Environment.NewLine);
            }
            if (!string.IsNullOrEmpty(s)) return s;
            AssetGesture S = (AssetGesture)ServerAsset;
            AssetData = S.AssetData;
            return UnknownName;
        }

        public List<SimAsset> GetParts()
        {

            AssetGesture gestureAsset = GetGesture();
            //            StringBuilder sb = new StringBuilder();
            //sb.Append("2\n");
            Name  = gestureAsset.Trigger;
            //sb.Append(TriggerKey + "\n");
            //sb.Append(TriggerKeyMask + "\n");
            //sb.Append(Trigger + "\n");
            Name = gestureAsset.ReplaceWith;//sb.Append(ReplaceWith + "\n");

            List<GestureStep> Sequence = gestureAsset.Sequence;
            int count = 0;
            if (Sequence != null)
            {
                count = Sequence.Count;
            }
            List<SimAsset> parts = new List<SimAsset>(count);
            //sb.Append(count + "\n");
            _Length = 0;
            for (int i = 0; i < count; i++)
            {
                GestureStep step = Sequence[i];
                // sb.Append((int)step.GestureStepType + "\n");
                SimAsset asset;

                switch (step.GestureStepType)
                {
                    case GestureStepType.EOF:
                        goto Finish;

                    case GestureStepType.Animation:
                        GestureStepAnimation animstep = (GestureStepAnimation) step;
                        asset = SimAssetStore.FindOrCreateAsset(animstep.ID, AssetType.Animation);
                        asset.Name = animstep.Name;
                        if (animstep.AnimationStart)
                        {
                            parts.Add(asset);
                            //                            sb.Append("0\n");
                            _Length += asset.Length;
                        }
                        else
                        {
                            //             sb.Append("1\n");
                        }
                        break;

                    case GestureStepType.Sound:
                        GestureStepSound soundstep = (GestureStepSound) step;
                        asset = SimAssetStore.FindOrCreateAsset(soundstep.ID, AssetType.Sound);
                        asset.Name = soundstep.Name;
                        parts.Add(asset);
                        _Length += asset.Length;
                        break;

                    case GestureStepType.Chat:
                        GestureStepChat chatstep = (GestureStepChat) step;
                        Name = chatstep.Text;
                        //sb.Append(chatstep.Text + "\n");
                        //sb.Append("0\n");
                        _Length += 10;
                        break;

                    case GestureStepType.Wait:
                        GestureStepWait waitstep = (GestureStepWait) step;
                        //sb.AppendFormat("{0:0.000000}\n", waitstep.WaitTime);
                        int waitflags = 0;

                        if (waitstep.WaitForTime)
                        {
                            waitflags |= 0x01;
                            _Length += waitstep.WaitTime;
                        }
                        if (waitstep.WaitForAnimation)
                        {
                            waitflags |= 0x02;
                            _Length += 10;
                        }
                        //sb.Append(waitflags + "\n");
                        break;
                }
            }
            Finish:

            if (parts.Count==1)
            {
                SimAsset A = parts[0];
                if (A is SimAnimation)
                {
                    AnimationGestures[A.AssetID] = this;
                    if (A.Item == null) A.Item = Item;
                }
            }
            return parts;
        }

        private float _Length = 1000f;

        public override float Length
        {
            get
            {
                GetParts();
                return _Length;
            }
        }

        public override bool IsLoop
        {
            get { return false; }
        }

        public static InventoryItem SaveAnimation(SimAnimation animation)
        {
            return null;
         //   throw new NotImplementedException();
        }
    }
}