using System;
using System.Collections.Generic;
using System.IO;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    internal class SimAnimation : SimAsset
    {
        private bool _NeedsRequest = true;
        public override bool NeedsRequest
        {
            get
            {
                if (HasData()) return false;
                if (_Name.Count > 0) return false;
                return _NeedsRequest;
            }
            set { _NeedsRequest = value; }
        }

        public SimAnimation(UUID uuid, string name)
            : base(uuid, name)
        {
        }

        public bool SameAnims(BinBVHAnimationReader bvh1, BinBVHAnimationReader bvh2)
        {
            if (bvh1 == bvh2) return true;
            if (bvh1.joints.Length != bvh2.joints.Length) return false;
            if (bvh1.Loop != bvh2.Loop) return false;
            return false;
        }

        protected override string GuessAssetName()
        {
            BinBVHAnimationReader bvh = Reader;
            if (bvh != null && !string.IsNullOrEmpty(bvh.ExpressionName))
            {
                string n = bvh.ExpressionName;
                _Name.Add(n);
                return n;
            }
            return null;
        }

        public BinBVHAnimationReader _reader;
        public BinBVHAnimationReader Reader
        {
            get
            {
                if (_reader == null)
                {
                    try
                    {
                        byte[] tryb = AssetData;
                        if (tryb != null && tryb.Length > 0)
                            _reader = new BinBVHAnimationReader(tryb);
                        AnalyzeType();

                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("" + e);
                        //                        _TypeData = null;
                    }
                }
                return _reader;
            }
            set
            {
                if (_reader == value)
                    return;
                _reader = value;
                AnalyzeType();
            }
        }

        override public Single Length
        {
            get
            {
                if (!HasData()) return 2;
                return Reader.Length;
            }
        }

        public override bool HasData()
        {
            return _reader != null;
        }

        public override bool IsLoop
        {
            get
            {
                if (!HasData()) return false;
                return Reader.Loop;
            }
        }


        private void AnalyzeType()
        {
            if (!HasData()) return;
            BinBVHAnimationReader r = _reader;
            lock (SimAssetStore.SimAssets) foreach (SimAsset animation in SimAssetStore.SimAssets)
                {
                    if (animation == this) continue;
                    if (!animation.HasData()) continue;
                    if (SameAsset(animation))
                    {
                        Console.WriteLine("Found dup " + animation);
                        foreach (string name in animation._Name)
                        {
                            AddName(name + "_" + AssetID);
                        }
                        foreach (string meaning in animation.Meanings)
                        {
                            AddType(meaning);
                        }
                        return;
                    }
                }
        }

        public override bool SameAsset(SimAsset animation)
        {
            if (animation==null) return false;
            if (animation.AssetType!=AssetType) return false;
            if (HasData())
            {
                
            }
            if (animation is SimAnimation)
            {
//                r = animation.Reader;
                
            }
            return false;
        }

        public override string DebugInfo()
        {
            string st = ToString();
            if (HasData())
            {
                BinBVHAnimationReader r = Reader;
                AddName(r.ExpressionName);

                SortedDictionary<JointMoveSize, JointMoveSize> stuff = new SortedDictionary<JointMoveSize, JointMoveSize>();

                foreach (var c in r.joints)
                {
                    // Torso shall cover it
                    if (c.Name == "mPelvis") continue;
                    JointMoveSize js = new JointMoveSize(c);
                    stuff[js] = js;
                }
                foreach (var c in stuff.Values)
                {
                    st += " " + c;
                }
            }
            return st;
        }

        public class JointMoveSize : IComparable<JointMoveSize>
        {
            readonly binBVHJoint joint;
            readonly float timeLen;
            readonly float posLen;

            float Mag
            {
                get { return posLen / timeLen; }
            }

            public int CompareTo(JointMoveSize other)
            {
                if (ReferenceEquals(this, other)) return 0;
                float f;

                f = (Mag - other.Mag);
                if (f > 0) return 1;
                if (f < 0) return -1;

                f = (timeLen - other.timeLen);
                if (f > 0) return 1;
                if (f < 0) return -1;

                f = (posLen - other.posLen);
                if (f > 0) return 1;
                if (f < 0) return -1;

                // no difference?
                return joint.Name.GetHashCode() - other.joint.Name.GetHashCode();
            }

            public override string ToString()
            {
                return String.Format("{0}={1:0.0#}", joint.Name, Mag);
                //return String.Format("{0}={1:0.0#}/{2:0.0#}", joint.Name, posLen, timeLen);
            }

            public JointMoveSize(binBVHJoint bvhJoint)
            {
                joint = bvhJoint;
                timeLen = ComputeTime(bvhJoint.rotationkeys) + ComputeTime(bvhJoint.positionkeys);
                posLen = ComputeMotion(bvhJoint.rotationkeys) + ComputeMotion(bvhJoint.positionkeys);
            }

            static float ComputeTime(binBVHJointKey[] binBVHJointKey)
            {
                if (binBVHJointKey.Length == 0) return 0;
                double totalLen = binBVHJointKey[0].time;
                if (binBVHJointKey.Length == 1) return (float)totalLen;
                for (int i = 1; i < binBVHJointKey.Length / 2; i++)
                {
                    totalLen += binBVHJointKey[i].time;
                }
                return (float)totalLen;
            }

            static float ComputeMotion(binBVHJointKey[] binBVHJointKey)
            {
                if (binBVHJointKey.Length == 0) return 0;
                Vector3 next = binBVHJointKey[0].key_element;
                double totalLen = binBVHJointKey[0].time;
                if (binBVHJointKey.Length == 1) return (float)totalLen;
                for (int i = 1; i < binBVHJointKey.Length / 2; i++)
                {
                    Vector3 n = binBVHJointKey[i].key_element;
                    totalLen += (next - n).LengthSquared();
                    next = n;
                }
                return (float)totalLen;
            }
        }


        public byte[] _TypeData;
        protected override void SaveFile(string tmpname)
        {
            byte[] bs = AssetData;
            if (bs != null && bs.Length > 0)
            {
                File.WriteAllBytes(tmpname + ".anim", AssetData);
            }
            //WorldObjects.Master.GetModuleName()
        }

        override public byte[] AssetData
        {
            get
            {
                if (_TypeData == null)
                {
                    foreach (string n in _Name)
                    {
                        byte[] N_TypeData;
                        string usedName;
                        if (SimAssetStore.BytesFromFile(n, out N_TypeData, out usedName))
                        {
                            _TypeData = N_TypeData;
                            Name = usedName;
                            break;
                        }

                    }

                }
                return _TypeData;
            }
            set
            {
                if (_TypeData == value)
                    return;
                _TypeData = value;
                try
                {
                    _reader = new BinBVHAnimationReader(_TypeData);
                    AnalyzeType();
                }
                catch (System.Exception ex)
                {
                    Console.WriteLine("" + ex);
                    //_TypeData = null;
                }
            }
        }
    }
}