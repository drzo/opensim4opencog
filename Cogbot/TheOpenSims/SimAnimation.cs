using System;
using System.Collections.Generic;
using System.IO;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    internal class SimAnimation : SimAsset
    {
        readonly static UUID WALK_ADJUST = new UUID("829bc85b-02fc-ec41-be2e-74cc6dd7215d");
        readonly static UUID FLY_ADJUST = new UUID("db95561f-f1b0-9f9a-7224-b12f71af126e");
        internal static void ClassifyAnims()
        {
            string type = "Sitting";
            PutType(type, Animations.SIT_GROUND);
            PutType(type, Animations.SIT);
            PutType(type, Animations.SIT_GENERIC);
            PutType(type, Animations.SIT_FEMALE);
            PutType(type, Animations.CROUCH);
            PutType(type, Animations.MOTORCYCLE_SIT);
            PutType(type, Animations.SIT_GROUND_staticRAINED);

            type = "Flying";

            PutType(type, Animations.FLYSLOW);
            PutType(type, FLY_ADJUST);
            PutType(type, Animations.HOVER_DOWN);
            PutType(type, Animations.HOVER_UP);
            PutType(type, Animations.FLY);
            PutType(type, Animations.HOVER);
            PutType(type, Animations.YOGA_FLOAT);

            type = "Walking";
            PutType(type, Animations.WALK);
            PutType(type, Animations.TURNLEFT);
            PutType(type, Animations.TURNRIGHT);
            PutType(type, Animations.STRIDE);
            PutType(type, Animations.RUN);
            PutType(type, Animations.FEMALE_WALK);
            PutType(type, WALK_ADJUST);
            PutType(type, Animations.CROUCHWALK);


            type = "Moving";
            PutType(type, Animations.FLYSLOW);
            PutType(type, FLY_ADJUST);
            PutType(type, Animations.HOVER_DOWN);
            PutType(type, Animations.HOVER_UP);
            PutType(type, Animations.WALK);
            PutType(type, Animations.TURNLEFT);
            PutType(type, Animations.TURNRIGHT);
            PutType(type, Animations.STRIDE);
            PutType(type, Animations.RUN);
            PutType(type, Animations.FEMALE_WALK);
            PutType(type, WALK_ADJUST);
            PutType(type, Animations.CROUCHWALK);


            type = "Stopping";
            PutType(type, Animations.SIT_TO_STAND);
            PutType(type, Animations.FALLDOWN);
            PutType(type, Animations.STANDUP);
            PutType(type, Animations.MEDIUM_LAND);
            PutType(type, Animations.LAND);
            PutType(type, Animations.HOVER);

            type = "Laying";
            PutType(type, Animations.SLEEP);
            PutType(type, Animations.DEAD);

            type = "Standing";
            PutType(type, Animations.STAND);
            PutType(type, Animations.STAND_1);
            PutType(type, Animations.STAND_2);
            PutType(type, Animations.STAND_3);
            PutType(type, Animations.STAND_4);
            PutType(type, Animations.STANDUP);
            PutType(type, Animations.AWAY);
            PutType(type, Animations.BUSY);
            PutType(type, Animations.HOVER);

            type = "Jumping";
            PutType(type, Animations.JUMP);
            PutType(type, Animations.PRE_JUMP);

            type = "Communicating";
            PutType(type, Animations.TALK);
            PutType(type, Animations.TYPE);
            PutType(type, Animations.SHRUG);
            PutType(type, Animations.YES);
            PutType(type, Animations.YES_HAPPY);
            PutType(type, Animations.NO);
            PutType(type, Animations.NO_UNHAPPY);
            PutType(type, Animations.ANGRY);
            PutType(type, Animations.LAUGH_SHORT);
            PutType(type, Animations.CRY);
            PutType(type, Animations.WINK);
            PutType(type, Animations.WHISTLE);
            PutType(type, Animations.SHOUT);
        }


        static void PutType(string anims, UUID uUID)
        {
            SimAsset a = SimAssetStore.FindAsset(uUID);
            a.AddType(anims);
        }

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