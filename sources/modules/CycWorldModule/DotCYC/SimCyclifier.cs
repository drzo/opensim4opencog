using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Xml.Linq;
using cogbot;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using org.opencyc.api;
using org.opencyc.cycobject;
using CUID = org.opencyc.util.UUID;
//using Guid=org.opencyc.cycobject.Guid;
//using CycFort = org.opencyc.cycobject.CycObject;
using OpenMetaverse.Assets;
using PathSystem3D.Navigation;
using Exception=System.Exception;
using Object=System.Object;
using UUID=OpenMetaverse.UUID;
using System.Drawing;
using MushDLR223.ScriptEngines;

namespace CycWorldModule.DotCYC
{
    public class SimCyclifier : SimEventSubscriber
    {
        // ReSharper disable InconsistentNaming

        [ConfigSetting(SkipSaveOnExit = true)]
        public static bool UseCyc = true;
        [ConfigSetting]
        public static bool ClearGridDataBetweenSessions = false;
        [ConfigSetting]
        public static bool ClearRegionDataBetweenSessions = false;
        [ConfigSetting]
        public static bool ClearHistoryBetweenSessions = true;
        [ConfigSetting]
        public static bool StoreInfrastructureEvents = false;
        [ConfigSetting]
        public static bool StorePersonalEvents = false;

        static public CycAccess cycAccess
        {
            get
            {
                _cycAccess = _cycAccess ?? cycConnection.getCycAccess();
                return _cycAccess;
            }
        }
        private static CycAccess _cycAccess;

        static public CycFort vocabMt;
        static public CycFort assertMt;
        static public CycFort queryMt;
        static public CycFort staticStateMt;
        static public int KBTick = 0;
        [ConfigSetting(SkipSaveOnExit = true)]
        static public bool ProcessEvents = true;
        private bool IsDisposing;

        public static IEqualityComparer<object> CompareKeys = new CompareKeysImpl();
        static public Dictionary<object, CycFort> simFort
        {
            get
            {
                return cycTerms;
            }
        }// new Dictionary<string, CycFort>();
        static CycConnectionForm cycConnection;
        static readonly DateTime baseTime = new DateTime(1970, 1, 1, 0, 0, 0);
        private static readonly TaskQueueHandler cycAccessQueueHandler = new TaskQueueHandler("CycAssertions");
        public static readonly TaskQueueHandler cycInfoMapSaver = new TaskQueueHandler("CycInfoMapSaver");
        private static TaskQueueHandler SharedTaskQueueHandler = null;// new TaskQueueHandler("SimCyclifier", 0);

        readonly static public Dictionary<object, CycFort> cycTerms = new Dictionary<object, CycFort>(CompareKeys);
        readonly static public Dictionary<Object, int> hashChanges = new Dictionary<Object, int>(CompareKeys);
        List<UUID> SimObjectsAdded = new List<UUID>();
        internal static readonly Dictionary<Type, CycTypeInfo> typeFort = new Dictionary<Type, CycTypeInfo>();
        private CycFort genlPreds;
        private CycFort cycIsa;
        //static readonly List<String> SkipVerbs = new List<string>() { "on-log-message", "on-login", "on-event-queue-running", "on-sim-connecting" };
        public static SimCyclifier Master;
        static readonly object SimCyclifierLock = new object();
        static bool IsCycDead;
        // ReSharper restore InconsistentNaming
        public void OnEvent(SimObjectEvent evt)
        {
            if (cycAccess == null)
            {
                //Console.WriteLine("No Cyc connection");
                // IsCycDead = true;
                return;
            }
            else
            {
                IsCycDead = false;
            }
            if (IsCycDead) return;
            if (evt.EventType == SimEventType.UNKNOWN)
            {
                return;
            }
            if (evt.EventType == SimEventType.NETWORK && !StoreInfrastructureEvents) return;
            if (evt.EventClass == SimEventClass.PERSONAL && !StorePersonalEvents) return;
            //if (SkipVerbs.Contains(evt.Verb.ToLower())) return;
            if (!UseQueue)
            {
                OnEvent0(evt);
                return;
            }
            SharedTaskQueueHandler.Enqueue(() => OnEvent0(evt));
        }

        static public void OnEvent0(SimObjectEvent evt)
        {
            if (!ProcessEvents) return;
            try
            {
                if (Master.IsDisposing) return;
                if (evt.EventType == SimEventType.DATA_UPDATE)
                {
                    foreach (var v in evt.GetArgs())
                    {
                        Master.DataUpdate(v);
                    }
                    return;
                }
                if (evt.EventType == SimEventType.EFFECT)
                {
                    if (evt.Verb == "LookAtType-Idle") return;
                    if (evt.Verb == "LookAtType-FreeLook") return;
                    if (!StorePersonalEvents) return;
                }
                CycFort fort = Master.FindOrCreateCycFort(evt);
                // Debug("to fort -> " + fort);
            }
            catch (Exception e)
            {

                Exception(e);
                Debug("Error to cyc -> " + evt + " " + e);
            }

        }

        internal void DataUpdate(object v)
        {
            if (IsCycDead) return;
            if (IsDisposing) return;
            object constant = ToFort(v);
            if (constant is CycFort)
            {
                if (v is SimObject)
                {
                    SimObject o = (SimObject)v;
                    if (WorldObjects.GridMaster != null && WorldObjects.GridMaster.TheSimAvatar.Distance(o) < Dist100)
                    {
                        SaveInfoMap(constant as CycFort, o);
                    }
                    else
                        cycInfoMapSaver.Enqueue(() => SaveInfoMap(constant as CycFort, o));
                }
                else if (v is BotMentalAspect)
                {
                    cycInfoMapSaver.AddFirst(() => SaveInfoMap(constant as CycFort, v as BotMentalAspect));
                }
            }
        }

        public void Dispose()
        {
            IsDisposing = true;
            ProcessEvents = false;
            SharedTaskQueueHandler.Dispose();
            cycAccessQueueHandler.Dispose();
            cycInfoMapSaver.Dispose();
            DocQueue.Dispose();
            if (this == Master && cycAccess != null)
            {
                try
                {
                    CycConnectionInterface con = cycAccess.getCycConnection();
                    con.traceOn();
                    con.traceOnDetailed();
                    //cycAccess.persistentConnection = false;
                    con.close();
                    cycAccess.close();
                }
                catch (Exception)
                {
                }
                //java.lang.Thread.currentThread().getThreadGroup().destroy();
            }
        }

        public bool EventsEnabled
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public SimCyclifier(CycWorldModule tf)
        {
            eventFilter = new SimEventFilterSubscriber(this, false);
            lock (SimCyclifierLock)
            {
                if (Master == null) Master = this;
                if (!UseCyc) return;
                if (cycConnection == null)
                {
                    cycConnection = tf.CycConnectionForm;
                }
                if (SharedTaskQueueHandler == null)
                {
                    SharedTaskQueueHandler = new TaskQueueHandler("SimCyclifier");
                    SharedTaskQueueHandler.AddFirst(AssertKE);
                }
                eventFilter.EventsEnabled = true;
            }
        }

        private void AssertKE()
        {
            cycAccessQueueHandler.NoQueue = true;
            CycAccess cycAccess = _cycAccess ?? cycConnection.getCycAccess();
            if (cycAccess == null)
            {
                DLRConsole.DebugWriteLine("No Cyc connection");
                return;
            }
            _cycAccess = cycAccess;
            genlPreds = C("genlPreds");
            cycIsa = C("isa");
            queryMt = cycAccess.createIndividual("SimCurrentStateMt", "#$DataMicrotheory for the simulator", "UniversalVocabularyMt",
                                        "DataMicrotheory");
            vocabMt = cycAccess.createIndividual("SimVocabularyMt", "#$VocabularyMicrotheory for the simulator",
                                       "UniversalVocabularyMt",
                                       "VocabularyMicrotheory");
            assertMt = cycAccess.createIndividual("SimInitialStateMt", "#$DataMicrotheory for T0 of the simulator", "UniversalVocabularyMt",
                           "DataMicrotheory");
            cycAccess.assertGaf(CycAccess.baseKB, C("genlMt-Vocabulary"), assertMt, vocabMt);
            staticStateMt = cycAccess.createIndividual("SimStaticStateMt", "#$Microtheory static entities for the simulator",
                           "UniversalVocabularyMt",
                           "Microtheory");
            cycAccess.assertGaf(CycAccess.baseKB, C("genlMt"), queryMt, assertMt);
            cycAccess.assertGaf(CycAccess.baseKB, C("genlMt-Vocabulary"), staticStateMt, vocabMt);
            cycAccess.assertGaf(CycAccess.baseKB, C("genlMt"), assertMt, staticStateMt);

            if (ClearHistoryBetweenSessions)
            {
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimEvent-LISPFn\"))");
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimEvent-EFFECTFn\"))");
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimEvent-ANIMFn\"))");
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimEvent-UNKNOWNFn\"))");
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimEvent-NETWORKFn\"))");
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimEvent-SOCIALFn\"))");
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimEvent-MOVEMENTFn\"))");
            }
            if (ClearGridDataBetweenSessions)
            {
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimAvatarFn\"))");
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimGroupFn\"))");
            }
            if (ClearRegionDataBetweenSessions || ClearGridDataBetweenSessions)
            {
                cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimObjectFn\"))");
            }
            cycAccess.converseVoid("(fi-kill (find-or-create-constant \"TheDefaultSimInstance\"))");
            cycAccess.converseVoid("(fi-kill (find-or-create-constant \"SimTheDefaultInstance\"))");

            simFort["SimObject"] = createCollection("SimObject", "#$SpatiallyDisjointObjectType for the simulator",
                                                    "SimVocabularyMt", "SpatiallyDisjointObjectType", null);

            assertGafNow(CycAccess.baseKB, C("defaultDefiningMtForInstances"), simFort["SimObject"],
                         C("SimStaticStateMt"));
            simFort["SimAsset"] = createCollection("SimAsset",
                                                   "A SimAsset is a #$PartiallyTangibleTypeByPhysicalFeature or #$ObjectTypeBySensibleFeature from the simulator such as #$BlueColor or #$BlowingAKiss animation",
                                                   "SimVocabularyMt", "Collection", "ObjectTypeBySensibleFeature");
            assertGenls(simFort["SimAsset"], C("ExistenceDependentNonInitialIntermittentCollection")); //ExistenceDependentCollection
            simFort["SimAvatar"] = createCollection("SimAvatar", "#$Agent-Generic for the simulator", "SimVocabularyMt",
                                                    "Collection", "Agent-Generic");

            assertGenls(simFort["SimAvatar"], simFort["SimObject"]);
            //assertGenls(simFort["SimAvatar"], C("SimAgent"));
            FunctionToIndividual("SimRegionFn", "SimRegion", "A region in the simulator");
            FunctionToIndividual("SimAvatarFn", "SimAvatar", "An avatar in the simulator");
            FunctionToIndividual("SimObjectFn", "SimObject", "A primitive in the simulator");
            //"TODO maybe not all objects are Artifacts?"
            //assertGafNow(C("resultIsa"),C("SimObjectFn"),C("SimArtifact"));
            assertIsa(simFort["SimObject"], C("SpatiallyDisjointObjectType"));
            assertGenls(simFort["SimObject"], assertCollection(C("SimItem")));

            // FunctionToCollection("SimAnimationFn", "SimAnimation", "An animation in the simulator");

            assertIsa(C("simEventData"), C("VariableArityPredicate"));
            assertIsa(C("SimObjectEvent"), CycAccess.collection);
            assertIsa(C("SimProperty"), CycAccess.collection);
            assertIsa(C("SimPredicate"), CycAccess.collection);
            assertGenls(C("SimPredicate"), C("Predicate"));
            assertGenls(C("SimProperty"), C("BinaryPredicate"));
            assertGenls(C("SimProperty"), C("SimPredicate"));
            assertIsa(C("SimObjectType"), CycAccess.collection);
            assertGenls(C("SimObjectEvent"), C("Event"));
            FunctionToCollection("SimAssetFn", "SimAsset", "Simulator assets that denote a feature set");
            assertIsa(C("SimAssetFn"), C("CollectionDenotingFunction"));

            simFort["SimGroup"] = createCollection("SimGroup", "#$SocialGroup for the simulator",
                                        "SimVocabularyMt", "Collection", "SocialGroup");
            FunctionToIndividual("SimGroupFn", "SimGroup", "A group in the simulator");

            assertIsa(C("SimEnumCollection"), CycAccess.collection);
            assertGenls(C("SimEnumCollection"), CycAccess.collection);
            assertIsa(C("SimEnumCollection"), C("CollectionType"));
            assertIsa(C("SimEnumBitFlagsCollection"), CycAccess.collection);
            assertGenls(C("SimEnumBitFlagsCollection"), C("SimEnumCollection"));
            assertIsa(C("SimEnumBitFlagsCollection"), C("CollectionType"));
            assertVocabGaf(CycAccess.comment, C("SimEnumCollection"), "Enums collected from SecondLife");
            assertVocabGaf(CycAccess.comment, C("SimEnumBitFlagsCollection"), "BitFlag Enums collected from SecondLife");

            // visit libomv
            if (cycAccess.find("PCode-None") == null)
            {
                Debug("Loading SimEnumCollection Collections ");
                VisitAssembly(Assembly.GetAssembly(typeof(AssetType)));
                VisitAssembly(Assembly.GetAssembly(typeof(OpenMetaverse.PrimFlags)));
                VisitAssembly(Assembly.GetAssembly(typeof(Vector4)));
                VisitAssembly(Assembly.GetAssembly(typeof(OpenMetaverse.Utilities.VoiceStatus)));
                VisitAssembly(Assembly.GetAssembly(typeof(OSD)));
            }
            //else
            {
                Debug("Found SimEnumCollection Collections ");
            }
            if (cycAccess.find("SimEventType-SCRIPT") == null)
            {
                VisitAssembly(Assembly.GetAssembly(typeof(SimEventType)));
            }
            // turn off metadata clearing
            ClearGridDataBetweenSessions = false;
            simFort["SimRegion"] = cycAccess.createCollection("SimRegion", "A region in the simulator", vocabMt, CycAccess.collection,
                           C("GeographicalPlace-3D"));
            assertGenls(simFort["SimRegion"], C("Polyhedron"));

            /*
             * (isa SimLSLTextFn CollectionDenotingFunction) in UniversalVocabularyMt
(isa SimLSLText Collection) in UniversalVocabularyMt
             * 
             * Explanation :
BodyMovementEvent is known not to be a spec of SimAnimation in mt SimVocabularyMt.  
sbhl conflict: (genls BodyMovementEvent SimAnimation) TRUE SimVocabularyMt
      because: ISA (isa genls TransitiveBinaryPredicate) UniversalVocabularyMt TRUE-DEF               
(genls BodyMovementEvent MovementOrShapeChangeEvent) TRUE               
(genls MovementOrShapeChangeEvent PhysicalEvent) TRUE               
(genls PhysicalEvent PhysicalSituation) TRUE               
(genls PhysicalSituation Situation-Localized) TRUE               
(genls Situation-Localized SpatialThing-Localized) TRUE               
(genls SpatialThing-Localized TemporallyExistingThing) TRUE               
(genls TemporallyExistingThing TemporalThing) TRUE               
(disjointWith TemporalThing AtemporalThing) TRUE               
(genls AbstractThing AtemporalThing) TRUE               
(genls #<(CollectionUnionFn (TheSet Predicate SetOrCollection))> AbstractThing) TRUE               
(genls SetOrCollection #<(CollectionUnionFn (TheSet Predicate SetOrCollection))>) TRUE               
(genls Collection SetOrCollection) TRUE               
(genls FixedOrderCollection Collection) TRUE               
(genls FirstOrderCollection FixedOrderCollection) TRUE               
(genls PartiallyTangibleTypeByPhysicalFeature FirstOrderCollection) TRUE               
(genls SimAsset PartiallyTangibleTypeByPhysicalFeature) TRUE               
(genls SimAnimation SimAsset) TRUE


             */
            conceptuallyRelated(C("BodyMovementEvent"), assertCollection(C("SimAnimation")));
            conceptuallyRelated(C("EmittingSound"), assertCollection(C("SimSound")));
            conceptuallyRelated(C("Sound"), C("SimSound"));
            conceptuallyRelated(C("AnimalBodyRegion"), assertCollection(C("SimBodypart")));
            conceptuallyRelated(C("SomethingToWear"), assertCollection(C("SimWearable")));
            conceptuallyRelated(C("Landmark"), assertCollection(C("SimLandmark")));
            conceptuallyRelated(C("VisualImage"), assertCollection(C("SimTexture")));



            bool newlyCreated;
            createIndividual("SimGlobalMapCoordinateSystem", "the secondlife global coordinate system", "CartesianCoordinateSystem", out newlyCreated);


            cycAccess.createCollection("SimRegionMapCoordinateSystem",
                                       "instances are secondlife regional coordinate systems", vocabMt, CycAccess.collection,
                                       C("ThreeDimensionalCoordinateSystem"));

            ResultIsa("SimRegionCoordinateSystemFn", "SimRegionMapCoordinateSystem",
                "#$SimRegionCoordinateSystemFn Takes a #$SimRegion and returns a example: (#$SimRegionCoordinateSystemFn (#$SimRegionFn \"LogicMoo\")) => #$ThreeDimensionalCoordinateSystem",
                simFort["SimRegion"]);

            createIndividual("PointInRegionFn", "Creates region 3D #$Point relative to (#$SimRegionFn :ARG1)", "SimVocabularyMt", "QuaternaryFunction", out newlyCreated);
            assertIsa(C("PointInRegionFn"), C("TotalFunction"));
            //assertGaf(CycAccess.isa, simFort["PointInRegionFn"], C(""));
            assertVocabGaf(C("arg1Isa"), simFort["PointInRegionFn"], C("IDString"));
            assertVocabGaf(C("arg2Isa"), simFort["PointInRegionFn"], C("NumericInterval"));
            assertVocabGaf(C("arg3Isa"), simFort["PointInRegionFn"], C("NumericInterval"));
            assertVocabGaf(C("arg4Isa"), simFort["PointInRegionFn"], C("NumericInterval"));
            assertVocabGaf(C("resultIsa"), simFort["PointInRegionFn"], C("Point"));
            if (false) assertCycLExpression("(#$expansion #$PointInRegionFn "
                + " (#$PointIn3DCoordinateSystemFn (#$SimRegionCoordinateSystemFn"
                + " (#$SimRegionFn :ARG1)) :ARG2 :ARG3 :ARG4 ))", vocabMt);

            createIndividual("PointRelativeToFn", "Creates Local 3D #$Point relative to the #$SpatialThing-Localized in :ARG1", "SimVocabularyMt", "QuaternaryFunction", out newlyCreated);
            assertIsa(C("PointRelativeToFn"), C("QuaternaryFunction"));
            assertIsa(C("PointRelativeToFn"), C("TotalFunction"));
            assertVocabGaf(C("arg1Isa"), simFort["PointRelativeToFn"], C("SpatialThing-Localized"));
            assertVocabGaf(C("arg2Isa"), simFort["PointRelativeToFn"], C("NumericInterval"));
            assertVocabGaf(C("arg3Isa"), simFort["PointRelativeToFn"], C("NumericInterval"));
            assertVocabGaf(C("arg4Isa"), simFort["PointRelativeToFn"], C("NumericInterval"));
            assertVocabGaf(C("resultIsa"), C("PointRelativeToFn"), C("Point"));

            if (false)
            {
                assertCycLExpression(
                    "(#$pointInSystem (#$PointInRegionFn ?STR ?X ?Y ?Z) (#$SimRegionCoordinateSystemFn (#$SimRegionFn ?STR)))", vocabMt);
                assertCycLExpression(
                    "(#$pointInSystem (#$PointInRegionFn \"Daxlandia\" 128 120 27) (#$SimRegionCoordinateSystemFn (#$SimRegionFn \"Daxlandia\")))", vocabMt);
            }
            WorldObjects.OnConnectedQueue.Enqueue(ProbeNewAssets);
            // This will be the runtime state
            cycAccessQueueHandler.NoQueue = false;
        }

        static int lastAssetCount = 0;
        static void ProbeNewAssets()
        {
            ICollection<SimAsset> v = SimAssetStore.GetAssets(AssetType.Unknown);
            //lock (v)
            if (v.Count != lastAssetCount)
            {
                lastAssetCount = v.Count;
                {
                    foreach (SimAsset asset in v)
                    {
                        Master.ToFort(asset);
                    }
                }
            }
        }

        private void VisitAssembly(Assembly assembly)
        {
            foreach (Type t in assembly.GetTypes())
            {

                try
                {
                    VisitType(t);
                }
                catch (Exception e)
                {
                    Exception(e);
                }
            }

        }

        public CycTypeInfo VisitType(Type type)
        {
            CycTypeInfo ctype;
            if (typeFort.TryGetValue(type, out ctype)) return ctype;
            lock (typeFort)
            {
                if (typeFort.TryGetValue(type, out ctype)) return ctype;
                CycTypeInfo cctype;
                if (type.IsSubclassOf(typeof(Asset)))
                {
                    string fn = type.Name;
                    if (fn.StartsWith("Asset")) fn = fn.Substring(5);
                    string col = string.Format("Sim{0}", fn);
                    string comment =
                        String.Format("A spec of #$SimAsset that is a #$PartiallyTangibleTypeByPhysicalFeature from the simulator's type {0} such as #$BlueColor or #$BlowingAKiss animation", type.FullName);
                    fn = string.Format("Sim{0}Fn", fn);
                    if (ClearGridDataBetweenSessions)
                    {
                        cycAccess.converseVoid("(fi-kill (find-or-create-constant \"" + fn + "\"))");
                    }
                    FunctionToCollection(fn, col, comment);
                    assertVocabGafNow(C("genlFuncs"), simFort[fn], simFort["SimAssetFn"]);
                    assertGenls(simFort[col], simFort["SimAsset"]);
                    cctype = typeFort[type] = new CycTypeInfo(simFort[col], type);
                    return cctype;
                }
                else //if (type.Namespace ==null || type.Namespace.StartsWith("OpenMetaverse"))
                {
                    try
                    {
                        if (type.IsEnum) return VisitEnumType(type);
                        else
                        {
                            return null;
                            string name = string.Format("Sim{0}", type.Name);
                            if (name.StartsWith("SimSim"))
                            {
                                name = name.Substring(3);
                            }
                            CycFort cn = C(name);
                            ctype = typeFort[type] = new CycTypeInfo(cn, type);
                            return ctype;
                        }
                    }
                    catch (Exception e)
                    {
                        Exception(e);
                        return null;
                    }
                }
            }
        }

        private CycTypeInfo VisitEnumType(Type type)
        {
            CycTypeInfo ctype;
            if (typeFort.TryGetValue(type, out ctype)) return ctype;
            lock (typeFort)
            {
                if (typeFort.TryGetValue(type, out ctype)) return ctype;
                string name = string.Format("Sim{0}", type.Name);
                if (name.StartsWith("SimSim"))
                {
                    name = name.Substring(3);
                }
                CycFort cn = C(name);
                ctype = typeFort[type] = new CycTypeInfo(cn, type);
                return ctype;
            }
        }

        public void ResultIsa(string fn, string col, string comment, CycFort arg1Isa)
        {
            bool newlyCreated;
            simFort[fn] = createIndividual(fn, comment, "SimVocabularyMt", "ReifiableFunction", out newlyCreated);
            if (newlyCreated)
            {
                assertVocabGafNow(C("resultIsa"), simFort[fn], C(col));
                assertVocabGafNow(C("arg1Isa"), simFort[fn], arg1Isa);
            }

        }

        public void conceptuallyRelated(CycFort a, CycFort b)
        {
            assertVocabGaf(C("conceptuallyCoRelated"), a, b);
        }

        public void FunctionToCollection(string fn, string col, string comment)
        {
            bool newlyCreated;
            CycFort fortFn = simFort[fn] = createIndividual(fn, comment, "SimVocabularyMt", "UnaryFunction", out newlyCreated);
            if (newlyCreated)
            {
                assertIsa(fortFn, C("CollectionDenotingFunction"));
                assertIsa(fortFn, C("ReifiableFunction"));
                CycFort fortCol = simFort[col] = createCollection(col, comment, "SimVocabularyMt", "Collection", null);
                assertGenls(assertCollection(fortCol), C("PartiallyTangibleTypeByPhysicalFeature")); //or isa?
                //not true assertIsa(simFort[fn], C("SubcollectionDenotingFunction"));
                //not true assertIsa(simFort[fn], C("TotalFunction"));
                assertVocabGafNow(C("resultIsa"), fortFn, C("PartiallyTangibleTypeByPhysicalFeature"));
                unassertVocabGafNow(C("resultIsa"), fortFn, C("FirstOrderCollection"));
                assertVocabGafNow(C("resultGenl"), fortFn, fortCol);
            }
        }

        private void unassertVocabGafNow(CycFort a, CycFort b, CycFort c)
        {
            try
            {
                cycAccess.unassertGaf(CycList.list(a, b, c), vocabMt);
            }
            catch (Exception e)
            {
                WhyFailed(CycList.list(a, b, c), vocabMt);
                Exception(e);
            }
        }

        private CycFort createCollection(string col, string comment, string simvocabmt, string isa, string genls)
        {
            CycFort fcol = C(col);
            assertIsa(fcol, CycAccess.collection);
            assertIsa(fcol, C(isa));
            if (genls != null)
            {
                var Cgenls = C(genls);
                assertGenls(fcol, assertCollection(Cgenls));
            }
            assertVocabGaf(CycAccess.comment, fcol, comment);
            return fcol;
        }

        private CycFort assertCollection(CycFort Cgenls)
        {
            assertIsa(Cgenls, CycAccess.collection);
            return Cgenls;
        }

        public void FunctionToIndividual(string fn, string col, string comment)
        {
            bool newlyCreated;
            CycFort fort = simFort[fn] = createIndividual(fn, comment, "SimVocabularyMt", "UnaryFunction", out newlyCreated);
            if (newlyCreated)
            {
                assertIsa(fort, C("IndividualDenotingFunction"));
                assertIsa(fort, C("ReifiableFunction"));
                CycFort fcol = C(col);
                assertIsa(fcol, CycAccess.collection);
                assertVocabGaf(CycAccess.comment, fcol, comment);
                // simFort[col] = createIndividual(col, comment, "SimVocabularyMt", "Collection");
                assertVocabGafNow(C("resultIsa"), fort, fcol);
            }
        }

        public CycFort createIndividual(string term, string comment, string mt, string type, out bool newlyCreated)
        {
            return createTerm(term, comment, mt, type, out newlyCreated);
        }

        // Happens "Now"
        public void assertGenls(CycFort a, CycFort b)
        {
            assertCycListNow(CycList.list(CycAccess.genls, a, b), vocabMt);
        }

        // Happens "Now"
        public void assertIsa(CycFort a, CycFort b)
        {
            if (true)
            {
                assertCycListNow(CycList.list(cycIsa, a, b), vocabMt);
                return;
            }
            cycAccessQueueHandler.Enqueue(() => assertCycListNow(CycList.list(cycIsa, a, b), vocabMt));
        }

        public static void assertVocabGafNow(CycFort a, CycFort b, CycFort c)
        {
            assertCycListNow(CycList.list(a, b, c), vocabMt);
        }
        public static void assertGafNow(CycObject mt, CycFort a, CycFort b, CycFort c)
        {
            assertCycListNow(CycList.list(a, b, c),(CycObject) mt);
        }
        public void assertVocabGaf(CycFort a, CycFort b, CycFort c)
        {
            cycAccessQueueHandler.Enqueue(() => assertVocabGafNow(a, b, c));
        }


        public void assertVocabGaf(CycFort a, CycFort b, string c)
        {
            if (c == null) return;
            cycAccessQueueHandler.Enqueue(() => assertCycListNow(CycList.list(a, b, c), vocabMt));
        }


        public static void assertCycListNow(CycList list, CycObject mt)
        {
            assertCycLExpressionNow(list.cyclifyWithEscapeChars(), mt);
        }

        private void assertCycList(CycList list, CycObject mt)
        {
            assertCycLExpression(list.cyclifyWithEscapeChars(), mt);
        }

        public void assertCycLExpression(string s, CycObject mt)
        {
            string asert = string.Format("(fi-assert '{0} {1})", s, mt.cyclifyWithEscapeChars());
            if (IsAssertDuped(asert)) return;
            RememberAsserted(asert);
            cycAccessQueueHandler.Enqueue(asert,() => assertCycLExpressionNowNoCheck(asert, s, mt));
        }

        static bool IsAssertDuped(string s)
        {
            lock (DuplicateCheck)
            {
                if (DuplicateCheck.Contains(s))
                {
               //     Debug("DUPED: " + s);
                    return true;
                }
                return false;
            }
        }

        static void RememberAsserted(string s)
        {
            lock (DuplicateCheck)
            {
                DuplicateCheck.Add(s);
                int cnt = DuplicateCheck.Count;
                if (cnt % 1000 == 0)
                {
                    if (cnt > 6000)
                    {
                        DuplicateCheck.RemoveRange(0, cnt - 6000);
                    }
                }
            }
        }

        static void assertCycLExpressionNow(string s, CycObject mt)
        {
            string asert = string.Format("(fi-assert '{0} {1})", s, mt.cyclifyWithEscapeChars());
            if (IsAssertDuped(asert)) return;
            RememberAsserted(asert);
            assertCycLExpressionNowNoCheck(asert, s, mt);
        }

        static void assertCycLExpressionNowNoCheck(string asert, string s, CycObject mt)
        {
            try
            {
                if (!cycAccess.converseBoolean(asert))
                {
                    Debug("Assertion failed: " + asert);
                    WhyFailed(s, mt);
                    if (!asert.Contains("icode"))
                        Trace();
                }
            }
            catch (Exception e)
            {
                WhyFailed(s, mt);
                Exception(e);
                return;
            }
        }

        private void WhyFailed(CycList objects, CycObject mt)
        {
            WhyFailed(objects.cyclifyWithEscapeChars(), mt);
        }
        private static void WhyFailed(string s, CycObject mt)
        {
            Debug("HL-EXPLANATION-OF-WHY-NOT-WFF: " + cycAccess.converseObject(string.Format("(HL-EXPLANATION-OF-WHY-NOT-WFF '{0} {1})", s, mt.cyclifyWithEscapeChars())));
            Debug("WHY-NOT-WFF: " + cycAccess.converseObject(string.Format("(WHY-NOT-WFF '{0} {1})", s, mt.cyclifyWithEscapeChars())));
        }


        public CycFort createTerm(string term, string comment, string mt, string type, out bool created)
        {
            CycFort indv;
            if (simFort.TryGetValue(term, out indv))
            {
                created = false;
                return indv;
            }
            bool noQueue = cycAccessQueueHandler.NoQueue;
            lock (simFort)
            {
                if (simFort.TryGetValue(term, out indv))
                {
                    created = false;
                    return indv;
                }
                created = true;
                indv = simFort[term] = C(term);
                cycAccessQueueHandler.NoQueue = true;
                if (type != "Collection")
                {
                    //   assertIsa(indv, C("Individual"));

                    /// Trace();
                }
                assertIsa(indv, C(type));
            }
            {
                cycAccessQueueHandler.NoQueue = false;
                assertVocabGaf(CycAccess.comment, indv, comment);
                cycAccessQueueHandler.NoQueue = noQueue;
                return indv; // simFort[term] = cycAccess.createIndividual(term, comment, mt, type);
            }
        }
        public CycFort createIndividual(string term, string comment, string type, out bool created)
        {
            return createTerm(term, comment, "SimVocabularyMt", type, out created);

        }

        public CycFort FindOrCreateCycFort(SimObject obj)
        {
            CycFort constant;
            lock (cycTerms)
            {
                if (cycTerms.TryGetValue(obj, out constant))
                {
                    // SaveInfoMap(constant, obj);
                    return constant;
                }
                string name;
                string type;
                if (obj is SimAvatar)
                {
                    type = "SimAvatar";
                    name = obj.ID.ToString();
                }
                else
                {
                    type = "SimObject";
                    name = obj.ID.ToString();
                }

                cycTerms[obj] = constant =
                                createIndividualFn(
                                    type, name,
                                    String.Format("{0} {1}", obj.GetName(), obj.ID), "SimVocabularyMt", type);
            }
            {
                if (obj is SimAvatar)
                {
                    string avName = obj.GetName();
                    if (string.IsNullOrEmpty(avName))
                    {
                        string name = WorldObjects.GridMaster.GetUserName(obj.ID);
                        if (!string.IsNullOrEmpty(name))
                        {
                            if (!name.EndsWith("..")) avName = name;
                        }
                    }
                    if (!string.IsNullOrEmpty(avName)) assertVocabGaf(C("fullName"), constant, avName);
                }
                else
                {
                    assertVocabGaf(CycAccess.comment, constant, obj.DebugInfo());
                }
                assertVocabGaf(C("externalTermStrings"), constant, obj.ID.ToString());
                return constant;
            }
        }

        private void SaveInfoMap(CycFort constant, BotMentalAspect obj)
        {
            Dictionary<object, object> fortValues = new Dictionary<object, object>(CompareKeys);
            var im = obj.GetInfoMap();
            lock (hashChanges)
            {
                if (hashChanges.ContainsKey(obj))
                {
                    if (hashChanges[obj] == im.Count)
                    {
                        return;
                    }
                    hashChanges[obj] = im.Count;
                }
                else
                {
                    hashChanges[obj] = im.Count;
                }
            }
            ICollection<NamedParam> infomap = im;
            lock (im)
            {
                infomap = new List<NamedParam>(infomap);
            }

            foreach (NamedParam o in infomap)
            {
                assertEntityData(fortValues, obj, constant, o);
            }
        }

        private string ToPropName(string key)
        {
            if (key.StartsWith("sim"))
            {
                return key;
            }
            if (key.ToLower().StartsWith("sim"))
            {
                return "sim" + key;
            }
            return string.Format("sim{0}{1}", key.Substring(0, 1).ToUpper(), key.Substring(1));

        }

        public CycFort FindOrCreateCycFort(SimAsset simObj)
        {
            CycFort constant;
            if (cycTerms.TryGetValue(simObj, out constant)) return constant;
            lock (cycTerms)
            {
                if (cycTerms.TryGetValue(simObj, out constant)) return constant;
                constant = createIndividualColFn("Sim" + simObj.AssetType, simObj.Name, simObj.DebugInfo(), "SimVocabularyMt", "Sim" + simObj.AssetType);
                cycTerms[simObj] = constant;
            }
            assertVocabGaf(CycAccess.comment, constant, simObj.DebugInfo());
            return constant;
        }
        public CycFort FindOrCreateCycFort(Asset simObj)
        {
            return FindOrCreateCycFort(SimAssetStore.GetSimAsset(simObj));
        }

        public object FindOrCreateCycFort(TaskQueueHandler simObj)
        {
            //Trace();
            return FindOrCreateCycFort(simObj.GetType(), simObj.Name.ToString());
        }

        public object FindOrCreateCycFort(Type type, string named)
        {
            CycTypeInfo ti = VisitType(type);
            string typeToString = TypeToString(type);
            return InstanceNamedFn(named, C(typeToString));
        }

        private string TypeToString(MemberInfo info)
        {
            string regionTypeName = info.Name;
            Type t = info as Type;
            if (t != null)
            {
                if (t.IsArray)
                {
                    return TypeToString(t.GetElementType()) + "Array";
                }
            }
            if (regionTypeName.Contains(".") || regionTypeName.Contains("<"))
            {
                Trace();
            }
            return regionTypeName;
        }


        public object FindOrCreateCycFort(SimAssetStore simObj)
        {
            //Trace();
            return LockToFort(simObj);
        }

        public CycFort FindOrCreateCycFort(SimObjectType simObj)
        {
            lock (cycTerms)
            {
                CycFort constant;
                if (cycTerms.TryGetValue(simObj, out constant)) return constant;
                constant = cycAccess.createIndividual(simObj.AspectName, simObj.ToDebugString(), "SimVocabularyMt",
                                                      "SimObjectType");
                cycTerms[simObj] = constant;
                return constant;
            }
        }

        public static int TimeRep = 0;
        /// <summary>
        ///  (MilliSecondFn 34 (SecondFn 59 (MinuteFn 12 (HourFn 18 (DayFn 14 (MonthFn February (YearFn 1966))))))
        /// </summary>
        /// <param name="dateTime"></param>
        /// <returns></returns>
        public CycFort FindOrCreateCycFort(DateTime dateTime)
        {
            switch (TimeRep)
            {
                case 0:
                    return new CycNart(makeCycList(C("DateFromStringFn"), dateTime.ToString("MMMM dd, yyyy HH:mm:ss.ffffZ")));
                case 1:
                    return new CycNart(
                        makeCycList(C("MilliSecondFn"), FindOrCreateCycFort(dateTime.Millisecond),
                                    makeCycList(C("SecondFn"), FindOrCreateCycFort(dateTime.Second),
                                                makeCycList(C("MinuteFn"), FindOrCreateCycFort(dateTime.Minute),
                                                            makeCycList(C("HourFn"), FindOrCreateCycFort(dateTime.Hour),
                                                                        makeCycList(C("DayFn"),
                                                                                    FindOrCreateCycFort(dateTime.Day),
                                                                                    makeCycList(C("MonthFn"),
                                                                                                C(dateTime.ToString("MMMM")),
                                                                                                makeCycList(
                                                                                                    C("YearFn"),
                                                                                                    FindOrCreateCycFort(
                                                                                                        dateTime.Year)))))))));

                default:
                    return
                        new CycNart(makeCycList(C("NuSketchSketchTimeFn"),
                                                FindOrCreateCycFort((dateTime - baseTime).Ticks / 10000)));
            }
        }

        //public CycFort FindOrCreateCycFort(Permissions simObj)
        //{
        //    return SomeValueFn(""+simObj);
        //}

        private CycFort SomeValueFn(string s)
        {
            return new CycNart(C("SubLStringParameterValueFn"), s);
        }

        public CycFort FindOrCreateCycFort(Primitive simObj)
        {
            return FindOrCreateCycFort(WorldObjects.GridMaster.GetSimObject(simObj));
        }

        public CycFort FindOrCreateCycFort(SimObjectEvent evt)
        {
            Dictionary<object, object> newDictionary = new Dictionary<object, object>(CompareKeys);
            CycFort predUsed;
            if (evt.EventType == SimEventType.DATA_UPDATE)
            {
                return null;
            }
            else
            {
                CycFort constant;
                lock (cycTerms)
                {
                    if (cycTerms.TryGetValue(evt, out constant)) return constant;
                    //   object[] forts = ToForts(simObj.Parameters);
                    cycTerms[evt] = constant = createIndividualFn("SimEvent-" + evt.EventType,
                                                                  evt.ToEventString(),
                                                                  evt.ToString(),
                                                                  "SimVocabularyMt",
                                                                  "SimObjectEvent");
                }
                bool wasNew;
                CycFort col = createTerm(evt.GetVerb().Replace(" ", "-"),
                                               "Event subtype of #$SimObjectEvent", "SimVocabularyMt", "Collection",
                                               out wasNew);
                if (wasNew)
                {
                    assertGenls(col, C("SimObjectEvent"));
                }
                assertIsa(constant, col);
                string[] names = evt.ParameterNames();
                IList<NamedParam> args = evt.Parameters;
                string datePred;
                switch (evt.EventStatus)
                {
                    case SimEventStatus.Start:
                        datePred = (TimeRep == 2 ? "startingPoint" : "startingDate");
                        break;
                    case SimEventStatus.Stop:
                        datePred = (TimeRep == 2 ? "endingPoint" : "endingDate");
                        break;
                    case SimEventStatus.Once:
                    default:
                        datePred = (TimeRep == 2 ? "timePoint" : "dateOfEvent");
                        break;
                }
                assertEntityDataEach(newDictionary, constant, datePred, ToFort(evt.Time), new HashSet<MemberInfo>(), out predUsed);

                int i = -1;
                foreach (NamedParam list in args)
                {
                    i++;
                    object o = list.Value;
                    if (o.GetType().IsEnum)
                    {
                        VisitEnumType(o.GetType());
                    }

                    if (list.Key != null)
                    {
                        string k = list.Key.ToString();
                        if (k == "isa")
                        {
                            if (o is String)
                            {
                                assertIsa(constant, C(o.ToString()));
                                continue;
                            }
                        }
                        if (k == "senderOfInfo" || k == "doneBy")
                        {
                            if (o is String)
                            {
                                o = new CycNart(C("SimAvatarFn"), o.ToString());
                            }
                        }
                        assertEntityDataEach(newDictionary, constant, k, o, newHashSet(args[i].info), out predUsed);
                    }
                    else
                    {
                        o = args[i].Value;
                        if (o.GetType().IsEnum)
                        {
                            ForEachEnumValue(delegate(CycFort f)
                                              {
                                                  assertIsa(constant, f);
                                              }, o);
                        }
                        else
                        {
                            assertEntityDataEach(newDictionary, constant, names[i], o, newHashSet(args[i].info), out predUsed);
                        }
                    }
                }
                return constant;
            }
        }

        private HashSet<MemberInfo> newHashSet(MemberInfo info)
        {
            HashSet<MemberInfo> set = new HashSet<MemberInfo>();
            if (info != null) set.Add(info);
            return set;
        }

        static System.Collections.IEnumerable Unfold(object value, out bool unFolded)
        {
            IList<object> results = new List<object>();
            var type = value.GetType();
            var utype = Enum.GetUnderlyingType(type);
            var values = Enum.GetValues(type);
            if (utype == typeof(byte) || utype == typeof(sbyte) || utype == typeof(Int16) || utype == typeof(UInt16) || utype == typeof(Int32))
            {
                unFolded = true;
                var num = (Int32)Convert.ChangeType(value, typeof(Int32));
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (Int32)Convert.ChangeType(val, typeof(Int32));
                    if (v == 0) continue;
                    if ((v & num) == v)
                    {
                        results.Add(Enum.ToObject(value.GetType(), val));
                    }
                }
            }
            else if (utype == typeof(UInt32))
            {
                unFolded = true;
                var num = (UInt32)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (UInt32)Convert.ChangeType(val, typeof(UInt32));
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(Int64))
            {
                unFolded = true;
                var num = (Int64)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (Int64)Convert.ChangeType(val, typeof(Int64));
                    if (v == 0L)
                    {
                        continue;
                    }
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(UInt64))
            {
                unFolded = true;
                var num = (UInt64)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (UInt64)Convert.ChangeType(val, typeof(UInt64));
                    if (v == 0U)
                    {
                        continue;
                    }
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else
            {
                throw new NotSupportedException();
            }
            return results;
        }

        private delegate void WithEnum(CycFort p);
        private void ForEachEnumValue(WithEnum withValue, object p)
        {
            Type pType = p.GetType();
            if (!CycTypeInfo.IsFlagType(pType))
            {
                CycFort fort = (CycFort)ToFort(p);
                withValue(fort);
                return;
            }
            Array pTypeValues = System.Enum.GetValues(pType);
            Array.Reverse(pTypeValues);

            if (p is byte)
            {
                byte b = (byte)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    byte bv = (byte)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is sbyte)
            {
                sbyte b = (sbyte)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    sbyte bv = (sbyte)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt16)
            {
                ushort b = (UInt16)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    ushort bv = (ushort)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int16)
            {
                short b = (Int16)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    short bv = (short)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt32)
            {
                uint b = (UInt32)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    uint bv = (uint)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int32)
            {
                int b = (Int32)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    int bv = (int)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt64)
            {
                ulong b = (UInt64)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    ulong bv = (ulong)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int64)
            {
                long b = (Int64)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    long bv = (long)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            string s = p.ToString();
            bool unfolded;
            foreach (var unfold in Unfold(p, out unfolded))
            {
                withValue((CycFort)ToFort(unfold));
                //return;
            }
            if (unfolded) return;
            Trace();
            if (p is IConvertible)
            {
                withValue((CycFort)ToFort(p));
                return;
            }

            if (p is Enum)
            {
                withValue((CycFort)ToFort(p));
                return;
            }
            withValue((CycFort)ToFort(p));
        }

        /*
                bool myCompar(Nullable<object> cmp1, Nullable<object> cmp2)
                {
                    return (cmp1.HasValue && cmp2.HasValue) ? (Object.ReferenceEquals(cmp1.Value, cmp2.Value)) : false;
                }

                bool myCompare(object cmp1, object cmp2)// where T : struct
            {
                return cmp1 == cmp2;int s
            }
                */

        private object assertEntityData(Dictionary<object, object> fortValues, object entity, CycFort arg1, NamedParam o)
        {
            fortValues[entity] = arg1;
            CycFort prop;
            return assertEntityDataEach(fortValues, arg1, ToPropName(o.Key.ToString()), o.Value, newHashSet(o.info), out prop);
        }

        private object assertEntityDataEach(Dictionary<object, object> fortValues, CycFort arg1, string name, object arg2Obj, HashSet<MemberInfo> info, out CycFort prop)
        {
            if (IsNeverAssertedName(name))
            {
                prop = null;
                return null;
            }
            prop = createSimProperty(name);
            //CheckConstantName(name);
            if (name.Length > 100)
            {
                Trace();
                return null;
            }

            if (arg2Obj != null)
            {

                object round;
                Type t = arg2Obj.GetType();
                if (IsNeverAsserted(t)) return null;
                if (fortValues.TryGetValue(arg2Obj, out round))
                {
                    if (round != null) defaultAssert(prop, arg1, round);
                    docPredicate(prop, info);
                    if (!IsCycEntity(t) && !t.IsValueType && t != typeof(Primitive.TextureEntryFace))
                    {
                        Trace();
                    }
                    return round;
                }
                if (IsCycEntity(t))
                {
                    object toFort = ToFort(arg2Obj);
                    if (toFort is string && arg2Obj is UUID)
                    {
                        Debug("missing uuidType for " + toFort + " ctx: " + name + " of " + arg1);
                    }
                    fortValues[arg2Obj] = toFort;
                    defaultAssert(prop, arg1, toFort);
                    docPredicate(prop, info);
                    return toFort;
                }
                if (t.IsEnum)
                {
                    CycFort each = prop;
                    ForEachEnumValue(delegate(CycFort f)
                                         {
                                             defaultAssert(each, arg1, f);
                                         }, arg2Obj);
                    docPredicate(prop, info);
                    return null;
                }
                if (!t.IsValueType)
                {
                    //dont visit this ref again
                    fortValues[arg2Obj] = null;
                    //Trace();
                }
                if (IsMultiValued(arg2Obj))
                {
                    // dont visting this Array again
                    fortValues[arg2Obj] = null;
                    //TODO add Indexer to CYC
                    IEnumerable mv = GetMultiValues(arg2Obj);
                    foreach (var e in mv)
                    {
                        if (e == null) continue;
                        object result = assertEntityDataEach(fortValues, arg1, name, e, info, out prop);
                        //if (e != null)                             
                        fortValues[e] = result;
                    }
                    docPredicate(prop, info);
                    return null;
                }
                var infomap = WorldObjects.GetMemberValues(name + "-", arg2Obj);
                docPredicate(prop, info);
                if (infomap.Count < 2)
                {
                    Trace();
                    ///if (t.IsValueType)
                    {
                        object toFort = ToFort(arg2Obj);
                        fortValues[arg2Obj] = toFort;
                        defaultAssert(prop, arg1, toFort);
                        docPredicate(prop, info);
                        return toFort;
                    }
                }
                foreach (NamedParam o in infomap)
                {
                    HashSet<MemberInfo> mis = new HashSet<MemberInfo>(info);
                    String propname = ToPropName(o.Key.ToString());
                    MemberInfo oinfo = o.info;
                    if (oinfo != null) mis.Add(oinfo);
                    CycFort sub;
                    object oValue = o.Value;
                    object newObj = assertEntityDataEach(fortValues, arg1, propname, oValue, mis, out sub);
                    if (oValue != null) fortValues[oValue] = newObj;
                    assertGenlPreds(sub, prop);
                }
                return null;
            }
            else
            {
                return null;
            }
            //  return null;

        }

        private bool IsNeverAsserted(Type t)
        {
            if (t == typeof(SimAssetStore)) return true;
            if (t == typeof(byte[])) return true;
            //if (t == typeof(Wor)) return true;
            return false;
        }
        private bool IsNeverAssertedName(string t)
        {
            if (t == "Store") return true;
            //if (t == typeof(Wor)) return true;
            return false;
        }

        HashSet<CycFort> GenlPredsAsserted = new HashSet<CycFort>();
        private void assertGenlPreds(CycFort sub, CycFort prop)
        {
            lock (GenlPredsAsserted)
            {
                if (!GenlPredsAsserted.Add(sub))
                {
                    return;
                }
            }
            assertVocabGafNow(genlPreds, sub, prop);
        }

        private void defaultAssert(CycFort fort, CycFort constant, object o)
        {

            CycFort mt = CurrentStateMt();
            if (IsCycString(o) ||
                (o is CycNart && ((CycNart)o).getFunctor() != C("TheList")))
            {
                assertCycList(CycList.makeCycList(fort, constant, o), mt);
                return;
            }
            if (o is CycVariable)
            {
                return;
            }
            
            string os = o.ToString();

            if (o is float || o is java.lang.Float || o is double || o is java.lang.Double)
            {
                if (os != "0.0" && os != "1.0")
                {
                    assertCycList(CycList.makeCycList(fort, constant, o), mt);
                    return;
                }
            }
            if (o is int || o is java.lang.Integer || o is long || o is java.lang.Long)
            {
                if (os != "0" && os != "1" && os != "255")
                {
                    assertCycList(CycList.makeCycList(fort, constant, o), mt);
                    return;
                }
            }

            object temp;
            bool added = false;
            CycObject DefaultInstance;
            Dictionary<object, object> DefaultNotNeededInAssert = GetDefaultMapFor(constant, out DefaultInstance);
            lock (DefaultNotNeededInAssert)
            {
                if (!DefaultNotNeededInAssert.TryGetValue(fort, out temp))
                {
                    DefaultNotNeededInAssert.Add(fort, o);
                    added = true;
                }
            }
            if (added)
            {
                assertCycList(CycList.makeCycList(fort, DefaultInstance, o), mt);
                return;
            }
            if (temp.Equals(o))
            {
                return;
            }
            else
            {
                assertCycList(CycList.makeCycList(fort, constant, o), mt);
            }
            return;

        }

        private bool IsCycString(object o)
        {
            return (o is String && ((String)o) != String.Empty);
        }

        private CycFort CurrentStateMt()
        {
            if (assertMt == null)
            {
                assertMt = C("SimInitialStateMt");
                KBTick = 0;
            }
            return assertMt;
        }

        private Dictionary<CycObject, DefaultInstanceMap> DefaultMapsFor = new Dictionary<CycObject, DefaultInstanceMap>();
        private Dictionary<object, object> GetDefaultMapFor(CycFort constant, out CycObject instance)
        {
            if (constant is CycNart)
            {
                constant = ((CycNart)constant).getFunctor();
            }
            DefaultInstanceMap dm;
            lock (DefaultMapsFor)
            {
                if (!DefaultMapsFor.TryGetValue(constant, out dm))
                {
                    dm = DefaultMapsFor[constant] = new DefaultInstanceMap((CycConstant)constant);
                }
            }
            instance = dm.GetDefaultObject();
            return dm.GetMap();
        }


        private void docPredicate(CycFort prop, HashSet<MemberInfo> info)
        {
            if (info == null) return;
            HashSet<MemberInfo> mi;
            lock (PropDoc)
            {
                if (!PropDoc.TryGetValue(prop, out mi))
                {
                    PropDoc[prop] = mi = new HashSet<MemberInfo>(info);
                    foreach (MemberInfo set in info)
                    {
                        docIt(set, prop);
                    }
                }
                else
                {
                    foreach (MemberInfo set in info)
                    {
                        if (mi.Add(set)) docIt(set, prop);
                    }
                }
            }
        }

        private void docIt(MemberInfo info, CycFort prop)
        {
            DocQueue.Enqueue(() =>
                                              {
                                                  if (cycAccessQueueHandler.RealTodo > 3)
                                                  {
                                                      // slow this one down to let the important stuff thru
                                                      Thread.Sleep(5000);
                                                  }
                                                  var doc = GetDocString(info);
                                                  if (!string.IsNullOrEmpty(doc))
                                                  {
                                                      try
                                                      {
                                                          cycAccess.assertGaf(vocabMt, CycAccess.comment, prop, doc);
                                                      }
                                                      catch (Exception e)
                                                      {
                                                          string task = "Error Docing " + info + " " + prop + " " + doc;
                                                          Debug(task);
                                                          Exception(e);
                                                      }
                                                  }
                                              });
        }


        private IEnumerable GetMultiValues(object fort)
        {
            return fort as IEnumerable;
        }

        private bool IsMultiValued(object fort)
        {
            if (fort is IList) return true;
            // if (fort is Array) return true;
            return false;
        }

        static readonly object CreateSimPropertyLock = new object();
        private CycFort createSimProperty(string p)
        {
            lock (CreateSimPropertyLock)
            {
                bool newlyCreated;
                CycFort fort = createIndividual(p, "sim #$BinaryPredicate", "SimVocabularyMt", "SimProperty",
                                                out newlyCreated);
                //TODO remove WORKAROUND opencyc?
                if (newlyCreated) assertIsa(fort, C("BinaryPredicate"));
                return fort;
            }
        }


        public CycFort createIndividualFn(string typename, string name, string comment, string simvocabmt, string simobjecttype)
        {
            bool newlyCreated;
            CycFort fn = createTerm(typename + "Fn", comment,
                                          simvocabmt, "UnaryFunction", out newlyCreated);
            if (newlyCreated)
            {
                assertVocabGafNow(cycIsa, fn, C("ReifiableFunction"));
                assertVocabGafNow(C("resultIsa"), fn, C(simobjecttype));
            }
            CycFort indv;
            bool b = typename.StartsWith("SimEvent");
            if (b) // right now we dont intern Events
            {
                indv = new CycNart(CycList.list(fn, name));
            }
            else
            {
                string nv = name + "-" + typename;
                if (cycTerms.TryGetValue(nv, out indv)) return indv;
                lock (cycTerms)
                {
                    {
                        if (cycTerms.TryGetValue(nv, out indv)) return indv;
                        indv = new CycNart(CycList.list(fn, name));
                        cycTerms[nv] = indv;
                    }
                }
                assertVocabGaf(CycAccess.comment, indv, comment);
            }

            return indv;
        }

        public CycFort createIndividualColFn(string typename, string name, string comment, string simvocabmt, string simobjecttype)
        {
            bool newlyCreated;
            CycFort fn = createTerm(typename + "Fn", comment,
                                          simvocabmt, "UnaryFunction", out newlyCreated);
            if (newlyCreated)
            {
                assertVocabGafNow(cycIsa, fn, C("ReifiableFunction"));
                assertVocabGafNow(cycIsa, fn, C("CollectionDenotingFunction"));
                assertVocabGafNow(C("resultGenl"), fn, assertCollection(C(simobjecttype)));
            }
            CycFort indv;
            bool b = typename.StartsWith("SimEvent");
            if (b) // right now we dont intern Events
            {
                indv = new CycNart(CycList.list(fn, name));
            }
            else
            {
                string nv = name + "-" + typename;
                if (cycTerms.TryGetValue(nv, out indv)) return indv;
                lock (cycTerms)
                {
                    if (cycTerms.TryGetValue(nv, out indv)) return indv;
                    indv = new CycNart(CycList.list(fn, name));
                    cycTerms[nv] = indv;
                }
                assertVocabGaf(CycAccess.comment, indv, comment);
            }
            return indv;
        }

        private bool IndividualExists(CycFort indv)
        {
            return cycAccess.converseInt("(length (term-assertions '" + indv.cyclifyWithEscapeChars() + "))") > 1;
        }

        public bool IsAlreadyFort(object b)
        {
            if (b is CycFort) return true;
            if (b is CycObject) return true;
            if (b is string) return true;
            if (b is java.lang.Object)
            {
                if (b is java.lang.Number)
                {
                    if (b is java.lang.Float) return true;
                    if (b is java.lang.Double) return true;
                    if (b is java.lang.String) return true;
                    if (b is java.lang.Integer) return true;
                    if (b is java.lang.Long) return true;
                    if (b is java.math.BigInteger) return true;
                    if (b is java.math.BigDecimal) return true;
                }
            }
            return false;
        }

        public object[] ToForts(object[] parameters)
        {
            object[] forts = new object[parameters.Length];
            for (int i = 0; i < parameters.Length; i++)
            {
                forts[i] = ToFort(parameters[i]);
            }
            return forts;
        }

        public object ToFort(object parameter)
        {
            if (parameter == null)
            {
                return CYC_NULL;
            }

            if (IsAlreadyFort(parameter)) return parameter;

            CycFort indv;
            //lock (simFort)
            {
                if (simFort.TryGetValue(parameter, out indv))
                {
                    return indv;
                }
            }
            object o = ToFortReal(parameter);
            indv = o as CycFort;
            if (indv != null)
            {
                lock (simFort) simFort[parameter] = indv;
            }
            return o;
        }

        private object ToFortReal(object parameter)
        {
            Type t = parameter.GetType();
            if (t == typeof(Object))
            { //lock type object
                return LockToFort(parameter);
            }
            var conv = GetConverter(t);
            if (conv != null)
            {
                try
                {
                    object obj = conv.Invoke(parameter);
                    return obj;
                }
                catch (Exception e)
                {
                    Exception(e);
                }
            }
            if (t.IsArray)
            {
                Array ar = (Array)parameter;
                return FindOrCreateCycArray(ar, t);
            }
            if (t.IsEnum)
            {
                VisitEnumType(t);
                return C(string.Format("{0}-{1}", t.Name, parameter.ToString()));
            }
            if (t.IsValueType)
            {
                Trace();
                try
                {
                    return C(string.Format("{0}-{1}", t.Name, parameter.ToString()));
                }
                catch (Exception e)
                {
                    Exception(e);
                }
            }

            if (t.IsLayoutSequential)
            {

            }
            Trace();
            Debug("Cant convert " + t + " " + t.StructLayoutAttribute);
            return CYC_NULL;
        }

        private CycFort LockToFort(object parameter)
        {
            Type type = parameter.GetType();
            string parameterName = "" + parameter.GetHashCode();
            return InstanceNamedFn(parameterName, ToFort(type));
        }

        private CycFort InstanceNamedFn(string parameterName, object cycTypeFort)
        {
            CycFort indv;
            if (parameterName == null || cycTypeFort == null)
            {
                Trace();
                return null;
            }
            string parameter = cycTypeFort + "-" + parameterName;
            if (!simFort.TryGetValue(parameter, out indv))
            {
                lock (simFort)
                {
                    if (!simFort.TryGetValue(parameter, out indv))
                    {
                        return simFort[parameter] = new CycNart(C("InstanceNamedFn"), "" + parameterName, cycTypeFort);
                    }
                }
            }
            return indv;
        }


        //public CycObject FindOrCreateCycFort(Avatar.Statistics b)
        //{
        //    return ToStructMembers(b);
        //}

        private CycObject ToStructMembers(ValueType statistics)
        {
            CycList lst = new CycList(C("TheList"));
            Type t = statistics.GetType();
            int found = 0;
            foreach (var fi in t.GetFields())
            {
                if (fi.IsStatic) continue;
                lst.add(CreateCycNart("TheList", fi.Name, fi.GetValue(statistics)));
                found++;
            }
            if (found == 0) throw new NullReferenceException("" + statistics);
            return new CycNart(lst);
        }

        private CycFort FindOrCreateCycArray(Array args, Type t)
        {
            int len = args.GetLength(0);
            if (len > 100)
            {
                // Trace();
                return LockToFort(args);
            }
            return CreateCycNartEnumerable("TheList", args);
        }

        private CycFort CreateCycNartEnumerable(String header, IEnumerable args)
        {
            CycList lst = new CycList(C(header));
            foreach(var e in args)
            {
                var v = ToFort(e);
                if (v is CycVariable)
                {
                    
                }
                lst.add(v);                
            }
            return new CycNart(lst);
        }

        private CycFort CreateCycNart(String header, params object[] args)
        {
            return CreateCycNartEnumerable(header, args);
        }

        public CycFort FindOrCreateCycFort(IDictionary b)
        {
            CycList lst = new CycList(C("TheList"));
            lock (b)
            {
                foreach (var key in b.Keys)
                {
                    object b1 = b[key];
                    var kv = ToFort(b1);
                    lst.add(CreateCycNart("TheList", ToFort(key), kv));
                }
            }
            return new CycNart(lst);
        }


        //public CycFort FindOrCreateCycFort(Vector2 b)
        //{
        //    return new CycNart(makeCycList(C("TheList"),
        //        new java.lang.Float(b.X),
        //        new java.lang.Float(b.Y)));
        //}

        //public CycFort FindOrCreateCycFort(Vector3 b)
        //{
        //    return new CycNart(makeCycList(C("TheList"),
        //        new java.lang.Float(b.X),
        //        new java.lang.Float(b.Y),
        //        new java.lang.Float(b.Z)));
        //}

        //public CycFort FindOrCreateCycFort(Vector4 b)
        //{
        //    return new CycNart(makeCycList(C("TheList"),
        //        new java.lang.Float(b.W),
        //        new java.lang.Float(b.X),
        //        new java.lang.Float(b.Y),
        //        new java.lang.Float(b.Z)));
        //}

        public CycFort FindOrCreateCycFort(Color4 b)
        {
            return new CycNart(makeCycList(C("TheList"),
                new java.lang.Float(b.R),
                new java.lang.Float(b.G),
                new java.lang.Float(b.B),
                new java.lang.Float(b.A)));
        }
        public CycFort FindOrCreateCycFort(Color b)
        {
            string name = b.Name;
            if (!b.IsNamedColor)
            {
                KnownColor kc = b.ToKnownColor();
                if (kc != 0)
                {
                    name = kc.ToString();
                }
            }
            return new CycNart(makeCycList(C("TheList"),
                name,
                new java.lang.Float(b.R),
                new java.lang.Float(b.G),
                new java.lang.Float(b.B),
                new java.lang.Float(b.A)));
        }

        public CycFort FindOrCreateCycFort(byte[] b)
        {
            return FindOrCreateCycArray(b, b.GetType());
        }
        public CycFort FindOrCreateCycFort(int[] b)
        {
            return FindOrCreateCycArray(b, b.GetType());
        }

        public object FindOrCreateCycFort(GridClient client)
        {
            return FindOrCreateCycFort(client.Self.AgentID);
        }

        public object FindOrCreateCycFort(cogbot.BotClient client)
        {
            return FindOrCreateCycFort(client.WorldSystem);
        }

        public object FindOrCreateCycFort(WorldObjects client)
        {
            return FindOrCreateCycFort((SimObject)client.TheSimAvatar);
        }


        public CycObject FindOrCreateCycFort(NullType region)
        {
            string regionTypeName = TypeToString(region.Type);
            return new CycVariable("NULL-" + regionTypeName + "-" + (nullCount++));
        }

        // TODO confirm this will work
        public object FindOrCreateCycFort(ulong b)
        {
            return new java.math.BigInteger("" + b);
        }
        public object FindOrCreateCycFort(Int16 b)
        {
            return new java.lang.Integer(b);
        }

        public object FindOrCreateCycFort(Single b)
        {
            return new java.lang.Float(b);
        }
        public object FindOrCreateCycFort(double b)
        {
            return new java.lang.Double(b);
        }
        public object FindOrCreateCycFort(int b)
        {
            return new java.lang.Integer(b);
        }
        public object FindOrCreateCycFort(long b)
        {
            return new java.lang.Long(b);
        }
        public object FindOrCreateCycFort(byte b)
        {
            return new java.lang.Integer(b);
        }
        public CycFort FindOrCreateCycFort(bool b)
        {
            return b ? C("True") : C("False");
        }

        public bool IsCycEntity(Type ptype)
        {
            if (converters.ContainsKey(ptype)) return true;
            foreach (var t in converters.Keys)
            {
                if (t.IsAssignableFrom(ptype)) return true;
            }
            return (GetConverter(ptype) != null);
        }

        static public readonly Dictionary<Type, Converter<object, object>> converters = new Dictionary<Type, Converter<object, object>>();
        static object PassThruConversion(object inp)
        {
            return inp;
        }

        void LoadConverters()
        {
            lock (converters)
            {
                if (converters.Count > 0) return;
                converters[typeof(string)] = PassThruConversion;
                converters[typeof(CycObject)] = PassThruConversion;
                foreach (var fort in typeof(SimCyclifier).GetMethods())
                {
                    if (fort.Name != "FindOrCreateCycFort") continue;
                    ParameterInfo[] ps = fort.GetParameters();
                    if (ps.Length != 1) continue;
                    Type ptype = ps[0].ParameterType;
                    if (converters.ContainsKey(ptype)) continue;
                    converters[ptype] = MethodInfoToConverter(ptype, this, fort);
                    Debug("Created conversion " + ptype.FullName);
                }
                //converters[typeof(IConvertible)] = PassThruConversion;
            }
        }

        static Converter<object, object> MethodInfoToConverter(Type sane, SimCyclifier cyclifier, MethodInfo info)
        {
            Converter<object, object> ret = delegate(object inp)
                                                {
                                                    if (inp == null) return CYC_NULL;
                                                    if (!sane.IsInstanceOfType(inp))
                                                    {
                                                        throw new ArgumentException("" + inp + " is not " + sane);
                                                    }
                                                    try
                                                    {
                                                        return info.Invoke(cyclifier, new[] { inp });
                                                    }
                                                    catch (Exception e)
                                                    {
                                                        string errr = "" + e;
                                                        Exception(e);
                                                        return errr;
                                                    }
                                                };
            return ret;
        }

        static Converter<object, object> MethodInfoToConverterFunSyntax(Type sane, SimCyclifier cyclifier, MethodInfo info)
        {
            return inp =>
                       {
                           if (inp == null) return CYC_NULL;
                           if (!sane.IsInstanceOfType(inp))
                               throw new ArgumentException("" + inp + " is not " + sane);
                           return info.Invoke(cyclifier, new[] { inp });
                       };
        }

        public static int nullCount = 0;
        static public bool UseQueue = true;
        static readonly Dictionary<CycFort, HashSet<MemberInfo>> PropDoc = new Dictionary<CycFort, HashSet<MemberInfo>>();

        public static CycObject CYC_NULL
        {
            get { return new CycVariable("NULL-" + (nullCount++)); }
        }

        public Converter<object, object> GetConverter(Type type)
        {
            Converter<object, object> converter;
            if (converters.TryGetValue(type, out converter)) return converter;
            lock (converters)
            {
                LoadConverters();
                //one day will be Converter<object, CycFort> converter;

                if (converters.TryGetValue(type, out converter)) return converter;
                foreach (var v in converters)
                {
                    if (type.IsSubclassOf(v.Key))
                    {
                        return converters[type] = v.Value;
                    }
                }
                foreach (var v in converters)
                {
                    if (!v.Key.IsValueType && v.Key.IsAssignableFrom(type))
                    {
                        return converters[type] = v.Value;
                    }
                }
            }
            return null;
        }

        public CycObject FindOrCreateCycFort(Simulator sim)
        {
            return FindOrCreateCycFort(SimRegion.GetRegion(sim));
        }
        public CycFort FindOrCreateCycFort(Vector3 simObjectEvent, Object regionName)
        {
            CycList makeCycList1 = makeCycList(C("PointInRegionFn"),
                                               regionName,
                                               FindOrCreateCycFort(simObjectEvent.X),
                                               FindOrCreateCycFort(simObjectEvent.Y),
                                               FindOrCreateCycFort(simObjectEvent.Z));
            return new CycNart(makeCycList1);
        }

        public static CycList makeCycList(params object[] argsObject)
        {
            CycList list = new CycList(argsObject.Length);
            foreach (object e in argsObject)
            {
                list.add(e);
            }
            return list;
        }

        public CycFort C(string p)
        {
            CycFort c;
            if (simFort.TryGetValue(p, out c)) return c;
            //lock (SimCyclifierLock) // added to guard againt early users
            {
                string sp = CheckConstantName(p);
                lock (simFort)
                {
                    if (simFort.TryGetValue(p, out c)) return c;
                    try
                    {
                        return simFort[p] = simFort[sp] = cycAccess.findOrCreate(sp);
                    }
                    catch (Exception e)
                    {
                        Exception(e);
                        throw e;
                        //return CYC_NULL;
                    }
                }
            }
        }

        private String CheckConstantName(string s)
        {
            if (s.EndsWith("-6") || s.EndsWith("-4") || s.Contains("offsetU"))
            {
                Trace();
            }
            //if (!cycAccess.isValidConstantName(s))
            {
                //  Trace();
            }
            CycConstant colided = cycAccess.constantNameCaseCollision(s);
            if (colided != null)
            {
                // Trace();
                return CheckConstantName(s + "-Pred");
            }
            return s;
        }

        public object FindOrCreateCycFort(UUID region)
        {
            if (region == UUID.Zero) return CYC_NULL;
            object o = WorldObjects.GridMaster.GetObject(region);
            if (o != null && !(o is UUID) && !(o is string)) return ToFort(o);
            return "" + region;
            //return createIndividualFn("SimRegion", region.RegionName, vocabMt.ToString(), "SimRegion " + region, "GeographicalPlace-3D");
        }

        public object FindOrCreateCycFort(UUID[] region)
        {
            if (region == null) return CYC_NULL;
            CycList lst = new CycList(C("TheList"));
            foreach (UUID uuid in region)
            {
                lst.Add(FindOrCreateCycFort(uuid));
            }
            return new CycNart(lst);
            //return createIndividualFn("SimRegion", region.RegionName, vocabMt.ToString(), "SimRegion " + region, "GeographicalPlace-3D");
        }

        public CycObject FindOrCreateCycFort(Type sim)
        {
            CycFort fort;
            if (simFort.TryGetValue(sim, out fort)) return fort;
            while (sim.Name.Contains("<"))
            {
                sim = sim.GetGenericTypeDefinition();
            }

            return C(sim.Name);
        }

        CycFort DeclareGeneric(string name)
        {
            string gname = name;
            CycFort fort;
            if (simFort.TryGetValue(gname, out fort)) return fort;
            lock (simFort)
            {
                if (simFort.TryGetValue(gname, out fort)) return fort;

                fort = simFort[gname] = createCollection(gname, "#$Thing for the simulator",
                                                  "SimVocabularyMt", "Collection", null);
            }
            FunctionToIndividual(gname + "Fn", gname, "A " + name + " in the simulator");
            return fort;
        }

        public CycObject FindOrCreateCycFort(SimGeneric region)
        {
            if (region == null) return CYC_NULL;
            string gname = region.GetGenericName();
            CycFort gtype = DeclareGeneric(gname);
            CycFort obj;
            if (simFort.TryGetValue(region, out obj)) return obj;
            lock (simFort)
            {
                if (simFort.TryGetValue(region, out obj)) return obj;

                obj = simFort[region] = createIndividualFn(gname, region.ID.ToString(), gname + " " + region, vocabMt.ToString(),
                                         gname);
            }
            if (region.Value != null)
            {
                SaveInfoMap(obj, region);
            }
            return obj;
        }

        public CycObject FindOrCreateCycFort(SimGroup region)
        {
            if (region == null) return CYC_NULL;
            CycFort obj;
            lock (simFort)
            {
                if (simFort.TryGetValue(region, out obj)) return obj;

                obj = simFort[region] = createIndividualFn("SimGroup", region.ID.ToString(), "SimGroup " + region, vocabMt.ToString(),
                                         "SimGroup");
            }
            if (!string.IsNullOrEmpty(region.Group.Name))
            {
                SaveInfoMap(obj, region);
            }
            return obj;
        }

        public CycObject FindOrCreateCycFort(SimRegion region)
        {
            if (region == SimRegion.UNKNOWN) return CYC_NULL;
            CycFort obj;
            lock (simFort)
            {
                if (simFort.TryGetValue(region, out obj)) return obj;

                simFort[region] = obj = createIndividualFn("SimRegion", region.RegionName, "SimRegion " + region, vocabMt.ToString(),
                                         "GeographicalPlace-3D");
            }
            if (region.GridInfo.MapImageID != UUID.Zero)
            {
                SaveInfoMap(obj, region);
            }
            return obj;
        }

        public CycObject FindOrCreateCycFort(SimPathStore region)
        {
            if (region.RegionName == "<0,0>") return CYC_NULL;
            return createIndividualFn("SimRegion", region.RegionName, "SimRegion " + region, vocabMt.ToString(), "GeographicalPlace-3D");
        }

        public CycObject FindOrCreateCycFort(SimHeading b)
        {
            if (SimHeading.UNKNOWN == b) return CYC_NULL;
            if (b.IsRegionAttached)
            {
                SimPathStore r = b.PathStore;
                CycObject findOrCreateCycFort = FindOrCreateCycFort(r);
                return FindOrCreateCycFort(b.SimPosition, r.RegionName);
            }
            else
            {
                object arg1 = ToFort(b.GetRoot());
                Vector3 offset = b.GetOffset();
                //return "PointRelativeToFn";
                return new CycNart(makeCycList(C("PointRelativeToFn"), arg1,
                                               FindOrCreateCycFort(offset.X),
                                               FindOrCreateCycFort(offset.Y),
                                               FindOrCreateCycFort(offset.Z)));
            }
        }

        public CycFort FindOrCreateCycFort(Vector3d simObjectEvent)
        {
            SimRegion r = SimRegion.GetRegion(simObjectEvent);
            CycObject findOrCreateCycFort = FindOrCreateCycFort(r);
            return FindOrCreateCycFort(r.GlobalToLocal(simObjectEvent), r.RegionName);
        }

        public object FindOrCreateCycFort(uint b)
        {
            return new java.lang.Long(b);
        }

        public object FindOrCreateCycFort(ushort b)
        {
            return new java.lang.Integer(b);
        }

        public object FindOrCreateCycFort(char b)
        {
            return new java.lang.Character(b);
        }
        public CycObject FindOrCreateCycFort(InventoryFolder folder)
        {
            return FindOrCreateCycFort(WorldObjects.DeclareGeneric("SimInventoryFolder", folder.UUID, null));
        }
        public CycObject FindOrCreateCycFort(InventoryItem folder)
        {
            return FindOrCreateCycFort(WorldObjects.DeclareGeneric("SimInventoryItem", folder.UUID, null));
        }

        public CycFort FindOrCreateCycFort(BotSocialAction botSocialAction)
        {
            Trace();
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(BotObjectAction botObjectAction)
        {
            Trace();
            throw new NotImplementedException();
        }

        //public CycFort FindOrCreateCycFort(Primitive.TextureEntry te)
        //{
        //    return createIndividualFn("SimTextEntry", te);
        //}
        //public CycFort FindOrCreateCycFort(Avatar.AvatarProperties te)
        //{
        //    return createIndividualFn("SimAvatarProperties", te);
        //}

        private CycFort createIndividualFn(string p, object te)
        {
            lock (cycTerms)
            {
                CycFort constant;
                if (cycTerms.TryGetValue(te, out constant)) return constant;
                //   object[] forts = ToForts(simObj.Parameters);
                return cycTerms[te] = createIndividualFn(p, te.ToString(), te.ToString(), "SimVocabularyMt", p);
            }
        }

        public CycFort FindOrCreateCycFort(SimTypeUsage simTypeUsage)
        {
            Trace();
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(SimObjectUsage simObjectUsage)
        {
            Trace();
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(MoveToLocation moveToLocation)
        {
            Trace();
            throw new NotImplementedException();
        }

        public void World_OnSimObject(SimObject obj)
        {
            //  FindOrCreateCycFort(obj);
        }

        static Dictionary<Assembly, XElement> AssmblyXDoics = new Dictionary<Assembly, XElement>();
        public TaskQueueHandler DocQueue = new TaskQueueHandler("Cyc Doc Queue", TimeSpan.FromSeconds(1), true);
        readonly public SimEventSubscriber eventFilter;
        public static double Dist100 = 100;
        static private readonly List<string> DuplicateCheck =  new List<string>();

        public static XElement GetXmlDocMembers(Assembly typeAssembly)
        {
            XElement ele;
            lock (AssmblyXDoics)
                if (!AssmblyXDoics.TryGetValue(typeAssembly, out ele))
                {
                    try
                    {
                        AssmblyXDoics[typeAssembly] = ele = GetXmlDocMembers0(typeAssembly);
                    }
                    catch (Exception e)
                    {
                        Trace();
                        Debug("Cannot doc " + typeAssembly + " " + e);
                        AssmblyXDoics[typeAssembly] = ele = null;
                    }
                }
            return ele;
        }

        public static XElement GetXmlDocMembers0(Assembly typeAssembly)
        {
            var file = GetXmlDocFile(typeAssembly);
            if (file == null) return null;
            XDocument f = XDocument.Load(file.FullName);
            if (f.Root == null) return null;
            return f.Root.Element("members");
        }

        public static XElement GetXmlDocMembers(Type type)
        {
            return GetXmlDocMembers(type.Assembly);
        }


        private static FileInfo GetXmlDocFile(Assembly assembly)
        {
            string assemblyDirPath = Path.GetDirectoryName(assembly.Location);
            string fileName = String.Format("{0}.xml", Path.GetFileNameWithoutExtension(assembly.Location).ToLower());
            foreach (string file in Directory.GetFiles(assemblyDirPath))
            {
                if (Path.GetFileName(file).ToLower().Equals(fileName))
                {
                    return new FileInfo(file);
                }
            }
            Trace();
            Debug("Assebly Doc File not found {0}", fileName);
            return null;
        }

        static string GetMemberId(MemberInfo member)
        {
            char memberKindPrefix = GetMemberPrefix(member);
            string memberName = GetMemberFullName(member);
            return memberKindPrefix + ":" + memberName;
        }

        static char GetMemberPrefix(MemberInfo member)
        {
            return member.GetType().Name
              .Replace("Runtime", "")[0];
        }

        static string GetMemberFullName(MemberInfo member)
        {
            string memberScope = "";
            if (member.DeclaringType != null)
                memberScope = GetMemberFullName(member.DeclaringType);
            else if (member is Type)
                memberScope = ((Type)member).Namespace;

            return memberScope + "." + member.Name;
        }


        private string GetDocString(MemberInfo memberInfo)
        {
            var docMembers = GetXmlDocMembers(memberInfo.DeclaringType.Assembly);
            return GetDocString(docMembers, memberInfo);
        }

        public static string GetDocString(XElement docMembers, MemberInfo info)
        {
            if (docMembers == null) return null;
            string memberId = GetMemberId(info);
            foreach (XElement e in docMembers.Elements("member"))
            {
                var anme = e.Attribute("name");
                if (anme != null)
                    if (anme.Value.ToLower().Substring(1) == memberId.ToLower().Substring(1))
                    {
                        var fn = e.FirstNode as XElement;
                        if (fn != null)
                        {
                            var vv = fn.Value;
                            if (!string.IsNullOrEmpty(vv))
                            {
                                return vv.Trim();
                            }

                        }

                    }
            }
            return null;
        }


        static void Debug(string s, params object[] args)
        {
            if (args == null || args.Length == 0)
            {
                args = new object[] { s };
                s = "{0}";
            }
            DLRConsole.DebugWriteLine(string.Format("[SimCyclifier] {0}", s), args);
        }

        static internal void Exception(Exception e)
        {
            Debug("" + e);
            var ie = e.InnerException;
            if (ie != null && ie != e)
            {
                Exception(ie);
            }
            Trace();
        }

        // just used for a breakpoint in debugger
        internal static void Trace()
        {

        }

    }

    internal class DefaultInstanceMap
    {
        private Dictionary<object, object> defmap;
        private CycFort clazz;
        private SimCyclifier Master;
        public DefaultInstanceMap(CycConstant constant)
        {
            Master = SimCyclifier.Master;
            string n = "SimDefaultInstance-" + constant.name;
            SimCyclifier.cycAccess.converseVoid("(fi-kill (find-or-create-constant \"" + n + "\"))");
            clazz = Master.C(n);
            defmap = new Dictionary<object, object>(SimCyclifier.CompareKeys);
            Master.assertIsa(clazz, Master.C("Thing"));
            Master.assertIsa(clazz, Master.C("HLPrototypicalTerm"));
        }

        public CycObject GetDefaultObject()
        {
            return clazz;
        }

        public Dictionary<object, object> GetMap()
        {
            return defmap;
        }
    }

    internal class CompareKeysImpl : IEqualityComparer<object>
    {
        public bool Equals(object x, object y)
        {
            if (Object.ReferenceEquals(x, y)) return true;
            try
            {
                if (x is System.IConvertible && y is System.IConvertible)
                {
                    if (!Object.Equals(x, y))
                    {
                        //SimCyclifier.Trace();
                        return false;
                    }
                    return true;
                }
                Type tx = x.GetType();
                Type ty = y.GetType();
                if (tx != ty) return false;
                if (!tx.IsValueType)
                {
                    if (Object.Equals(x, y))
                    {
                        SimCyclifier.Trace();
                    }
                    return false;
                }
                if (tx == typeof(NullType))
                {
                    return ((NullType)x).Type == ((NullType)y).Type && ((NullType)x).Inst == ((NullType)y).Inst;
                }
                return Object.Equals(x, y);
            }
            catch (Exception e)
            {
                SimCyclifier.Exception(e);
                return false;
            }

        }

        public int GetHashCode(object obj)
        {
            return obj.GetHashCode();

        }
    }
}
