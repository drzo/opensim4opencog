using System;
using cogbot.TheOpenSims;
using OpenMetaverse;
using org.opencyc.api;
using org.opencyc.cycobject;
using CUID = org.opencyc.util.UUID;
using Guid=org.opencyc.cycobject.Guid;

namespace CycWorldModule.DotCYC
{
    public class SimCyclifier
    {
        public static bool UseCyc = true;
        readonly private CycAccess cycAccess;
        readonly private CycFort vocabMt;
        readonly private CycFort assertMt;
        readonly private CycFort simObjectFort;
        readonly CycConnectionForm cycConnection;

        public SimCyclifier(CycWorldModule tf)
        {
            if (!UseCyc) return;
            cycConnection = tf.CycConnectionForm;
            cycAccess = cycConnection.getCycAccess();
            assertMt = createIndividual("SimDataMt", "#$DataMicrotheory for the simulator", "UniversalVocabularyMt",
                                                  "DataMicrotheory");
            vocabMt = createIndividual("SimVocabMt", "#$VocabularyMicrotheory for the simulator", "UniversalVocabularyMt",
                                                  "VocabularyMicrotheory");
            simObjectFort = createIndividual("SimObject", "#$SpatiallyDisjointObjectType for the simulator", "SimVocabMt", "Collection");
            cycAccess.assertIsa(simObjectFort, foc("SpatiallyDisjointObjectType"));
            cycAccess.assertGenls(
                createIndividual("SimAvatar", "#$Agent-Generic for the simulator", "SimVocabMt", "Collection"),
                simObjectFort);


        }

        public CycFort createIndividual(string term, string comment, string mt, string type)
        {
            return cycAccess.createIndividual(term, comment, mt, type);

        }
        public CycFort createIndividual(string term, string comment, string type)
        {
            return createIndividual(term, comment, "SimVocabMt", type);

        }

        public CycFort FindOrCreateCycFort(SimObject obj)
        {
            UUID id = obj.Prim.ID;
            string name = id.ToString();
            string type;
            if (obj is SimAvatar)
            {
                type = "SimAvatar";
            }
            else
            {
                type = "SimObject";
            }

            string cname = name + "-" + id;
            //byte[] ba = id.GetBytes();
            ////ulong umsb = Utils.BytesToUInt64(ba);
            ////long msb = umsb;
            ////long lsb = 0L;
            //System.Guid g = new System.Guid();
            ////CUID cycid = CUID.nameUUIDFromBytes(ba);
            CycFort constant = cycAccess.find(name);
            if (constant == null)
            {
                constant = cycAccess.createIndividual(cname, "" + obj, "SimVocabMt", type);
            }
            return constant;
        }

        public CycFort FindOrCreateCycFort(SimAnimation simAnimation)
        {
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(SimObjectType simObjectType)
        {
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(SimObjectEvent simObjectEvent)
        {
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(Vector3 simObjectEvent, SimRegion region)
        {
            return new CycNart(makeCycList(foc("PointIn3DCoordinateSystemFn"), FindOrCreateCycFort(region), simObjectEvent.X, simObjectEvent.Y, simObjectEvent.Z));
        }

        public static CycList makeCycList(params object[] argsObject)
        {
            CycList list = new CycList(argsObject.Length);
            int i = 0;
            foreach (object e in argsObject)
            {
                list.set(i++, e);
            }
            return list;
        }

        public CycFort foc(string p)
        {
            return cycAccess.findOrCreate(p);
        }

        public CycFort FindOrCreateCycFort(SimRegion region)
        {
            return createIndividual(region.RegionName, "SimRegion " + region, "GeographicalPlace-3D");
        }

        public CycFort FindOrCreateCycFort(Vector3d simObjectEvent)
        {
            return FindOrCreateCycFort(SimRegion.GlobalToLocal(simObjectEvent), SimRegion.GetRegion(simObjectEvent));
        }

        internal CycFort FindOrCreateCycFort(BotSocialAction botSocialAction)
        {
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(BotObjectAction botObjectAction)
        {
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(SimTypeUsage simTypeUsage)
        {
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(SimObjectUsage simObjectUsage)
        {
            throw new NotImplementedException();
        }

        public CycFort FindOrCreateCycFort(MoveToLocation moveToLocation)
        {
            throw new NotImplementedException();
        }
    }
}
