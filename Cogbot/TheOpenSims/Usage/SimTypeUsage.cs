using System;
using System.Collections.Generic;

namespace cogbot.TheOpenSims
{
    public class SimTypeUsage : SimUsage
    {

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public SimTypeUsage(String name):base(name)
        {
            if (name == "Passable")
            {
                throw new ArgumentException();
            }
            
        }

        // the scripting usename name
        public string TextName = "";

        // the maximum distance the user can be away *excluding* the object size
        public int maximumDistance = 1;

        // How much time the effect should take total
        public int totalTimeMS = 14000;  // the time this usage takes

        // Side effects On "use"
        public string IsTransformedOnUse = null; // new type it converts to
        public bool IsDestroyedOnUse;

        //what really happens ofter 1 minute use
        public BotNeeds ChangeActual = new BotNeeds(0.0F);

        // what most users think will happen by default
        public BotNeeds ChangePromise = new BotNeeds(0.0F);

        public HashSet<string> SpecifiedProperties = new HashSet<string>();
        // if true the avatar will attempt to sit on the object for the duration
        public bool UseSit = false;
        // if true the client will attempt to invoke the "touch/grab" in SL for the duration
        public bool UseGrab = false;
        // if "KICK" or another Anim the avatar will play this anim
        public String UseAnim = null;
        /// if set the client will attempt to run
        /// the lisp code that does the animation effects
        public Object LispScript = null; 

        public string ToDebugString()
        {
            String str = UsageName;
            str += " TextName: '" + TextName;
            str += "' totalTimeMS: " + totalTimeMS;
            str += " maximumDistance: " + maximumDistance;
            str += " ChangePromise:" + ChangePromise.ShowNonZeroNeeds();
            str += " ChangeActual:" + ChangeActual.ShowNonZeroNeeds();
            if (SpecifiedProperties.Contains("UseSit")) str += " UseSit: " + UseSit;
            if (SpecifiedProperties.Contains("UseGrab")) str += " UseGrab: " + UseGrab;
            if (SpecifiedProperties.Contains("UseAnim")) str += " UseAnim: " + UseAnim;
            if (SpecifiedProperties.Contains("LispScript")) str += " LispScript: " + LispScript;
            return str;
        }

        public SimTypeUsage OverrideProperties(SimTypeUsage use)
        {
            SimTypeUsage newUse = this;
            foreach (string prop in use.SpecifiedProperties)
            {
                newUse.SpecifiedProperties.Add(prop);
                System.Reflection.FieldInfo fi = newUse.GetType().GetField(prop);
                if (fi.FieldType==typeof(BotNeeds)) continue;
                SimTypeSystem.SetValue(fi, newUse, fi.GetValue(use));
            }
            //if (use.SpecifiedProperties.Contains("TextName"))
            //    newUse.TextName = use.TextName;
            //if (use.SpecifiedProperties.Contains("UseGrab"))
            //    newUse.UseGrab = use.UseGrab;
            //if (use.SpecifiedProperties.Contains("UseSit"))
            //    newUse.UseSit = use.UseSit;
            //if (use.SpecifiedProperties.Contains("LispScript"))
            //    newUse.LispScript = use.LispScript;
            //if (use.SpecifiedProperties.Contains("UseAnim"))
            //    newUse.UseAnim = use.UseAnim;

            newUse.ChangeActual = newUse.ChangeActual.Copy();
            newUse.ChangeActual.AddFrom(use.ChangeActual);
            newUse.ChangePromise = newUse.ChangePromise.Copy();
            newUse.ChangePromise.AddFrom(use.ChangePromise);
            return newUse;
        }

        public override float RateIt(BotNeeds needs)
        {
            return ChangePromise.TotalSideEffect(needs);
        }
    }
}