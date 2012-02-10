using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;
using PrologScriptEngine;
using SbsSW.SwiPlCs;

namespace cogbot.Actions.System
{
    public class SwipCommand : Command, RegionMasterCommand
    {
        private PrologScriptEngine.PrologScriptInterpreter pse = null;
		public SwipCommand(BotClient testClient)
        {
            Name = "swip";
            Description = "runs swi-prolog commands on current sim.";
            Category = CommandCategory.Simulator;
		    Parameters = new[]
		                     {
		                         new NamedParam("prologCode", typeof (string), null)
		                     };
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            int argsUsed;
            string text = args.str;
            bool UsePSE = true;
            try
            {
                if (!UsePSE)
                {
                    text = text.Replace("(", " ").Replace(")", " ").Replace(",", " ");
                    return Client.ExecuteCommand(text);
                }
                if (UsePSE) pse = pse ?? new PrologScriptInterpreter(this);
                Nullable<PlTerm> cmd = null;
                object o = null;
                ManualResetEvent mre = new ManualResetEvent(false);
                Client.InvokeGUI(() =>
                                     {
                                         o = PrologClient.InvokeFromC(() =>
                                                                      {
                                                                          cmd = pse.prologClient.Read(text, null) as Nullable<PlTerm>;
                                                                          o = pse.prologClient.Eval(cmd);
                                                                          mre.Set();
                                                                          return o;
                                                                      });
                                     });
                mre.WaitOne();
                if (o == null) return Success("swip: " + cmd.Value);
                PrologClient.RegisterThread(Thread.CurrentThread);
                return Success("swip: " + cmd.Value + " " + o);
            }
            catch (Exception e)
            {
                string f = e.Message + " " + e.StackTrace;
                Client.WriteLine(f);
                return Failure(f);
            }
        }
    }
}