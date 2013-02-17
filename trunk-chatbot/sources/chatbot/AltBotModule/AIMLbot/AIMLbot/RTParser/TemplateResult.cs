using System;
using System.Collections.Generic;
using AIMLbot;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;

namespace AltAIMLbot
{
    internal class TemplateResult : ChatSignal
    {
    }
    internal class ChatSignalOverBudget : ChatSignal
    {
        public ChatSignalOverBudget(Request req, string mesg)
            : base(mesg, null)
        {
            request = req;
        }
    }

    public class ChatSignal : Exception
    {
        public bool TemplateSucceeded;
        public bool CreatedOutput;
        public string TemplateOutput;
        public AIMLTagHandlerU TagHandlerU;
        public SubQuery SubQuery;
        public TemplateInfo TemplateInfo;
        public Result result;
        public Request request;

        public virtual bool NeedsAdding { get { return true; } }
        public bool KeepThrowing;
        public ChatLabel FinalTarget;
        public readonly Guid id = Guid.NewGuid();

        protected ChatSignal(string mesg, Exception coz)
            : base(mesg, coz)
        {
        }

        protected ChatSignal()
        {
        }

        public override string ToString()
        {
            return GetType().Name + ":" + id + "\n" + base.ToString() + "\n for " + " " + request;
        }

    }
    public class ChatLabel : RequestDone
    {
        static public readonly List<ChatLabel> Labels = new List<ChatLabel>();
        public ChatLabel()
        {
            lock (Labels) Labels.Add(this);
        }

        public bool IsSignal(ChatSignal signal)
        {
            lock (Labels)
            {
                if (signal.id == id)
                {
                    if (IsFirst(this)) return true;
                    return false;
                }
                lock (Labels)
                {
                    int last = Labels.Count - 1;
                    var r = Labels[last];
                    Labels.RemoveAt(last);
                  //  signal.Label = r;
                    if (signal.KeepThrowing) throw signal;
                }
                return false;
            }
        }

        public static bool IsFirst(ChatLabel label)
        {
            lock (Labels) return (Labels[Labels.Count - 1].id == label.id);
        }

        // override object.Equals
        public override bool Equals(object obj)
        {
            //       
            // See the full list of guidelines at
            //   http://go.microsoft.com/fwlink/?LinkID=85237  
            // and also the guidance for operator== at
            //   http://go.microsoft.com/fwlink/?LinkId=85238
            //

            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }
            return id == ((ChatLabel)obj).id;
        }

        public override int GetHashCode()
        {
            return id.GetHashCode();
        }

        public void PopScope()
        {
            lock (Labels) Labels.RemoveAt(Labels.Count - 1);
        }
    }

    public class RequestDone : ChatSignal
    {
        public override bool NeedsAdding { get { return false; } }
    }
}