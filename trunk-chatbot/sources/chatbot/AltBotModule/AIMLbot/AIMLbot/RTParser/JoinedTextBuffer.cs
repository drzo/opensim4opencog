using System;

namespace AltAIMLbot
{
    internal class JoinedTextBuffer
    {
        static int count(string s, string t)
        {
            int f = s.IndexOf(t);
            if (f < 0) return 0;
            return 1 + count(s.Substring(f + 1), t);
        }
        private String message = "";
        public void AddMore(string m)
        {
            if (Noise(m)) return;
            if (m.ToLower().StartsWith(message.ToLower()))
            {
                message = AltBot.ReTrimAndspace(m);
                return;
            }
            message += " " + m;
            message = AltBot.ReTrimAndspace(message);
        }

        private bool Noise(string s)
        {
            s = AltBot.ReTrimAndspace(s.ToLower());
            if (s == "um,") return true;
            if (s == "you know,") return true;
            if (message.ToLower().EndsWith(s)) return true;
            return false;
        }

        public bool IsReady()
        {
            if (message.EndsWith(",")) return false;
            if (message.EndsWith(".")) return true;
            if (count(message, " ") > 2) return true;
            return false;
        }

        public string GetMessage()
        {
            return message;
        }
    }
}