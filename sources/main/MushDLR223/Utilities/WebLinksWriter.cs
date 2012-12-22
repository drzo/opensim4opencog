using System;
using System.IO;
using System.Net;
using System.Text;
using System.Web;
using Action=System.Action;

namespace MushDLR223.Utilities
{
    public class WebLinksWriter : TextWriter
    {
        [ThreadStatic] public static TextWriter tl_WarnWriter = null;
        [ThreadStatic]
        public static int tl_BodyDepth;
        [ThreadStatic]
        public static bool tl_AsHTML;
        [ThreadStatic]
        public static TextWriter tl_MultiPage;
        [ThreadStatic]
        public static string tl_title;

        private static void WriteHtmlPreBody(TextWriter writer, string title)
        {
            if (!tl_AsHTML) return;
            tl_title = title ?? tl_title;
            if (tl_BodyDepth == 0)
            {
                writer.WriteLine("<html><head><title>{0}</title></head><body>", tl_title);
            }
            tl_BodyDepth++;
        }

        private static void WriteHtmlPostBody(TextWriter writer)
        {
            if (!tl_AsHTML) return;
            tl_BodyDepth--;
            if (tl_BodyDepth == 0)
            {
                writer.WriteLine("</body></html>");
            }
        }

        public static TextWriter HtmlStreamWriter(HttpListenerContext context)
        {
            if (tl_MultiPage != null) return tl_MultiPage;
            Stream s = context.Response.OutputStream;
            TextWriter writer = new StreamWriter(s);
            if (tl_AsHTML)
            {
                var writer1 = writer;
                WriteHtmlPreBody(writer, tl_title);
                WebLinksWriter writer2 = WebLinksWriter.EnsureWriteLinksWriter(writer, null);
                writer = writer2;
                writer2.OnClose = () =>
                {
                    tl_AsHTML = true;
                    tl_BodyDepth = 1;
                    WriteHtmlPostBody(writer1);
                };
            }
            return AddWarnWriter(writer);
        }

        private readonly TextWriter w;
        public Action OnClose;
        private bool selfWriting = false;
        public LinkifyArgPred LinkifyArg = LinkifyArgDefault;

        public override void Close()
        {
            if (KeepOpen) return;
            ReallyClose();
        }

        public override void Flush()
        {
            w.Flush();
        }

        protected override void Dispose(bool disposing)
        {
            if (KeepOpen) return;
            ReallyClose();
            if (disposing)
            {
                w.Dispose();
            }
        }

        private bool IsClosed = false;
        private void ReallyClose()
        {
            RemoveWarnWriter(this);
            lock(this)
            {
                if (IsClosed) return;
                IsClosed = true;
            }
            w.Flush();
            if (OnClose != null)
            {
                try
                {
                    OnClose();
                }
                catch (Exception)
                {
                }
            }
            w.Close();
        }

        private bool KeepOpen
        {
            get
            {
                if (tl_MultiPage != null)
                {
                    //Flush();
                    return true;
                }
                return false;
            }
        }

        static public WebLinksWriter EnsureWriteLinksWriter(TextWriter tw, LinkifyArgPred pred)
        {
            if (tw is WebLinksWriter) return (WebLinksWriter)tw;
            return new WebLinksWriter(tw) {LinkifyArg = pred ?? LinkifyArgDefault};
        }

        private WebLinksWriter(TextWriter writer)
        {
            this.w = writer;
        }


        public override void Write(char[] buffer, int index, int count)
        {
            w.Write(buffer, index, count);
        }

        public override void WriteLine(string format)
        {
            WriteLineParms("{0}", EntityFormat(format));
        }
        public override void WriteLine(object arg0)
        {
            WriteLineParms("{0}", arg0);
        }
        public override void WriteLine(string format, object arg0)
        {
            WriteLineParms(format, arg0);
        }
        public override void WriteLine(string format, object arg0, object arg1)
        {
            WriteLineParms(format, arg0, arg1);
        }
        public override void WriteLine(string format, object arg0, object arg1, object arg2)
        {
            WriteLineParms(format, arg0, arg1, arg2);
        }
        public override void WriteLine(string format, params object[] arg)
        {
            var args = LinkifyArgs(arg);
            WriteLineParms(format, args);
        }

        public void WriteLineParms(string format, params object[] arg)
        {
            WriteParms(format, arg);
            w.WriteLine("<br/>");
        }

        public override void Write(string format)
        {
            WriteParms(format);
        }
        public override void Write(object arg0)
        {
            WriteParms("{0}", arg0);
        }
        public override void Write(string format, object arg0)
        {
            WriteParms(format, arg0);
        }
        public override void Write(string format, object arg0, object arg1)
        {
            WriteParms(format, arg0, arg1);
        }
        public override void Write(string format, object arg0, object arg1, object arg2)
        {
            WriteParms(format, arg0, arg1, arg2);
        }
        public override void Write(string format, params object[] arg)
        {
            var args = LinkifyArgs(arg);
            WriteParms(format, args);
        }
        public void WriteParms(string format, params object[] arg)
        {
            var args = LinkifyArgs(arg);
            selfWriting = true;
            try
            {
                if (arg.Length > 0)
                {
                    format = EntityFormat(format);
                }
                else
                {
                    string newForm;
                    if (LinkifyArg(format, out newForm))
                    {
                        format = newForm;
                    }
                }
                w.Write(format, args);
            }
            finally
            {
                selfWriting = false;
            }
        }

        static public string EntityFormat(string format)
        {
            return HttpUtility.HtmlEncode(format);
        }


        public object[] LinkifyArgs(object[] arg)
        {
            for (int i = 0; i < arg.Length; i++)
            {
                object o = arg[i];
                string newVal;
                if (LinkifyArg(o, out newVal))
                {
                    arg[i] = newVal;
                }
            }
            return arg;
        }

        public delegate bool LinkifyArgPred(object o, out string val);

        private static bool LinkifyArgDefault(object o, out string s)
        {
            s = "" + o;
            if (o is Uri)
            {
                s = ((Uri) o).AbsoluteUri;
            }
            if (s.StartsWith("http"))
            {
                string link = s;
                if (link.ToLower().EndsWith(".btx"))
                {
                    link = link.Replace("/behavior/", "/scheduler/");
                    link = link + "?a=info";
                }
                s = string.Format("<a href='{0}'>{1}</a>", link, s);
                return true;
            }
            return false;
        }

        public override Encoding Encoding
        {
            get { return w.Encoding;  }
        }

        public override int GetHashCode()
        {
            return w.GetHashCode();
        }
        public override IFormatProvider FormatProvider
        {
            get
            {
                return w.FormatProvider;
            }
        }
        public override string NewLine
        {
            get
            {
                return w.NewLine;
            }
            set
            {
                w.NewLine = value;
            }
        }

        public static TextWriter WarnWriter
        {
            get { return tl_WarnWriter; }
        }

        public static void RemoveWarnWriter(TextWriter writer)
        {
            if (tl_WarnWriter == writer)
            {
                tl_WarnWriter = null;
            }
        }

        public static TextWriter AddWarnWriter(TextWriter writer)
        {
            tl_WarnWriter = writer;
            return writer;
        }
    }
}