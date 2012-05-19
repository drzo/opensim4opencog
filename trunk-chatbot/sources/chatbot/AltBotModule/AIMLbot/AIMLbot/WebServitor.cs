using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Reflection;
using System.IO;
using System.Threading;

namespace AltAIMLbot
{
    public class WebServitor
    {
        public static HttpListener listener = new HttpListener();
        public static string startUpPath = System.IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);
        public static Servitor ourServitor = null;
        public static string serverRoot = "http://localhost:8123/";
        public static void beginService(Servitor theServitor)
        {
            if (!HttpListener.IsSupported)
            {
                Console.WriteLine("***** HttpListener is not supported on this platform. *****");
                return;
            }
            ourServitor = theServitor;
            listener.Start();
            listener.Prefixes.Add(serverRoot);
            Thread t = new Thread(new ThreadStart(clientListener));
            t.Start();
        }

        public static void clientListener()
        {
            while(true)
            {
                try
                {
                    HttpListenerContext request=listener.GetContext ();
                    ThreadPool.QueueUserWorkItem(processRequest, request);
                }
                catch(Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        // maybe http://localhost:8888/aiml/zenoaeronaut_resident/bstore/READ_ALICEINWONDERLAND.BTX

        public static void processRequest(object listenerContext)
        {
                var context = (HttpListenerContext)listenerContext;
            try
            {

                switch (context.Request.HttpMethod)
                {
                    case "POST":
                        CREATE(context);
                        break;
                    case "GET":
                        READ(context);
                        break;
                    case "PUT":
                        UPDATE(context);
                        break;
                    case "DELETE":
                        DELETE(context);
                        break;
                    default:
                        ERR(context);
                        break;
                }
                context.Response.Close();

            }
            catch
            {
                context.Response.Close();

            }
            finally
            {
                context.Response.Close();
            }
        }
        public static string  filenameToBehaviorname(string name)
        {
            name = name.Replace(".btx", "");
            name = name.Replace(".BTX", "");
            return name;
        }
        public static void ERR(HttpListenerContext context)
        {
            byte[] msg;
            context.Response.StatusCode = (int)HttpStatusCode.NotFound;
            msg = File.ReadAllBytes(startUpPath + "\\webroot\\error.html");
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);
        }

        public static void CREATE(HttpListenerContext context)
        {
            //POST (INSERT to DB)
            // Put the Behavior in the BTX cache
            byte[] msg;
            string[] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = ourServitor.curBot.myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = ourServitor.curBot.myBehaviors.persistantDirectory;
            string path = Path.Combine(startUpPath, filename);

            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }
            // The message body is posted as bytes. read the bytes
            //byte[] PostData = context.Request.BinaryRead(context.Request.ContentLength);
            Stream bodyStream = context.Request.InputStream;
            Encoding encoding = context.Request.ContentEncoding;

            StreamReader streamReader = new StreamReader(bodyStream, encoding);
            if (context.Request.ContentType != null) 
            {
                string infoContentType = context.Request.ContentType;
                long infoContentLength = context.Request.ContentLength64;
                string infoBody = streamReader.ReadToEnd();
                //Convert the bytes to string using Encoding class
                //string str = Encoding.UTF8.GetString(PostData);
                ourServitor.curBot.myBehaviors.defineBehavior(behaviorName, infoBody);
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
                // return the posted contents if any
            }
            else
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
            }
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);

        }

        public static void READ(HttpListenerContext context)
        {
            //GET (READ from DB)
            string [] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = ourServitor.curBot.myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = ourServitor.curBot.myBehaviors.persistantDirectory;
            //string path = Path.Combine(startUpPath, filename);
            //string path = Path.Combine(startUpPath, context.Request.RawUrl);
            string path = "." + justURL;
            string query = context.Request.QueryString["q"];
            string action = context.Request.QueryString["a"];
            
            if (path.Contains("./scheduler/"))
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                using (Stream s = context.Response.OutputStream)
                using (StreamWriter writer = new StreamWriter(s))
                    ourServitor.myScheduler.performAction(writer,action,query, behaviorName);
                return;
            }

            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            byte[] msg;
                if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }

            if (!File.Exists(path))
            {

                if (path.EndsWith("/list/"))
                {
                    context.Response.StatusCode = (int)HttpStatusCode.OK;
                    //context.Response.ContentLength64 = 0;

                    string [] fileList = Directory.GetFiles(behaviorDir);
                    string fileListString = "";
                    string uriPrefix = serverRoot+"behavior/";
                    using (Stream s = context.Response.OutputStream)
                    using (StreamWriter writer = new StreamWriter(s))
                    {
                        foreach (string f in fileList)
                        {
                            string fout = uriPrefix + Path.GetFileName(f) + "\n";
                            //fileListString += fout + "\n";
                            writer.Write(fout);
                            //writer.Flush();
                        }
                        //msg = System.Text.Encoding.ASCII.GetBytes(fileListString);
                        writer.Close();
                    }
                    return;
                }
                else
                {
                    Console.WriteLine("Client requesed nonexistant file {0} => {1}, sending error ...",
                        justURL, path);
                    context.Response.StatusCode = (int)HttpStatusCode.NotFound;
                    msg = File.ReadAllBytes(startUpPath + "\\webroot\\error.html");
                }
            }
            else
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
            }
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);

        }
        public static void UPDATE(HttpListenerContext context)
        {
            //PUT (Update DB)
            byte[] msg;
            string[] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string path = Path.Combine(startUpPath, filename);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = ourServitor.curBot.myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = ourServitor.curBot.myBehaviors.persistantDirectory;
            string query = context.Request.QueryString["q"];
            string action = context.Request.QueryString["a"];
            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }
            // The message body is posted as bytes. read the bytes
            //byte[] PostData = context.Request.BinaryRead(context.Request.ContentLength);
            Stream bodyStream = context.Request.InputStream;
            Encoding encoding = context.Request.ContentEncoding;

            StreamReader streamReader = new StreamReader(bodyStream, encoding);
            if (context.Request.ContentType != null)
            {
                string infoContentType = context.Request.ContentType;
                long infoContentLength = context.Request.ContentLength64;
                string infoBody = streamReader.ReadToEnd();
                //Convert the bytes to string using Encoding class
                //string str = Encoding.UTF8.GetString(PostData);
                ourServitor.curBot.myBehaviors.defineBehavior(behaviorName, infoBody);
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
                // return the posted contents if any
            }
            else
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
            }
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);

        }
        public static void DELETE(HttpListenerContext context)
        {
            //DELETE
            string[] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string path = Path.Combine(startUpPath, filename);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = ourServitor.curBot.myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = ourServitor.curBot.myBehaviors.persistantDirectory;
            string query = context.Request.QueryString["q"];
            string action = context.Request.QueryString["a"];
            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }


        }


    }
}
