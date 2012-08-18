using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Text;
using System.Net;
using System.Web;

//using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.WebUtil
{
    public class HttpPost : Command, SystemApplicationCommand
    {
        /// <summary>
        /// Posts the supplied data to specified url.
        /// </summary>
        /// <param name="url">The url to post to.</param>
        /// <param name="values">The values to post.</param>
        /// <returns>a string containing the result of the post.</returns>
        static public string DoHttpPost(string url, NameValueCollection values)
        {
            StringBuilder postData = new StringBuilder();
            for (int i = 0; i < values.Count; i++)
            {
                EncodeAndAddItem(ref postData, values.GetKey(i), values[i]);
            }
            HttpWebRequest request = null;
            Uri uri = new Uri(url);
            request = (HttpWebRequest) WebRequest.Create(uri);
            request.Method = "POST";
            request.ContentType = "application/x-www-form-urlencoded";
            request.ContentLength = postData.Length;
            using (Stream writeStream = request.GetRequestStream())
            {
                UTF8Encoding encoding = new UTF8Encoding();
                byte[] bytes = encoding.GetBytes(postData.ToString());
                writeStream.Write(bytes, 0, bytes.Length);
            }

            string result = string.Empty;
            using (HttpWebResponse response = (HttpWebResponse) request.GetResponse())
            {
                using (Stream responseStream = response.GetResponseStream())
                {
                    using (StreamReader readStream = new StreamReader(responseStream, Encoding.UTF8))
                    {
                        result = readStream.ReadToEnd();
                    }
                }
            }
            return result;
        }

        public HttpPost(BotClient Client)
            : base(Client)
        {
            Name = "HttpPost";
        }

        override public void MakeInfo()
        {
            Description = "Post the name/value paires the contents of a URL to return result";
            Details = AddUsage(Name + " url [n1 v1 [n2 v2]]", Description) +
                    AddExample(Name + " http://localhost:5580/ cmd say args hello",
                                    "makes the bot say something");
            Parameters = CreateParams("url", typeof (Uri), "url to post to",
                                                 "name-value", typeof (string), "name value pairs");

        }
        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            String url = args[0];

            NameValueCollection dict = new NameValueCollection();
            for (int i = 1; i < args.Length; i++)
            {
                dict.Add(args[i++], args[i]);
            }
            return Success(DoHttpPost(url, dict));
        }

        /// <summary>
        /// Encodes an item and ads it to the string.
        /// </summary>
        /// <param name="baseRequest">The previously encoded data.</param>
        /// <param name="dataItem">The data to encode.</param>
        /// <returns>A string containing the old data and the previously encoded data.</returns>
        static private void EncodeAndAddItem(ref StringBuilder baseRequest, string key, string dataItem)
        {
            if (baseRequest == null)
            {
                baseRequest = new StringBuilder();
            }
            if (baseRequest.Length != 0)
            {
                baseRequest.Append("&");
            }
            baseRequest.Append(key);
            baseRequest.Append("=");
            baseRequest.Append(HttpUtility.UrlEncode(dataItem));
        }
    }
}