using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;
using AltAIMLParser;
using AltAIMLbot.Utils;
using AltAIMLbot;

namespace AltAIMLbot.Database
{
    public class GoogleTranslator
    {
         
        private AltBot TheBot;
        public GoogleTranslator(AltBot bot)
        {
            TheBot = bot;
            TheBot.AddExcuteHandler("gpara", GetParaphrase);
        }

        public static string GetGoogtextStr(string textstr, string language, string tolanguage)
        {

            WebClient web = new WebClient();
            WebHeaderCollection headers = new WebHeaderCollection();
            headers[HttpRequestHeader.ContentType] = "application/x-www-form-urlencoded; charset=utf-8";
            headers[HttpRequestHeader.Referer] = "http://translate.google.cn/";
            web.Headers = headers;
            string text = textstr;
            string url = string.Format("http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q={0}&langpair={1}%7C{2}", text, language, tolanguage);
            byte[] bystr = web.DownloadData(url);
            string urldata = GetText(System.Web.HttpUtility.UrlDecode(bystr, Encoding.UTF8));
            return urldata;

        }

        public static object GetParaphrase(string textstr, Request request)
        {
            string said;
            string tolang;
            if (!TextPatternUtils.SplitOff(textstr, "-", out tolang, out said))
            {
                tolang = "bg";
                said = textstr;
            }
            textstr = GetGoogPraphrase(said, tolang);
            return textstr;
        }

        public static string GetGoogPraphrase(string textstr, string tolanguage)
        {
            textstr = textstr.TrimEnd();
            char lastChar = textstr[textstr.Length - 1];
            if (char.IsLetterOrDigit(lastChar))
                textstr = textstr + ".";

            string foriegn = GetGoogtextStr(textstr, "en", tolanguage);
            var vv = GetGoogtextStr(foriegn, tolanguage, "en");
            return vv;
        }


        public static string GetText(string p)
        {
            //.*{"translatedText":"\([^"]*\)".*
            string[] pSplit = p.Split(new[] {'"'});
            if (pSplit.Length>5) 
                return pSplit[5];
            return p;
        }
    }
}
