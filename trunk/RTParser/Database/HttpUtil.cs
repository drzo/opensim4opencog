using System.Net;
using System.Text;

namespace RTParser.Database
{
    public static class HttpUtil
    {
        public static string GetUrlData(string url)
        {
            WebClient web = new WebClient();
            WebHeaderCollection headers = new WebHeaderCollection();
            headers[HttpRequestHeader.ContentType] = "application/x-www-form-urlencoded; charset=utf-8";
            headers[HttpRequestHeader.Referer] = "http://translate.google.cn/";
            web.Headers = headers;
            byte[] bystr = web.DownloadData(url);
            return System.Web.HttpUtility.UrlDecode(bystr, Encoding.UTF8);
        }

    }
}