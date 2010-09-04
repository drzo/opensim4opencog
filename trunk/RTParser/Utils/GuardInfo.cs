using System.Xml;

namespace RTParser.Utils
{
    public class GuardInfo : OutputInfo
    {
        public GuardInfo(XmlNode template) : base(template)
        {
        }

        public static GuardInfo GetGuardInfo(XmlNode guardnode)
        {
            bool prev = NoInfo;
            try
            {
                NoInfo = false;
                return new GuardInfo(guardnode);
            }
            finally
            {
                NoInfo = prev;
            }
        }
    }
}