using System.Collections.Generic;

namespace RTParser
{
    public class ConversationLog
    {
        static public readonly Dictionary<string, ConversationLog> ConversationLogs = new Dictionary<string, ConversationLog>();

        private string Key;
        private User User1;
        private User User2;
        private ConversationLog(string find, User user1, User user2)
        {

            Key = find;
            User1 = user1;
            User2 = user2;
        }

        public static ConversationLog GetConversationLog(RTPBot robot, string userName1In, string userName2In, bool createIfMissing)
        {
            User user1 = robot.FindOrCreateUser(userName1In);
            User user2 = robot.FindOrCreateUser(userName2In);
            return GetConversationLog(user1, user2, createIfMissing);
        }

        public static ConversationLog GetConversationLog(User user1, User user2, bool createIfMissing)
        {

            // have to order the user names
            if (user1.UserID.CompareTo(user2.UserID) > 0)
            {
                User userMid = user2;
                user2 = user1;
                user1 = userMid;
            }

            string find = user1.UserID + "<->" + user2.UserID;
            lock (ConversationLogs)
            {
                ConversationLog tqh;
                if (!ConversationLogs.TryGetValue(find, out tqh))
                {
                    if (!createIfMissing)
                    {
                        return null;
                    }
                    tqh = new ConversationLog(find, user1, user2);
                    ConversationLogs[find] = tqh;
                    return tqh;
                }
                return tqh;
            }
        }
    }
}