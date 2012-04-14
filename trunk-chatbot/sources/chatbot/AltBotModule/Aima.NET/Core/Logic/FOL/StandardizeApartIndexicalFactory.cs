using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL
{
    /// <summary>
    /// This class ensures unique standardize apart indexicals are created. 
    /// </summary>
    public class StandardizeApartIndexicalFactory 
    {
        private static IDictionary<char, int> _assignedIndexicals = new Dictionary<char, int>();

        // For use in test cases, where predictable behavior is expected.
        public static void Flush() 
        {
            lock (_assignedIndexicals) 
            {
                _assignedIndexicals.Clear();
            }
        }

        public static IStandardizeApartIndexical NewStandardizeApartIndexical(char preferredPrefix) 
        {
           if (!(char.IsLetter(preferredPrefix) && char.IsLower(preferredPrefix)))
            {
                throw new ArgumentOutOfRangeException("Preferred prefix :"
                        + preferredPrefix + " must be a valid a lower case letter.");
            }

            var sb = new StringBuilder();
            lock (_assignedIndexicals)
            {
                int currentPrefixCnt = 0;
                if (_assignedIndexicals.ContainsKey(preferredPrefix))
                {
                    currentPrefixCnt = _assignedIndexicals[preferredPrefix] + 1;
                }
                _assignedIndexicals[preferredPrefix] = currentPrefixCnt;
                sb.Append(preferredPrefix);
                for (var i = 0; i < currentPrefixCnt; i++) 
                {
                    sb.Append(preferredPrefix);
                }
            }

            return new StandardizeApartIndexicalImpl(sb.ToString());
        }
    }

    class StandardizeApartIndexicalImpl : IStandardizeApartIndexical 
    {
        private string prefix;
        private int index;

        public StandardizeApartIndexicalImpl(String prefix) 
        {
            this.prefix = prefix;
        }

        public string GetPrefix() 
        {
            return prefix;
        }

        public int GetNextIndex() 
        {
            return index++;
        }
    }
}
