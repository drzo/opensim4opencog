using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;

using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.NomBank.NomLex
{
    /// <summary>
    /// Provides access to information in NomLex
    /// </summary>
    public class NomLexEngine
    {
        #region static members
        /// <summary>
        /// Gets the index of the parenthesis that balances the parenthesis at the beginning of a piece of text
        /// </summary>
        /// <param name="text">Text to search</param>
        /// <returns>Index of balancing parenthesis</returns>
        private static int IndexOfBalancingParen(string text)
        {
            if (text == null || text.Length == 0 || text[0] != '(')
                throw new Exception("Invalid text");

            return IndexOfBalancingParen(text, 0);
        }

        /// <summary>
        /// Gets the index of the parenthesis that balances the parenthesis at a specified position
        /// </summary>
        /// <param name="text">Text to search</param>
        /// <param name="startParen">Where to start search</param>
        /// <returns>Index of balancing parenthesis</returns>
        private static int IndexOfBalancingParen(string text, int startParen)
        {
            if (text[startParen] != '(')
                throw new Exception("Invalid start");

            // check paren balance
            int parenBal = 0;
            for (int i = startParen; i < text.Length; ++i)
            {
                char c = text[i];
                parenBal += c == '(' ? 1 :
                            (c == ')' ? -1 : 0);
                if (parenBal == 0)
                    return i;
            }

            return -1;
        }
        #endregion

        private Dictionary<string, List<NomLexEntry>> _nounEntries;
        private Set<string> _classes;

        /// <summary>
        /// Gets nouns in NomLex
        /// </summary>
        public IEnumerable<string> Nouns
        {
            get { return _nounEntries.Keys; }
        }

        /// <summary>
        /// Gets classes in NomLex
        /// </summary>
        public IEnumerable<string> Classes
        {
            get { return _classes; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="path">Path to the NomLex dictionary</param>
        public NomLexEngine(string path)
        {
            if (!File.Exists(path))
                throw new FileNotFoundException("Invalid NomLex file:  \"" + path + "\"");

            string nomLex = File.ReadAllText(path);

            // get number of entities
            int numEntries = 0;
            int entryStart = 0;
            while (entryStart >= 0 && entryStart < nomLex.Length)
            {
                // should be on the open paren
                if (nomLex[entryStart] != '(')
                    throw new Exception("Invalid entry");

                // get text for entry
                int entryEnd = IndexOfBalancingParen(nomLex, entryStart);

                // start at next entry
                entryStart = nomLex.IndexOf('(', entryEnd + 1);

                ++numEntries;
            }

            // extract entries
            _nounEntries = new Dictionary<string, List<NomLexEntry>>(numEntries);
            _classes = new Set<string>(false);
            entryStart = 0;
            while (entryStart >= 0 && entryStart < nomLex.Length)
            {
                // should be on the open paren
                if (nomLex[entryStart] != '(')
                    throw new Exception("Invalid entry");

                // get text for entry
                int entryEnd = IndexOfBalancingParen(nomLex, entryStart);
                string entryText = nomLex.Substring(entryStart, entryEnd - entryStart + 1);

                // extract entry
                NomLexEntry entry = ExtractEntry(entryText);

                // get noun from entry
                string noun = entry.Features["orth"].ToString();

                // add entry to list
                _nounEntries.EnsureContainsKey(noun, typeof(List<NomLexEntry>));
                _nounEntries[noun].Add(entry);

                // add to class index
                _classes.Add(entry.Name);

                // start at next entry
                entryStart = nomLex.IndexOf('(', entryEnd + 1);
            }
        }

        /// <summary>
        /// Extracts an entry from text
        /// </summary>
        /// <param name="entryText">Entry text</param>
        /// <returns>NomLexEntry</returns>
        private NomLexEntry ExtractEntry(string entryText)
        {
            // make sure it's a valid entry
            if (entryText == null || entryText.Length == 0 || entryText[0] != '(' || entryText[entryText.Length - 1] != ')' ||
                IndexOfBalancingParen(entryText) != entryText.Length - 1)
                throw new Exception("Invalid entry text");

            // remove surrounding parens
            entryText = entryText.Substring(1, entryText.Length - 2).Trim();

            // get symbol name and create entry
            int symbolLength = entryText.IndexOfAny(new char[] { ' ', ')' });
            if (symbolLength == -1)
                symbolLength = entryText.Length;

            string symbolName = entryText.Substring(0, symbolLength).ToLower();
            NomLexEntry entry = new NomLexEntry(symbolName);

            int featureValuePairsStart = symbolLength + 1;
            if (featureValuePairsStart >= entryText.Length)
                return entry;

            // get text for feature/value pairs
            string featureValuePairs = entryText.Substring(featureValuePairsStart);
            while (featureValuePairs.Length > 0)
            {
                if (featureValuePairs[0] != ':')
                    throw new Exception("Invalid feature start");

                // trim leading ':'
                featureValuePairs = featureValuePairs.Substring(1);
                int nameLength = featureValuePairs.IndexOf(' ', 0);
                string featureName = featureValuePairs.Substring(0, nameLength).ToLower().Trim();

                // get start of value
                int valueStart = nameLength + 1;
                char valueStartChar = featureValuePairs[valueStart];
                int valueEnd = -1;

                // string value
                if (valueStartChar == '"' || valueStartChar == '*')
                {
                    // get end location
                    valueEnd = featureValuePairs.IndexOf(valueStartChar, valueStart + 1);
                    while (true)
                    {
                        // make sure the end character is not escaped
                        if (featureValuePairs[valueEnd - 1] != '\\')
                            break;
                        // find the next end character
                        else
                            valueEnd = featureValuePairs.IndexOf(valueStartChar, valueEnd + 1);
                    }

                    // get value
                    string value = featureValuePairs.Substring(valueStart, valueEnd - valueStart + 1).ToLower().Trim();
                    if (value[0] != valueStartChar || value[value.Length - 1] != valueStartChar)
                        throw new Exception("Invalid string value");

                    // remove start and end characters
                    value = value.Substring(1, value.Length - 2);

                    // create and add value
                    StringFeatureValue val = new StringFeatureValue(value);
                    entry.Features.Add(featureName, val);
                }
                // atomic value
                else if (valueStartChar != '(')
                {
                    valueEnd = featureValuePairs.IndexOfAny(new char[] { ' ', ')' }, valueStart + 1);
                    if (valueEnd == -1)
                        valueEnd = featureValuePairs.Length - 1;

                    // get and add atom
                    string atomValue = featureValuePairs.Substring(valueStart, valueEnd - valueStart + 1).ToLower().Trim();
                    AtomicFeatureValue atom = new AtomicFeatureValue(atomValue);
                    entry.Features.Add(featureName, atom);
                }
                // list of something..either strings or entries
                else
                {
                    FeatureValueList valList = new FeatureValueList();

                    // find the end of the list
                    valueEnd = IndexOfBalancingParen(featureValuePairs, valueStart);
                    string valueText = featureValuePairs.Substring(valueStart, valueEnd - valueStart + 1);

                    // remove parens around list
                    valueText = valueText.Substring(1, valueText.Length - 2).Trim();

                    // get start char
                    valueStartChar = valueText[0];

                    // string list
                    if (valueStartChar == '"')
                    {
                        // get all strings in list
                        string[] stringList = valueText.Split(new char[] { '"' }, StringSplitOptions.RemoveEmptyEntries);
                        foreach (string value in stringList)
                        {
                            string trimmed = value.Trim();
                            if (trimmed == "")
                                continue;

                            StringFeatureValue stringValue = new StringFeatureValue(trimmed);
                            valList.Add(stringValue);
                        }
                    }
                    // entry list
                    else if (valueStartChar == '(')
                        // read all entries
                        while (valueText != "")
                        {
                            // get nested entry text and entry
                            int nestedEntryLength = IndexOfBalancingParen(valueText) + 1;
                            string nestedText = valueText.Substring(0, nestedEntryLength);
                            NomLexEntry nestedEntry = ExtractEntry(nestedText);

                            // add to list
                            valList.Add(nestedEntry);

                            // remove nested entry text
                            valueText = valueText.Substring(nestedEntryLength).Trim();
                        }
                    else
                        throw new Exception("Invalid list character");

                    entry.Features.Add(featureName, valList);
                }

                // remove processed feature
                int newStart = valueEnd + 1;
                if (newStart < featureValuePairs.Length)
                    featureValuePairs = featureValuePairs.Substring(featureValuePairs.IndexOf(':', newStart));
                else
                    featureValuePairs = "";
            }

            return entry;
        }

        /// <summary>
        /// Gets whether or not NomLex contains a nominalization
        /// </summary>
        /// <param name="nominalization">Nominalization to check</param>
        /// <returns>True if the nominalization is present, false otherwise.</returns>
        public bool Contains(string nominalization)
        {
            return _nounEntries.ContainsKey(nominalization);
        }

        /// <summary>
        /// Gets entries for a nominalization
        /// </summary>
        /// <param name="nominalization">Nominalization to get entries for</param>
        /// <returns>Entries</returns>
        public List<NomLexEntry> GetEntries(string nominalization)
        {
            return _nounEntries[nominalization];
        }

        /// <summary>
        /// Tries to get entries for a nominalization
        /// </summary>
        /// <param name="nominalization">Nominalization to get entries for</param>
        /// <param name="entries">Entries, if successfully retrieved, null if given nominalization does not exist</param>
        /// <returns>True if entries were retrieved, false otherwise</returns>
        public bool TryGetEntries(string nominalization, out List<NomLexEntry> entries)
        {
            return _nounEntries.TryGetValue(nominalization, out entries);
        }

        /// <summary>
        /// Gets all nominalizations in a given class
        /// </summary>
        /// <param name="nomLexClass">NomLex class to get nominalizations for</param>
        /// <param name="allowAmbiguousNominalizations">Whether or not to return nominalizations that appear in the given class but also 
        /// appear in another class</param>
        /// <returns>List of nominalizations</returns>
        public Set<string> GetNominalizations(string nomLexClass, bool allowAmbiguousNominalizations)
        {
            Set<string> nouns = new Set<string>(false);
            foreach (string noun in _nounEntries.Keys)
            {
                // get classes for noun
                Set<string> classes = GetClasses(noun);

                // if noun is in desired class...
                if (classes.Contains(nomLexClass))
                    // ...and ambiguity is not a problem
                    if (allowAmbiguousNominalizations || classes.Count == 1)
                        // add to set
                        nouns.Add(noun);
            }

            return nouns;
        }

        /// <summary>
        /// Gets whether or not the given nominalization has an ambiguous NomLex class
        /// </summary>
        /// <param name="nominalization">Nominalization to check</param>
        /// <returns>True if given nominalization has an ambiguous class, false otherwise</returns>
        public bool HasAmbiguousClass(string nominalization)
        {
            return GetClasses(nominalization).Count > 1;
        }

        /// <summary>
        /// Gets NomLex classes for a nominalization
        /// </summary>
        /// <param name="nominalization">Nominalization to get classes for</param>
        /// <returns>NomLex classes</returns>
        public Set<string> GetClasses(string nominalization)
        {
            // get all classes for nominalization
            Set<string> classes = new Set<string>(false);
            foreach (NomLexEntry entry in GetEntries(nominalization))
                classes.Add(entry.Name);

            return classes;
        }

        /// <summary>
        /// Tries to get classes for a nominalization
        /// </summary>
        /// <param name="nominalization">Nominalization to try to get classes for</param>
        /// <param name="classes">Classes of nominalization</param>
        /// <returns>True if classes were found, false otherwise</returns>
        public bool TryGetClasses(string nominalization, out Set<string> classes)
        {
            classes = null;
            if (!Contains(nominalization))
                return false;

            classes = GetClasses(nominalization);

            return true;
        }

        /// <summary>
        /// Gets verbs associated with a nominalization
        /// </summary>
        /// <param name="nominalization">Nominalization to get verbs for</param>
        /// <returns>Verb associated with nominalization</returns>
        public Set<string> GetVerbs(string nominalization)
        {
            if (!Contains(nominalization))
                throw new Exception("Invalid nominalization:  " + nominalization);

            // get verbs
            Set<string> verbs = new Set<string>(false);
            FeatureValue verb;
            foreach (NomLexEntry entry in GetEntries(nominalization))
                if (entry.Features.TryGetValue("verb", out verb))
                    verbs.Add(verb.ToString());

            return verbs;
        }

        /// <summary>
        /// Gets verbs for a list of nominalizations
        /// </summary>
        /// <param name="nominalizations">Nominalizations to get verbs for</param>
        /// <returns>List of verbs</returns>
        public Set<string> GetVerbs(IEnumerable<string> nominalizations)
        {
            Set<string> verbs = new Set<string>(false);
            foreach (string nom in nominalizations)
                foreach (string verb in GetVerbs(nom))
                    verbs.Add(verb);

            return verbs;
        }
    }
}
