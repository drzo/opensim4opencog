using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.Propositional.Algorithms
{
    using System.Diagnostics;

    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;
    using Aima.Core.Logic.Propositional.Visitors;
    [Serializable ]
    public class KnowledgeBase {
        public IList<Sentence> Sentences { get; private set; }

        private PEParser parser;

        public KnowledgeBase() {
            this.Sentences = new List<Sentence>();
            parser = new PEParser();
        }

        public void Tell(String aSentence) {
            Sentence sentence = (Sentence) parser.Parse(aSentence);
            if (!(this.Sentences.Contains(sentence))) {
                this.Sentences.Add(sentence);
            }
        }

        public void TellAll(String[] percepts)
        {
            foreach (string t in percepts)
            {
                this.Tell(t);
            }
        }

        public int Size() {
            return this.Sentences.Count;
        }

        public Sentence AsSentence() {
            return LogicUtils.ChainWith("AND", this.Sentences);
        }

        public bool AskWithDpll(String queryString) {
            Sentence query = null, cnfForm = null;
            //TODO: figure out if we really need to eat the exception here, if so add debug compilation condition
            try {
                // just a check to see that the query is well formed
                query = (Sentence) parser.Parse(queryString);
            } catch (Exception e) {
                Debug.WriteLine("error parsing query" + e.Message);
            }

            Sentence kbSentence = this.AsSentence();
            Sentence kbPlusQuery = null;
            if (kbSentence != null) {
                kbPlusQuery = (Sentence) parser.Parse(" ( " + kbSentence + " AND " + queryString + " )");
            } else {
                kbPlusQuery = query;
            }
            try {
                cnfForm = new CNFTransformer().Transform(kbPlusQuery);
                // System.out.println(cnfForm.toString());
            } catch (Exception e) {
                Debug.WriteLine("error converting kb +  query to CNF" + e.Message);

            }
            return new DPLL().DPLLSatisfiable(cnfForm);
        }

        public bool AskWithTtEntails(string queryString) {

            return new TTEntails().TtEntails(this, queryString);
        }

        public override string ToString() {
            if (this.Sentences.Count == 0) {
                return "";
            } else
                return this.AsSentence().ToString();
        }
    }
}
