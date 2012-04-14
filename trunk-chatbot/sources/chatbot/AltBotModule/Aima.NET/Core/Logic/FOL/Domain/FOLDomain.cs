using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Domain
{
    public class FOLDomain 
    {
        public ISet<string> Constants { get; private set; }

        public ISet<string> Functions { get; private set; }
        
        public ISet<string> Predicates { get; private set; }

        private int skolemConstantIndexical;
        private int skolemFunctionIndexical;
        private int answerLiteralIndexical;

        public event EventHandler<FOLDomainAnswerLiteralAddedEventArgs> FOLDomainAnswerLiteralAddedEvent;

        public event EventHandler<FOLDomainSkolemConstantAddedEventArgs> FOLDomainSkolemConstantAddedEvent;
        
        public event EventHandler<FOLDomainSkolemFunctionAddedEventArgs> FOLDomainSkolemFunctionAddedEvent;
        
        public FOLDomain() 
        {
            this.Constants = new HashedSet<string>();
            this.Functions = new HashedSet<string>();
            this.Predicates = new HashedSet<string>();
        }

        public FOLDomain(FOLDomain toCopy) : this(toCopy.Constants, toCopy.Functions, toCopy.Predicates)
        {
        }

        public FOLDomain(ISet<string> constants, ISet<string> functions,ISet<string> predicates) 
        {
            this.Constants = new HashedSet<String>(constants);
            this.Functions = new HashedSet<String>(functions);
            this.Predicates = new HashedSet<String>(predicates);
        }

        public void AddConstant(string constant) 
        {
            this.Constants.Add(constant);
        }

        public string AddSkolemConstant() {

            string sc = null;
            do 
            {
                sc = "SC" + (skolemConstantIndexical++);
            } 
            while (Constants.Contains(sc) || Functions.Contains(sc)
                    || Predicates.Contains(sc));

            AddConstant(sc);
            if (this.FOLDomainSkolemConstantAddedEvent != null)
            {
                this.FOLDomainSkolemConstantAddedEvent(this, new FOLDomainSkolemConstantAddedEventArgs(sc));
            }

            return sc;
        }

        public void AddFunction(string function) 
        {
            this.Functions.Add(function);
        }

        public string AddSkolemFunction() 
        {
            string sf;
            do 
            {
                sf = "SF" + (skolemFunctionIndexical++);
            } 
            while (Constants.Contains(sf) || Functions.Contains(sf)
                    || Predicates.Contains(sf));

            this.AddFunction(sf);
            if (this.FOLDomainSkolemFunctionAddedEvent != null)
            {
                this.FOLDomainSkolemFunctionAddedEvent(this, new FOLDomainSkolemFunctionAddedEventArgs(sf));
            }

            return sf;
        }

        public void AddPredicate(string predicate) 
        {
            Predicates.Add(predicate);
        }

        public string AddAnswerLiteral() 
        {
            string al;
            do 
            {
                al = "Answer" + (this.answerLiteralIndexical++);
            }
            while (this.Constants.Contains(al) || this.Functions.Contains(al) || this.Predicates.Contains(al));

            this.AddPredicate(al);
            
            if (this.FOLDomainAnswerLiteralAddedEvent != null)
            {
                this.FOLDomainAnswerLiteralAddedEvent(this, new FOLDomainAnswerLiteralAddedEventArgs(al));
            }

            return al;
        }
    }
}
