using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.KB
{
    using Aima.Core.Logic.FOL.Domain;
    using Aima.Core.Logic.FOL.Inference;

    public class FOLKnowledgeBaseFactory
    {

        public static FOLKnowledgeBase CreateKingsKnowledgeBase(
                IInferenceProcedure infp)
        {
            FOLKnowledgeBase kb = new FOLKnowledgeBase(DomainFactory.KingsDomain(),
                    infp);
            kb.tell("((King(x) AND Greedy(x)) => Evil(x))");
            kb.tell("King(John)");
            kb.tell("King(Richard)");
            kb.tell("Greedy(John)");

            return kb;
        }

        public static FOLKnowledgeBase CreateWeaponsKnowledgeBase(
                IInferenceProcedure infp)
        {
            FOLKnowledgeBase kb = new FOLKnowledgeBase(DomainFactory
                    .WeaponsDomain(), infp);
            kb
                    .tell("( (((American(x) AND Weapon(y)) AND Sells(x,y,z)) AND Hostile(z)) => Criminal(x))");
            kb.tell(" Owns(Nono, M1)");
            kb.tell(" Missile(M1)");
            kb.tell("((Missile(x) AND Owns(Nono,x)) => Sells(West,x,Nono))");
            kb.tell("(Missile(x) => Weapon(x))");
            kb.tell("(Enemy(x,America) => Hostile(x))");
            kb.tell("American(West)");
            kb.tell("Enemy(Nono,America)");

            return kb;
        }

        public static FOLKnowledgeBase CreateLovesAnimalKnowledgeBase(
                IInferenceProcedure infp)
        {
            FOLKnowledgeBase kb = new FOLKnowledgeBase(DomainFactory
                    .LovesAnimalDomain(), infp);

            kb
                    .tell("FORALL x (FORALL y (Animal(y) => Loves(x, y)) => EXISTS y Loves(y, x))");
            kb
                    .tell("FORALL x (EXISTS y (Animal(y) AND Kills(x, y)) => FORALL z NOT(Loves(z, x)))");
            kb.tell("FORALL x (Animal(x) => Loves(Jack, x))");
            kb.tell("(Kills(Jack, Tuna) OR Kills(Curiosity, Tuna))");
            kb.tell("Cat(Tuna)");
            kb.tell("FORALL x (Cat(x) => Animal(x))");

            return kb;
        }

        public static FOLKnowledgeBase CreateRingOfThievesKnowledgeBase(
                IInferenceProcedure infp)
        {
            var kb = new FOLKnowledgeBase(DomainFactory.RingOfThievesDomain(), infp);

            // s(x) => ~c(x) One who skis never gets caught
            kb.tell("(Skis(x) => NOT(Caught(x)))");

            // c(x) => ~s(x) Those who are caught don't ever ski
            kb.tell("(Caught(x) => NOT(Skis(x)))");

            // p(x,y) & c(y) => s(x) Jailbird parents have skiing kids
            kb.tell("((Parent(x,y) AND Caught(y)) => Skis(x))");

            // s(x) & f(x,y) => s(y) All friends ski together
            kb.tell("(Skis(x) AND Friend(x,y) => Skis(y))");
            
            // f(x,y) => f(y,x) Friendship is symmetric
            kb.tell("(Friend(x,y) => Friend(y,x))");
            
            // FACTS
            // 1. { p(Mike,Joe) } Premise
            kb.tell("Parent(Mike, Joe)");
            
            // 2. { p(Janet,Joe) } Premise
            kb.tell("Parent(Janet,Joe)");
            
            // 3. { p(Nancy,Mike) } Premise
            kb.tell("Parent(Nancy,Mike)");
            
            // 4. { p(Ernie,Janet) } Premise
            kb.tell("Parent(Ernie,Janet)");
            
            // 5. { p(Bert,Nancy) } Premise
            kb.tell("Parent(Bert,Nancy)");
            
            // 6. { p(Red,Ernie) } Premise
            kb.tell("Parent(Red,Ernie)");
            
            // 7. { f(Red,Bert) } Premise
            kb.tell("Friend(Red,Bert)");
            
            // 8. { f(Drew,Nancy) } Premise
            kb.tell("Friend(Drew,Nancy)");
            
            // 9. { c(Mike) } Premise
            kb.tell("Caught(Mike)");
            
            // 10. { c(Ernie) } Premise
            kb.tell("Caught(Ernie)");

            return kb;
        }

        // Note: see -
        // http://logic.stanford.edu/classes/cs157/2008/lectures/lecture15.pdf
        // slide 12 for where this test example was taken from.
        public static FOLKnowledgeBase CreateAbcEqualityKnowledgeBase(
                IInferenceProcedure infp, bool includeEqualityAxioms)
        {
            var domain = new FOLDomain();
            domain.AddConstant("A");
            domain.AddConstant("B");
            domain.AddConstant("C");

            var kb = new FOLKnowledgeBase(domain, infp);

            kb.tell("B = A");
            kb.tell("B = C");

            if (includeEqualityAxioms)
            {
                // Reflexivity Axiom
                kb.tell("x = x");

                // Symmetry Axiom
                kb.tell("(x = y => y = x)");

                // Transitivity Axiom
                kb.tell("((x = y AND y = z) => x = z)");
            }

            return kb;
        }

        // Note: see -
        // http://logic.stanford.edu/classes/cs157/2008/lectures/lecture15.pdf
        // slide 16,17, and 18 for where this test example was taken from.
        public static FOLKnowledgeBase CreateAbcdEqualityAndSubstitutionKnowledgeBase(
                IInferenceProcedure infp, bool includeEqualityAxioms)
        {
            var domain = new FOLDomain();
            domain.AddConstant("A");
            domain.AddConstant("B");
            domain.AddConstant("C");
            domain.AddConstant("D");
            domain.AddPredicate("P");
            domain.AddFunction("F");

            var kb = new FOLKnowledgeBase(domain, infp);

            kb.tell("F(A) = B");
            kb.tell("F(B) = A");
            kb.tell("C = D");
            kb.tell("P(A)");
            kb.tell("P(C)");

            if (includeEqualityAxioms)
            {
                // Reflexivity Axiom
                kb.tell("x = x");

                // Symmetry Axiom
                kb.tell("(x = y => y = x)");

                // Transitivity Axiom
                kb.tell("((x = y AND y = z) => x = z)");

                // Function F Substitution Axiom
                kb.tell("((x = y AND F(y) = z) => F(x) = z)");

                // Predicate P Substitution Axiom
                kb.tell("((x = y AND P(y)) => P(x))");
            }

            return kb;
        }
    }
}
