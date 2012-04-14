using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Domain
{
    public class DomainFactory
    {
        public static FOLDomain CrusadesDomain()
        {
            FOLDomain domain = new FOLDomain();
            domain.AddConstant("John");
            domain.AddConstant("Richard");
            domain.AddConstant("England");
            domain.AddConstant("Saladin");
            domain.AddConstant("Crown");

            domain.AddFunction("LeftLegOf");
            domain.AddFunction("BrotherOf");
            domain.AddFunction("EnemyOf");
            domain.AddFunction("LegsOf");

            domain.AddPredicate("King");
            return domain;
        }

        public static FOLDomain KnowsDomain()
        {
            FOLDomain domain = new FOLDomain();
            domain.AddConstant("John");
            domain.AddConstant("Jane");
            domain.AddConstant("Bill");
            domain.AddConstant("Elizabeth");
            domain.AddFunction("Mother");
            domain.AddPredicate("Knows");
            return domain;
        }

        public static FOLDomain WeaponsDomain()
        {

            FOLDomain domain = new FOLDomain();
            domain.AddConstant("West");
            domain.AddConstant("America");
            domain.AddConstant("M1");
            domain.AddConstant("Nono");
            domain.AddPredicate("American");
            domain.AddPredicate("Weapon");
            domain.AddPredicate("Sells");
            domain.AddPredicate("Hostile");
            domain.AddPredicate("Criminal");
            domain.AddPredicate("Missile");
            domain.AddPredicate("Owns");
            domain.AddPredicate("Enemy");

            return domain;
        }

        public static FOLDomain KingsDomain()
        {
            FOLDomain domain = new FOLDomain();
            domain.AddConstant("John");
            domain.AddConstant("Richard");
            domain.AddPredicate("King");
            domain.AddPredicate("Greedy");
            domain.AddPredicate("Evil");
            return domain;
        }

        public static FOLDomain LovesAnimalDomain()
        {
            FOLDomain domain = new FOLDomain();
            domain.AddPredicate("Animal");
            domain.AddPredicate("Loves");
            domain.AddPredicate("Kills");
            domain.AddPredicate("Cat");
            domain.AddConstant("Jack");
            domain.AddConstant("Tuna");
            domain.AddConstant("Curiosity");
            return domain;
        }

        public static FOLDomain RingOfThievesDomain()
        {
            FOLDomain domain = new FOLDomain();
            domain.AddPredicate("Parent");
            domain.AddPredicate("Caught");
            domain.AddPredicate("Friend");
            domain.AddPredicate("Skis");
            domain.AddConstant("Mike");
            domain.AddConstant("Joe");
            domain.AddConstant("Janet");
            domain.AddConstant("Nancy");
            domain.AddConstant("Ernie");
            domain.AddConstant("Bert");
            domain.AddConstant("Red");
            domain.AddConstant("Drew");
            return domain;
        }
    }
}
