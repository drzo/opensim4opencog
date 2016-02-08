# Introduction #

This is a glossary of terms, parts, and pieces of Cogbot.

If you add entries please keep them alphabetized

  * AIML - Artificial Intelligence Markup Language. A patttern matching markup language for encoding responses by a conversational system.

  * AIMLbot - Cogbot's version is an extension to AIML that is 'world aware' - patterns can match only, for example, when a third AV is present or a certain object is nearby, and can include botcmd commands.

  * botcmd  - the base language for interacting with Cogbot. Most other program bindings build on botcmd.

  * BTML - A behavior markup language.  Implementation of behavior trees by Kino Coursey, see http://www.daxtron.com/brainbin/AltBotLangSimp/  This implementation includes an AIML interpreter scheduled to replace ProgramK.

  * ChomskyAIML - A large set of AIML productions with wide applicability, similar to the Alice set.

  * CLI - Microsoft CLI, the bytecode format for .Net. Because LibOMV, the SL protocol library, is in C#, many things have been built atop CLI.  CLI is interpreted by .Net on Windows or Mono on Linux.


  * cogrobot.pl - Prolog constant definitions, including yet another place to define a bot. bin/prolog/cogbot.pl

  * cogrobot.pl - Prolog language bindings for robots. bin/prolog/simulator/cogrobot.pl  Source of most prolog API documentation

  * Cyc - a large ontology created by Cycorp. Cogbot includes a Cyc client. This can be used in two modes. Cyc can be used to improve the performance of AIML, and to keep the ontology up to date with world state information.

  * hyhtn - A hierarchical planner due to  Donghong Liu at the University of Huddersfield. Included with but not integrated with Cogbot.

  * LSL - the scripting language used within Second Life to control objects.

  * Lucene - A java based full text search implementation bundled with Cogbot.

  * Mono - CLI interpreter for Linux

  * OpenCYC - an open source implementation of a subset of the Cyc ontology. See Cyc.

  * OpenSim (aka OpenSimulator) - a server for the SL region agent protocol. A way of making a private version of Second Life.

  * opensim4opencog - what we're building. We're migrating away from this name and towards Cogbot.

  * ProgramK - All Prolog AIML interpreter. Not used in cogbot,

  * Second Life - A large virtual world service operated by Linden Labs

  * simexport - a semi-automated tool for iterating over the contents of a sim

  * SimAvatar - the C# object that represents an avatar known to the simulator, human or bot. You may only want logged on agents, in which case you need to check if the AV has a prim, like this:

```
[1] 58 ?- simAvatar(X),cli_get(X,hasprim,@(true)),cli_to_str(X,S).
X = @'C#638101232',
S = "BinaBot Daxeline" ;
X = @'C#638101728',
S = "Nephrael Rajesh" ;
X = @'C#638111960',
S = "Trollerblades Wasp" ;
false.

```

  * SWICLI - An interface between swi-prolog and Microsoft CLI, the bytecode format for .Net. Because LibOMV, the SL protocol library, is in C#, many things have been built in C# to run CLI on .NET or Mono

  * swipl - swi-Prolog, a prolog implementation with integrated IDE. Cogbot supports swi-Prolog programming

  * WordNet - a semantic thesaurus by Purdue Univ. used by Cogbot to improve AIML matching.