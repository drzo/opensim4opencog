# Introduction #

In artificial intelligence, a procedural reasoning system (PRS) is a framework for constructing real-time reasoning systems that can perform complex tasks in dynamic environments. It is based on the notion of a rational agent or intelligent agent using the belief–desire–intention software model.

A user application is predominately defined, and provided to a PRS system is a set of knowledge areas. Each knowledge area is a piece of procedural knowledge that specifies how to do something, e.g., how to navigate down a corridor, or how to plan a path (in contrast with robotic architectures where the programmer just provides a model of what the states of the world are and how the agent's primitive actions affect them). Such a program, together with a PRS interpreter, is used to control the agent.

The interpreter is responsible for maintaining beliefs about the world state, choosing which goals to attempt to achieve next, and choosing which knowledge area to apply in the current situation. How exactly these operations are performed might depend on domain-specific meta-level knowledge areas. Unlike traditional AI planning systems that generate a complete plan at the beginning, and replan if unexpected things happen, PRS interleaves planning and doing actions in the world. At any point, the system might only have a partially specified plan for the future.

PRS is based on the BDI or belief–desire–intention framework for intelligent agents. Beliefs consist of what the agent believes to be true about the current state of the world, desires consist of the agent's goals, and intentions consist of the agent's current plans for achieving those goals. Furthermore, each of these three components is typically explicitly represented somewhere within the memory of the PRS agent at runtime, which is in contrast to purely reactive systems, such as the subsumption architecture.

And application might be one that defines a narrative process that is an "internal dialog" like a computerized poetry or story generator simply constructing stories. Everything that the inner voice says has to be consistent and hopefully relevant to the rest of the system. The speed in which a system operates even in the real world processing is only at the speed of the internal voice.

# Assumptions #

The programs SAM/PAM by Roger Schank was indeed one of the first most viable starts of AI.  His theory may be viewed as one version of the Language of Thought hypothesis (which Schank calls 'Conceptual Dependency' theory, abbreviated as CD). Although much of his work was based on natural language "understanding".  He defined, at minimum, what the tenants what "understanding" might look like.  From this very start opponents will use the the Chinese Room argument against this language.  I'll ignore this because we've agreed "Artificial" is fine when it comes to machine intelligence. Those who have seen the source code of SAM realize that it is a system who's job is to find a "best fit" on programmed patterns.   What it does is create a language that "best fit" can exist.  We see that to take that initial program really into our world, millions of facts and rules are required to be put into the system.  Before we attempt to add these millions of facts and rules we have to define a very clear meta language (above C.D.).. I believe CycL is the perfect language for this.   And the millions of common sense facts have already been written in CycL!

"Self awareness" means that in order for a program to operate it must be forced to "observe" its execution transcript in the same language in which it interacts with it's environment.  One's own thoughts and plans are just as much part of the world we live in as the outside environment.  The inner environment has many cause-effect rules as the outside does of physics.  We (and the program) strive for control (satisfaction of goals) of the inner world as much as the outside. One definition of "Personality" I learned in school was "The manner of skill in which a person exerts their intentions to the control of their environment" We say a person has a well developed personality when they have found a way to make their environment (others around them) comfortable while they are satisfying their immediate goals.  I believe that in order for a person to function at a high skill level here the must master and win at the games of their inner self.   The concept of "inner self" is what is supposedly so hard to define for AI scientists.  So before defining what "it is" we are better off implementing the framework in which an inner self could operate in.  I think that C.D. representation or CycL might provide sufficient data types for whatever processor we define in this document.

"Inner self" means that we exist in some (game?) world that is separate from the outer environment.  It probably has objects and actions not defined or restricted by spatial coordinates.  It probably has bio-rythemy (dictated by some bio-chemistry) weather like system that is control autonomic-ally and may even be irrelevant to the situation a self-aware being is in.

# More assumptions #

Starting with the restaurant script by Schank we might have an inner script called "the first things we think about at the start of the day".  For some of us in order for items to make it on the list they have to first be qualified by "what is relevant for us to think about", "what do we have time to think about", "what deserves our attention" and "what things do I already think about each morning no mater what".  My point is that we have definite rules (personality) in which we use to keep our inner self compliant.  First this may sound like some phase of goal based planning, but that is not the point of this paragraph the goal is to point out is there is a sense of ontoligising our inner world simply as on the outside.  Imagine how simple it would be to write a flowchart of diagnosing why an engine wont start and realize it'd be just as simple as picking out what the first things we need to think about the start of the day would be.  Again not for a planner but just to understand how we label the rules of such an enterprise.   This meta language can have vague operators such as "this is more important than that" or "i want to talk to this person" and "each day I have to put gas in the car"..  The reason I declare this stuff as "easy" is because if someone was to as "why?"  We'd be able to explain in some ready made language script.  The point where some things are harder to explain is when we've either formed a postulate that cannot be further be simplified ("i am hungry"  "chicken tastes great and I cant explain it") or when the explanation is something that came from the autonomic instant weather system like: "it just came to my mind".   Things will come to mind often because they by tradition just do.

In Sci-fi, we like thinking androids will solve everything in life the same way they would play a game of chess.  We imagine them short circuiting when they encounter unexplainable emotions, situations or people.  So it that AI useful?  I wont say short circuiting is useful but say such an AI is exactly what we all want.  We want a tireless logic machine taking in the big and small picture and computing the most brilliant "act" or "hypothesis" for the moment that it is in.  We want to sit by it's side and explain how we think and feel so that it can inherit those same behaviors.  We hope to do that in English.  Answering many questions it has for us about the exciting new world we have brought it into.  How far is that from a reality?  Initially very very far.  It is important to define the types of questions we'd enjoy answering because those are the exact ones we think "make us human."

# Steps #

  1. Define an outside world model in STRIPS notation using Schanks C.D. language of anything/everything that we'd like the robot to be able to do. "here is how to gather wood and build a fire to achieve warmth" "you want warmth because it makes you feel good"
  1. Simply these models into the most concise featureless version possible. "do X, then do Y to achieve Z"  "wanting Z because it makes you feel A"  "A is good"
  1. Extract the stop-words that are left: "do" "then" "wanting" "makes you" "feel" "is".  Even: "good"
  1. decide the ontology of X,Y,Z,A
  1. write a small system to create new X,Y,Z,A's variables.
  1. Define these mbuild rules in the original way you did step 1 and repeat until you get back to this rule.
  1. Now do the same steps 1-4 for your stop-words.
  1. save this off as a new STRIPS notation
  1. put your rules of legal construction of such sentences back into STRIPS form as so that only valid sentences can be generated. out comes:  "do sit then do sit" ..
  1. find and create ways of stopping such exceptions (make a DSL)
  1. simplify your exceptions language created for detecting
  1. this new "exceptions language" repeat steps 1-8 on it
  1. run the sentence generator again.. When i say "sentence generator": i mean really it is a "rule generator".. hopefully _seemingly_ generating a great numbers of rules.
  1. reduce the X,Y,Z,A into only a small set of literals and see if you can ever make the generator ever stop.  You should be able to.
  1. rewrite the generator to allow yourself to predict exactly how many rules it can produce at any given time if you haven't already done so.
  1. invent new sets of X,Y,Z,As that together make good sense.  Determine what ontological basis you went by. example: GoCabin->Sitting->Comfort->Good  ontologically:  "chairs are comfortable and found in cabins"
  1. Again steps: 1-8..  on step 7.3:  "foundIn" "is" .. remember, step 3 before had found "is".
  1. Are you creating new language yet? or have you been reusing the same language you created the very first time?  Decide that your stop-word generation should not be the same as the first time.. create new versions of "is".  like "feeling\_is\_goal" and "goal\_is\_subgoal"
  1. define a program that will have generated everything you have done up to now.. including automatic forking the definition if "is"...  based in a DSL. Use no more than candidate items per datatype.   (the limit imposed mainly for debugging)
  1. rewrite this program now entirely in a STRIPS format that will generate exactly the kind of template you just created.
  1. use a version of a STRIPS like planner to generate the said templates.
  1. create a framework that pumps these templates into a the generator system that consumes them.
  1. in the framework allow the generators to pump output into another STRIPS like planner.  Decide why the first and 2nd level of planners inputs are incompatible (due to collision?).  If so, make sure collisions don't happen and they are totally separate.  During this process you may have seen some compatibilities.  Find sane ways to leverage those compatibilities.. If none found, worry not.
  1. figure out if you've created an optimization problem (size and scope of data) If so, find solutions shaped like "taxonomic pairs solution".  Decide these "shapes" are in fact tenants of your language.

Taking a break but will resume the steps shortly
Much of this Workflow sounds like writing a prolog program that is domain specific.. then rewriting the program to remove the domain..  In a way it very much is except ontologising is added same way as required in CycL..  Correct, the point of this initial bit is to flex the C.D. representation into something more semantic than what Schank initially taught.  The reason he stayed away from this is he needed to build a working NL representation based on his 7-10 primitives (which are easily anglified to explanation (see XP) ).. You are doing the same.  Except you are designing the the base primitives that have no definition other than to dictate the discourse of representation.  It wasn't the solidness of the primitives that made his work easy, it was the fact that XP (explanation patterns) make absolute sense (they are intended to do so!)  You are going to make a system that can not "think" but in the Chinese room sense is stuck only transcribing things that can make sense.  No mater how many random number generators are used, the system will be _incapable_ of a non lucid thought.  "Thought?" yes, we are building a program that is forced into pretending it is always thinking. The internal representation of Schank's forced it to tell detailed and lucid descriptions of scenes.  The process of explanation of A,B,C,D,E,F proved the listener rather have heard A,B,D,E steps and assumed to create in their own mind the missing pieces.  The user became impressed then they asked how did you get from B->D and the program this time around doesn't leave out C.  I believe the dialog of the mind is a similar implementation.  We have some very long thought chains but only have to deal with partial descriptions at a time.  We are optimized to hide away C and the robot would be well off to emulate that same behavior. _not yet finished explaining..._

Back to some more steps...

In step 5 "write a small system to create new X,Y,Z,A's"  This was not using a dialog based model.   It would be time to explore what a dialog for this system would look like.  It would also be good to next ontoligizing the phases of such dialog.

Dialog phases (pre as well):

  1. A was observed in some way and has not yet been in system.
  1. "I have recognition A.. may we discuss A?"
  1. wait for confirmation
  1. Ask initial categorizations "is A an action i can do?"
> > "is A an object that exists in the world?"
  1. store the results of A

Convert this to a STRIPS notation:
```
(always-rule
 (preconds
  (At ?User1)
  (Unknown ?ConceptA))
(postconds 
  (stable-system)
  (knownAbout ?ConceptA)))
...
```

"speech is a behavoural act" I agree.  Also we can actually have silent speech acts called internal dialog.  Usual internal dialog can be listened to.  Think quietly "I can here myself think" in your own voice. now do it in another persons voice "I can here you think".  Try to have a thought that has no voice.  Now paraphrase that voiceless thought back with your own voice.  My voiced version was "That chair is made out of wood" But i had to pick out something in my environment or some sensory memory that I never bothered voicing: "wow that was a salty steak last night"   Perhaps you can come up with thoughts in  which there are no words for. Generally with enough work you can write some sort of description in which words are used.  This has lead research to decide that all thoughts may be defined in speech acts.  Maybe all thought is a behavour (you are trained to do linguistics.. internal voices give us positive feedback (Pavlov comes in)).  I mean from the very level of composing a lucid thought had to be done via some rules close to linguistics.


_not yet finished explaining..._






