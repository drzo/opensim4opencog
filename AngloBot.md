#Design for a hillpeople world understanding authoring system

# Introduction #

> Cogbot is a server written for .NET that connects several avatars
> acting as "bots" to OpenSim or Second Life. The system is intended
> to provide basic features programmers need to populate a prebuilt
> virtual environments with non player characters doing "interesting"
> things.

Interesting, in human terms, means that the actions have meaning and make sense.  This requires some 'intelligence' tools.

As of this writing we've taken the Hill People example as far as we can without a more powerful tool. AngloBot will be that tool.

# Name #

Anglobot is a working title.

# Manifesto #

Overarching design principles:
  * Development is a total activity. We want a system that supports running test runs, updating, and debugging as an integrated, humane activity.
  * Better tools are needed to build a palace than a garden shed.
  * Formal power and semantics are interesting, but so is ease of use.
  * We appreciate the design decisions that lead to Inform 7, but recognize that their design solutions are for their domain, not ours.
  * Our users come first
  * Including technical details and requirements, making the learning curve more formidable, affects not only non/less technical users, but busy technical users as well. Complexity has a cost.
  * To understand is to relate actions to utility. This is the Papertian view and Gibson's ecological view.
  * PDDL is our machine code.
  * Our interpreter is opinionated. It understands some fundamental things that are awkward to encode in the user's language, e.g. that carrying a thing means it moves with the carrier.

# What we tell the system #

  * Snippets - sections of a plan that a planner can combine to create plans by HTN.
  * Ontology - facts about the nature of the world, in a form that can be converted to snippets.
  * Utility - facts about the value of doing things.

## Snippets ##


## Ontology ##


## Utility ##


## IDE ##

Skein - like Inform 7  (how do we 'reset')?

GUI tool to observe sysvars, botvars

## Other implementations that might be useful ##

http://www.agentfactory.com/index.php/Main_Page

http://jason.sourceforge.net/Jason/Jason.html

http://www.youtube.com/embed/r38b_b7IOkc

## the convo on skype ##

```
ok, confusion solved on voice.
[11:46:23 PM] Anne Ogborn: we;'ve been speaking of Inform 7 as a model for how to make Anglobot
[11:46:42 PM | Edited 11:46:54 PM] Douglas R. Miles: recap: when i said world rezzing . i was referning to the size of the DSL arround world state vs DSL size arround defining bot actions
[11:46:49 PM] Anne Ogborn: By that, I've been meaning more the development cycle and tools
[11:47:12 PM] Anne Ogborn: Inform 7 has two primary functions - define initial world state and define mutators
[11:47:21 PM] Anne Ogborn: that's not useful for us
[11:47:41 PM] Anne Ogborn: it's defined by the OAR and LSL files, not by anglobot
[11:48:03 PM] Anne Ogborn: instead, we need a system that can define utility
[11:48:16 PM] Anne Ogborn: and PDDL like snippets
[11:48:38 PM] Anne Ogborn: the interpreter than makes a plan that maximizes utility using the snippets
[11:49:11 PM] Anne Ogborn: because PDDL can be awkward, we give help by defining ontological relationships, like 'the hoe is carryable'
[11:50:15 PM] Anne Ogborn: and then the compiler knows what PDDL snippets about the hoe to generate (at least notionally, I assume we do some more efficient implementation)
[11:50:24 PM] Anne Ogborn: SWo, how are we like Inform 7?
[11:50:53 PM] Anne Ogborn: We have a 'wide' problem - a problem of generating much code that isn't very clever, just blorted out.
[11:51:23 PM] Anne Ogborn: that means having  a syntax that supports lots of not clever code.
[11:51:42 PM] Anne Ogborn: the pseudo english of Inform 7 demonstrably does that in it's domain
[11:52:17 PM] Anne Ogborn: also, the IDE for Inform 7 focuses on the RUN part of the debug-run-compile cycle.
[11:53:00 PM] Anne Ogborn: we need to do the same - have really great tools for setting the bots back where they were, and for changing a rule on the fly without restarting the world.
[11:53:21 PM] Anne Ogborn: (I think of games, which include script engines for the same reason)
[11:54:21 PM] Anne Ogborn: finally, if we're spendinb a lot of time writing this 'informish' language, we should have a good environment in which to do it, so the beauty and elegance of the inform 7 environment add real productiveity value
[11:55:03 PM] Anne Ogborn: Not to mention, if I'm gonna spend a year of my life making this thing I'll spend a lot of time staring into it, and I don't want it to look ugly or work awkwardly  8cD

```



## convo from skype, 7/10/2012 ##

```

[7/9/2012 11:59:30 PM | Edited 11:59:42 PM] Douglas R. Miles: 1) getting from SL initial state aligned to what the planner can inerpret
[12:00:52 AM] Douglas R. Miles: 2) perhaps mapping what differnt objects are that arent always already in the world
[12:01:10 AM | Edited 12:01:32 AM] Douglas R. Miles: such as "live fish" -> "raw fish"->"cooked fish"->"fish bones"
[12:02:09 AM | Edited 12:02:17 AM] Douglas R. Miles: obviously it only has to be as elablerate as to provide support for "what the bot can do"
[12:02:35 AM] Douglas R. Miles: right now to me it is unknown to how elaborate it might have to become
[12:02:53 AM | Edited 12:03:08 AM] Douglas R. Miles: to support descriptions of states as used by a plan definitopn
[12:03:49 AM | Edited 12:04:07 AM] Douglas R. Miles: for example "happy-well-fed-robot" might be something that makes sense as a affecct blessed onto a robot
[12:04:37 AM] Douglas R. Miles: in order to explain the result state that a plan will create
[12:05:40 AM] Douglas R. Miles: so i supposed a description of that "world rez" DSL is to be capable of at least describing state when there is no description of action yet
[12:06:29 AM] Douglas R. Miles: but i am hopefull that "description of action" will tell us the min/max scope of the world state language
[12:07:10 AM] Douglas R. Miles: i just know that those two context taken individually can be just as insteasting to design a UI/Language arround
[12:07:23 AM] Douglas R. Miles: sometimes it is easier to design arround world state descriptiuon
[12:07:33 AM] Douglas R. Miles: that has been where the most research has been done
[12:08:26 AM] Douglas R. Miles: KNext is an example of "Knowledge Representation" that is far richer than the world of action
[12:09:24 AM | Edited 12:09:28 AM] Douglas R. Miles: still tremendous overlap to the world of action
[12:11:39 AM] Douglas R. Miles: (oh yes i forgot to say that Annie had a fair recap)
[9:17:32 AM] Anne Ogborn: let me suggest that (1) should always be 'reactive', in the sense that the bot should never depend on the world state being a certain thing, but rather perceive the state of the world.
[9:30:28 AM] Anne Ogborn: I'd expect to support a condensed syntax for describing state transformations
[9:31:56 AM] Anne Ogborn: My rational for this is that it makes the system far less brittle. Experience with game AI and Rodney Brooks' work both show that this is the only way to deal with a chaotic environment
[9:32:12 AM] Anne Ogborn: the other day we came out of a restaurant, three of us together.
[9:32:32 AM] Anne Ogborn: there were 4  one dollar bills laying on the sidewalk
[9:32:48 AM] Anne Ogborn: the other people stared at them, I picked them up
[9:33:04 AM] Anne Ogborn: I'm $4 richer fro being more reactive
[9:40:27 AM] Anne Ogborn: as for making Anglobot 'run the world', I definitely think it shouldn't.
[9:42:01 AM] Anne Ogborn: That is the responsibility of other systems. Now, given the nature of SL, we may need some collusion with low level actions - eg to put the fish in the pan in SL you might detach the fish and the empty pan and attach pan_with_fish.
[9:43:08 AM] Anne Ogborn: But, a 'purer' way to do it is for the fish to present a dialog when touched. If you select 'put in pan', it sends a message to the pan to make a fish already in the pan visible, and then detaches itself.
[9:46:21 AM] Anne Ogborn: I can indeed imagine there being a construct in the language like
[9:49:18 AM] Anne Ogborn: Sitting on the fish1 poseball adds 1 raw_fish to fishing_basket average 1 hour.
[9:51:42 AM] Anne Ogborn: (should explain, these people fish by 'noodling' - holding their hands underwater, grabbing a fish and tossing it into a semi-submerged 'fishing basket' .  The fishing basket is fixed in place. They carry the fish by moving them to a 'basket'.)
[9:52:32 AM] Anne Ogborn: fishing_basket is a container.
[9:53:25 AM] Anne Ogborn: Touching fishing_basket moves all contents to basket 1 dist < 5 meters.
[9:54:24 AM] Anne Ogborn: basket is a carryable container

to me, the DSL  shouldn't describe world state at all , if by world state you mean 'the hoe is in the hut'
[7/9/2012 11:59:05 PM] Douglas R. Miles: well that "hut1" - "hut5" are type  "hut" and type "dwelling"

Yes - this I agree we should do.
In Hindi there's a 'habitual' tense for things that are always true -
kursi kamre mein hai  - the chair is in the room
kursi saman hote hain - chairs are possessions

I see us representing only habitual facts
hut1 is a hut
hut1 is a dwelling

during rain it is good to be in a hut.

to change a raw_fish into a cooked_fish
    be near fire
    have a raw_fish
    have a stick
    Touch stick
    select "Cook Fish" from dialog
    // presumably the stick plays animation of spitting the fish and cooking it
    raw_fish will remove
    cooked_fish will be in stick
    
Why only 'habitual' actions?  Because we want to be reactive. Vast experience in game AI and robotics
shows that the darnedest things will happen. So we don't want to assert fluents.

admittedly, I'm talking thru my hat - the hut might burn down. BUT, we're in a VW, not in RL. We know darn well
that the hut can't burn down unless Annie makes a burn down animation. So we have a closed universe of possible actions.

Still, we're representing 'what hillpeople know', in the sense of representing their world model.

That is, suppose the hill people believe they can bring rain by doing the rain dance. If we encode that 'knowledge', they'll
dance when they want rain. Is that 'accurate knowledge'? Depends on your world view.

Is this 'intelligent'? no. But it's a way to represent and use knowledge. Unsolved is how you obtain knowledge.
But getting this far seems to be consistent with our overall path.

Certainly the next stage beyond this is removing the hand generation of knowledge. But I'm OK with putting that off until we have
a happy representation.

I'm continuing to be puzzled by your (Douglas') clinging to the term 'world rez'.  I can't imagine the utility of a tool to generate a world. 
by a 'world rez' language I would mean editing in viewer, an OAR file, and LSL scripts.
And I really, really know that asserting 'the hoe is in the hut' is really fragile (seen this way too many times in game AI contexts).

```

# Design Process #

On Friday, 7/13/12, Douglas and Annie started doing a formal design process for Anglobot. I (Annie) have added this section to this page as a form to fill in for anglobot.

Design process comes from industrial/visual design. It's a formal set of steps that, while a bit corny or corporate feeling, can help make sure that what we build is what we wanted to build.

I'd note that, because I initiated this, it's got the stamp of 'my' concerns on it. We need to fix that. Before we work more on the acutal process, we should spend a little while on meta-process and figure out what I've left out by being blind to some of the AI-ish issues.

## Definition of problem ##

### Purpose of Building Anglobot ###

1) To implement a productive development environment for plan definitions for virtual world bot plans for less technical Subject MAtter Experts.
Attracting a community of users will prevent the 'toy problem' problem in our AI research. With real users using the system to create bots in the virtual world. Promote the sharing of user generated content.

2) Implement a BDI planner that is capable of processing the content produced by the comunity. #1 challenges the competency of #2 and allows us to learn the the nuances of the optimization problem around compiling that info. How to do that is an open question in AI.

3) To make the system practicable for real users it needs a rudimentary controlled dialog system. Additionally, our longer term goal, outside the scope of this project, is to use the BDI planner to create more robust dialog

4) To make the system practicable for real users it needs support for common actions (eg. carryable objects, moving along paths) without forcing the user to explain the details each time.

5) 1 thru 4 allows us to test mapping the domain files to a controlled dialog system. This dialog system is the fundimental langauge of the internal dialog that a single agent thinks in expressed in PAM and SAM software from 1977(aka 'Schanks hypothesis') from the book "Script, Plans and goals"

### Overall Objective ###

We can do the example scenarios.

To have a LF conversation with robot that results in demonstration of the robot predicting user mental model and using a prescribed behavour.  Each step produced in that process should be able to be read homeocentrically (in the terms of the robot using self in the LF)
Douglas R. Miles: (commitsGreeting Bot1 Annie1)
That output of those steps when converted with phases will constitute what we currently belive in pyschology to be what the mind's ear is hearing that is proof that AGI has been acheived.

LF = logicform


### Example Use Cases ###

#### Hillpeople ####

**Annie will continue to add complexity to the Hillpeople world in a way that stretches the capability of the system. Douglas and Andrew will expand the system to handle Annie's additions. So, for example, Annie might add a tool that's useful for many tasks (a basket) but have only one available. When the system can handle this situation, she adds a second basket.**

#### Afghan Village ####

**A notional military use case is to train users to interact with Afghan civilians in a culturally appropriate way. Such a system would place heavy demands on psychosociocultural models (the americans are resented as outside interlopers) and on dialog.**

**Can we do Afghan Village with our target design?  Should we be able to?**

#### 18th Century London ####

**A history teacher wants to recreate a portion of 18th century London, including bots who can interact with visitors and explain their actions.**

```
Ryan's Use Cases

	+ The Revival
	
	Creation of The Revival, a video game based on a multipolar world
	with complex political coalitions and interagent cooperation.
	
	+ Mini-Games
	
	Creation of minigames and playable demos to test out and develop
	aspects of The Revival.
	
Jess's Use Cases

	+ Jason/Perl Integration

	Implement a Perl interface to Jason.

Joey's Use Cases

	+ AIML Cyn Cyc bot

	+ Network Security?

Sara's Use Cases

	+ funfunctions Math Tutor

	Virtual world for protoype of interactive RL game that teaches Navigation.

CW's Use Cases

	+ Game Development / Programming Instruction

	Provide an environment for students to learn programming skills
	and develop games.

Andrew's Use Cases

	+ Free Life Planning Coach
	
	The free life planner called action planner intends to be a
	decision support system for analyzing a person's life and helping
	them to improve their wellbeing.  It models factors such as
	health, food security, financial security, and provides tools to
	help manage the user's plans and commitments.  It is heavily
	related to the cell-phone based interactive execution monitor.
	
	My goal is to model, within a symbolic environment, these factors
	and to build agents that are good at solving the associated
	problems, with the goal of making these tools available open
	source to the public and developing polished interfaces.
	
	+ Homeless at CMU survival simulator
	
	While pursuing the development of AI I was forced into a
	difficult position where I was living homeless at CMU.  During
	that time I was pursuing the development of a PDDL planning
	system for modeling the state of the world and planning my
	actions within it.  I failed to develop the system in time to be
	useful to me there, but I made progress.
	
	I regard the problem as unsolved and wish to make a video game
	simulation out of it in order to complete its development.  I
	intend it to lead to solutions that can help the homeless (and as
	such it is related to the free life planning coach).  It can also
	be a fun game.  The idea is to develop a conformant plan that
	satisfies the requirements of life of the simulated homeless
	person.  Future work may involve adding deontic logics to
	constrain the morality.  Ideally it would provide the homeless
	the means of survival while meeting the needs of the rest of the
	community - with all conflicts being analyzed.
	
	This simulation involves the use of exogenous events.  For
	instance, a person may have taken up a hidden sleeping place, and
	someone may exogenously walk into the room, pinning the user
	down.  We might eventually be able to simulate the motivations,
	beliefs, and strategies of the various agents.
	
	+ SPSE2 Cell phone based task management
	
	I am developing a cell phone based system which includes a speech
	interface, in which the user can assert new goals, and a planning
	component and interactive execution monitor which walks the user
	through the plans.  This is a tactical system and works with the
	life planner to achieve long and short range planning.  The idea
	is to model the environment to the degree that, wrt certain
	criteria, plans are more or fully optimal.
	
	The system involves location logic, which is similar to Douglas'
	SecondLife2Kif converter, and includes predicates which explicate
	the users trajectory and provide material for handlers such as
	detecting that one is exiting a movie theatre and so could be
	expected to want to unsilence their phone.
	
	The system is explained here:
	
	http://frdcsa.org/~andrewdo/writings/Temporal-Planning-and-Inferencing-for-Personal-Task-Management-with-SPSE2.pdf
	
	The use case here is to use SL to simulate the real world and
	debug the software by running the actual Android software on an
	ARM emulator, and running the FRDCSA software on a suitable
	remote machine.
	
	+ Ireland History Game
	
	Since the Battle of Kinsale on Christmas Eve of 1601, the Gaelic
	way of life seen somewhat of a decline.  The goal of the Ireland
	History game is model with AI technology various aspects of Irish
	History and Culture, and become a resource for those seeking to
	learn about the past and promote the Gaelic revival.  As such,
	lots of Irish language integration, such as the Gramadoir grammar
	checker, etc, tools for learners of the Irish Language, virtual
	libraries containing public domain works regarding the Irish
	langauge.  Modeling of Kings and Bards as AI agents that are
	knowledgeable in what they would be knowledgeable such as their
	lineages and territories.  Theorem proving regarding Irish
	History.  Role playing and interaction with NPCs.  Historical and
	ahistorical simulations/games.
	
	+ InContext/InConcert like Agent Based OS
	
	For some time I have been trying to implement agents that can
	execute system administration tasks as well as compose software.
	The use of natural language processing tools like KNext to
	develop models of what actions agents can take (when reading
	documentation), the automatical construction of PDDL domains from
	text using tools like the one mentioned here:
	
	Saeed Hassanpour, Martin J. O’Connor, Amar K. Das, “A Framework
	for the Automatic Extraction of Rules from Online Text”,
	Proceedings of the 5th International Symposium on Rules, pp
	266-280, Barcelona, Spain, 2011.
	
	They should handle plan failure well and make use of numerous
	tools in the agent literature, perhaps including the Plexil
	system.
	
	+ Cyc Integration with the FRDCSA
	
	While the FRDCSA has a few CycL modules, the integration is much,
	much less than the integration witnessed with dmiles' work.  The
	goal here is to play catchup and get access to such tools.
	
	+ Agent-based NLU
	
	Formalizing text using various tools, but specifically,
	developing a system to interpret text artifacts based on a
	researching defeasible agent.
	
	+ General Environment / Agent Ontology and Instantiator
	
	Translation between domains, such as blocksworld, and
	other environments e.g. integration with GGP game rules,
	Jason environments, SL, PDDL domains etc.  Multiple
	agents/reasoners, such as Cadiaplayer, Jason, OpenCog,
	LPG-TD, Soar, Oscar, Golem, and (an RTS solver). Ontology
	models which reasoners are applicable to which
	environments.  System to formalize descriptions of worlds
	into concrete worlds based upon NLU and argumentation
	frameworks.
	
	+ Setanta Instrusion Prevention System
	
	Similar to CycSecure (I'm guessing).  Automatic asset
	inventorying, transport to different machines, uses previously
	mentioned system administration to maintain network defenses.
	
	http://frdcsa.org/frdcsa/internal/setanta/index.html

	+ Jason/FRDCSA Integration

	Implement a Perl interface to Jason.
```

### Constraints ###

What constraints and conditions are there on the solution?

## Analysis ##

### Fun ###

What parts of the task do each of us see as fun? Boring? Difficult?

What are our secret agendas?

**Annie sez her secret agenda is to make beautiful things that are a delight to use, to get street cred in the VW community, and to learn a s`**`tload about software from Douglas and Andrew. She told a friend this was her year to learn to program**

### Research ###

What research tasks should be performed before proceeding with the design?

### User Interviews ###

Can we interview potential users?

### Feasibility ###

Can we do this?

### Planners ###

We're operating at the edge of known algorithm space. What constraints
does that impose?

Perhaps this phase involves some prototype coding.

### Similar Projects ###

Identify software (and non software?) we can take ideas from.

Identify software we can take code from.

### Why Did Others Fail? ###

Other people have done this sort of thing, why did they fail?

### Needed design features ###

  * Encourage sharing of content
  * No restart of bots

**who controls the bots?**

### Tradeoffs ###

What are the important tradeoffs in the design?

### Method ###

How will we build it?

How will we test it?

How will we package and deliver it?

What OSes and modes (headless, interactive) and such are needed?

How will users install it?

How will users learn to use it?

### Support Services ###

What support services are needed?

## Ideation ##

Whoot - finally we get to spew out ideas.

Should we separate this into two pieces, one for the user facing design, and one for the underlying algorithms?

Our objective is to generate MANY possible designs.

### Brainstorming ###

A time limited period where we avoid owning ideas and avoid criticizing ideas. Brainstorming should work on single questions.


> Focus on quantity: This rule is a means of enhancing divergent production, aiming to facilitate problem solving through the maxim quantity breeds quality. The assumption is that the greater the number of ideas generated, the greater the chance of producing a radical and effective solution.
> Withhold criticism: In brainstorming, criticism of ideas generated should be put 'on hold'. Instead, participants should focus on extending or adding to ideas, reserving criticism for a later 'critical stage' of the process. By suspending judgment, participants will feel free to generate unusual ideas.
> Welcome unusual ideas: To get a good and long list of ideas, unusual ideas are welcomed. They can be generated by looking from new perspectives and suspending assumptions. These new ways of thinking may provide better solutions.
> Combine and improve ideas: Good ideas may be combined to form a single better good idea, as suggested by the slogan "1+1=3". It is believed to stimulate the building of ideas by a process of association.[1](1.md)


### Exhaustion ###

## Solution Selection ##

Select a solution. If we're lucky we've arrived at this point without having strongly 'owned' solutions (tell the Ali Baba Slots story here).

## Implement ##

### Engineering Requirements Document ###

This doesn't mean final code. It means draw up a detailed (enough) design document gthat we're clear what our solution is.

### Prototypes of Major Algorithms ###

Some toy code to demo specific algorithms.

## Critique ##

Examine our solution critically. Did something get left out? Will it work?

### User Review ###

Show our design to potential users. Will it fullfill their needs?

### Peer Review ###

Show our design to peers in the AI community. Defend our design.

### Compile Design Casualties ###

By this point we should have some pretty clear ideas how our design has areas wanting improvement.

We have probably identified new design requirements.

We feed these new requirements into the top and loop again (usually the range of designs is much narrower on subsequent passes, and the whole process much faster).

## Acceptance ##

We're all happy with the design. We build it. The world applauds, and/or the project dissolves in blame and recriminations.