### Introduction ###

CYC has a notion of per-understood content: Agents, Hair, Walking, Being Surprised, PortableObjects like Shovels and Pants.

Also has a notion for committing actions: PointingAtSomething, Movement-TranslationEvent, SittingOnFurnature.

Obviously these things have been designed to reflect a WorldLikeOurs. And OpenSim/Secondlife was also designed to be as well.

The purpose of the CycWorldModule is to easily use and understand (aline) existing OpenSim and SecondLife worlds with prebuilt content into CYC KBs.

Video games are not represented at some atom by atom basis (good or bad Knowledge Engineering).

A  bird flying in the air is only a transparent cube with an animated gif.

#### That bird has coded into it: ####

  * A Name: "filthy bird 777"
  * A natural flying pattern (albeit only a low circle around the sky-box) "BoringBirdFlyPattern.lsl"
  * Uses texture: "AnimatedWings.gif"
  * Drops poop at random intervals ("rezzes" a new object "poop" effected by gravity) contains: "LeaveScatScript.lsl"
  * If the bird is contacted by another object it squawks in complaint: contains: "DontTouchMe.lsl"

## SimCycifier's job is to pick up on some of these facts and assert them into CYC ##

  * (isa Bird-Individual777 AnimatedAgent)
  * (simShape Bird-Individual777 TransparentCube)
  * (simNamed Bird-Individual777 "filthy bird 777")
  * (simLocatedAt Bird-Individual777 (PointFn 125 128 30))
  * (texturedWith Bird-Individual777 "AnimatedWings.gif")
  * (containsScript Bird-Individual777 "LeaveScatScript.lsl")
  * (containsScript Bird-Individual777 "DontTouchMe.lsl")
  * (containsScript Bird-Individual777 "BoringBirdFlyPattern.lsl")

### CycL KE's can (but not required to) ###

  * "LeaveScatScript.lsl" => Animal, performsFrequentlyFn whatnot
  * "DontTouchMe.lsl" => make assertions that will describe cause-effect
  * texturedWith -> PartlyTangbleObject
  * Bird-Individual777 and is probably #$isa #$Bird a low amount cleanliness based on its name.
  * TransparentCube -> means the simShape is not important

### CYC's job is to deduce: ###

  * try to avoid being under the bird
  * may throw stones at the bird to make it squawk.

Does this sound like a lot of KE work?!

Easier to map existing content then it is the create new content, since you **still** have map CYC.

### DOUGLAS TODO: (Postponed - but can be undone) ###

  * Ontologically Engineer a House (Room by Room) With objects and Salient scripted actions with agendas.
  * Extend CycWorldModule to "make it so" n OpenSim/SecondLife.

Strangely,  ther first is harder than it sounds. (Has Cycorp already done this)

Second is easier. (OK, not easy!  But only 80-160 hours of  work).. But, requires the first to be done to make it worth the while to me.