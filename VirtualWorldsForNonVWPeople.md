#A brief introduction to virtual worlds

# Introduction #

Many of Cogbot's users are not from the traditional virtual world communities, but are from an AI background. So I thought it might be useful to give a very brief introduction to the SL protocol virtual worlds.

## What's a Virtual World? ##

A virtual world is a simulated place populated by avatars who represent a mix of real (human) users and programmatic 'bots'.

Unlike a computer game, the goal of a virtual world is usually social interaction. Like social interaction in the real world, the presence of 'something to talk about' helps lubricate interactions.

Unlike a computer game, the creators of a virtual world provide a relatively sparse environment and rich tools for modifying that environment.

The term 'virtual world' depends a bit on context.  We'll concentrate on 'SL Protocol' worlds, and from now on when I say VW I mean an SL Protocol virtual world.

SL Protocol is the protocol for the free virtual world Second Life http:www.secondlife.com offered by Linden Labs. This protocol is the de facto standard for communicating with a virtual world server.

## How Can I experience a virtual world? ##

You'll need 3 things: viewer software, a grid, and an account on that grid.
You can get a free account on the world's largest grid, Second Life, at http://www.secondlife.com You can download and use their viewer.

The Imprudence viewer http://blog.kokuaviewer.org/ has a nice tool for setting a grid name. This is very useful when working with multiple grids.

If you would like to experience the opensim server, several companies operate commercial opensim based grids. https://www.avination.com/ gives free accounts.

If you're an early adopter of Cogbot, Annie may give you an account on the Pathways grid used to develop Cogbot.

It's easiest to learn on someone elses grid before trying to set up your own. That said, setting up opensim to operate a standalone grid is not complex for the average Cogbot user.

## The Big Pieces ##

### The Objects Of The World ###

  * Grid - The entire world. Could be as small as one _region_, or as large as a county (Second Life has 38,000 regions, a land area about as big as the suburban county I live in).

  * Land - The ground surface. The land is divided into a grid of _regions_ (sometimes called _sims_ ) 256 meters on a side. The land appears continuous - you can walk across the boundary. A collection of regions operating as a single 'world' is a _grid_.

  * Inventory - everything a user 'owns' is passivated into their 'inventory'. It can be copied from inventory to the world, a process called 'rezzing', or back, a process called 'taking'. So, if I tire of my castle in Second Life, I can take it back to inventory, making it disappear from the world, rez my beach house from my inventory, and invite my friends over for a little beach party. When scripts are passivated their state is preserved

  * Assets - textures, sounds, notecards (a modifyable text file), scripts (see below), clothing, etc. These don't directly appear in world but influence objects and avatars (eg a prim might have a texture on it's face, or play a sound when touched).

  * Prims - A prim is the smallest manipulable unit of geometry. SL geometry is based on parametric solids. So one doesn't have polys, like in a mesh system, but rather a box, sphere, torus, etc. These shapes are essentially those of a square, circle, or triangle, swept along a line or circle.

Prims are edited with tools built into the viewer. The organic editing tools give VW's a very dynamic feel.
There is a way to import traditional meshes, but newcomers should learn to use prims before trying to force fit the SL geometry into preexisting poly mesh tool pipelines.

  * Objects - An _object_ is a _linked_ set of _prims_. Objects move as rigid bodies. So, typically, a chair would consist of a number (anywhere from 3 to 20 or so) prims. One prim is the _root_. There is no hierarchy, but the children have a stable _link_ _number_.

  * Scripts - Each prim can contain other objects and assets. A script living in a prim produces programmed behavior. A script can affect the prim it's in or affect avatars - for example, most chairs have a script that responds to a 'sit' event by playing an animation that makes the avatar sit naturally in the chair. Scripts have nearly unlimited control of objects, but limited control of avatars.

  * Windlight - the trade name of the environmental controls that produce day and night

  * Avatars - An _agent_ is a human or bot's presence in the world. It's visual appearance is an avatar. Avatars can wear so called _real_ clothing, which is simply a texture on the body mesh, or _prim_ clothing, which is an object 'attached' to the avatar mesh. So a pair of jeans would likely be 'real' clothing, but a carmen miranda hat would be prim clothing. Avatars can play animations, which are BVH format animations of the rigged skeleton. So, for example, a dance floor would play an animation on the avatar to make it dance.

**HUD - To modify the user interface of the program, one can build (from prims) a custom user control. This can be attached to the viewer's content area, much as if it were glued to the 'inside' of the screen.**

### Implementation ###

The user runs client software called a _viewer_.  Viewers tend to fall into three catagories - graphical viewers, used for normal work in the world, text viewers like _Radegast_, that don't display the world but allow the avatar to talk and exchange objects, and _bots_, which provide some sort of programmatic control of an avatar. Cogbot can reasonably be called a 'bot on steroids'.

The other end of the system is the server. Technically, a viewer makes a number of connections to different services, but at this level of detail we can think of 'the server'.  There are two implementations of the SL protocol server. One is the proprietary servers used by Linden Labs to operate the Second Life grid. The other is the open source project http://www.opensimulator.org

Each _region_ is simulated by a process called the _simulator_. Traditionally, one hardware 2U server served one region. With better hardware, it is now common to run multiple simulators on one server. Opensim allows a single simulator process to handle multiple regions. But still, the basic unit of control is the region.

Every object and avatar must be represented by an object (programming sense) in each simulator that 'knows' about the object. So when an object or avatar crosses a region boundary a process of handoff occurs. Particularly complex, scripts need to hand off state.

In practice, this means that users tend to build things within a single region, since there's a perceptable 'bump' when crossing the boundary. An exception is large event auditoriums, which are built on the 'apex' where 4 regions meet so that the load of simulating the audience is spread over 4 simulators.

Assets are served from a seperate 'asset server'.  Every asset has a UUID. So, for example, a block sitting in a region with some wood texture on it would have a representation in the region's simulator that had a UUID for each face of the block. Assets are immutable, though this is occasionally hidden by the UI (eg you can modify a notecard - but you're making a new version under the covers).
Objects in inventory are also assets. (objects in world have a UUID, but it is not their inventory UUID).

Assets have a permission system that is slightly unusual. Every asset and object has a set of permissions for the current owner, and a set for the next owner. When the object is transferred the next owner perms are copied to the current perms. The current owner can change the next owner perms, but not the current perms.

The permissions are to copy, modify, and transfer the object. A common notation for perms is to use upper case for 'allow' and lower case for 'deny' - so CmT means allow copy and transfer but not modify (another common scheme is positional, with y and n).
For example, Bob makes a box with a script in it. As creator his box and script are CMT.

He sets the box next owner perms to CMt. He sets the script to be cmT. He then gives the box to Sally.

Sally can make as many copies of the box as she wants, but only one copy will contain the script. She can resize the box. But she cant give the box (with its script inside) to Fred, because she doesn't have transfer permissions.

She can take the script out of the box to inventory, but can't look at it's contents. She can give the script to Fred, but when she does so her copy will disappear.

This perm system allows users to create objects that act like real objects (if I give you my bicycle I don't have a bicycle, and you don't have two bicycles), like files (I give you a pdf file, I still have it), and control whether the object can be examined deeply or changed.

### Scripts ###

A script is an asset that can be placed in a prim.

Scripts are written in LSL,  http://wiki.secondlife.com/wiki/LSL_Portal
a small state/event based language. The source is sent to the server, compiled to CLI (.net) bytecode and executed. Sandboxing is done simply by selecting the desired API's (and limiting memory/cpu quotas).

A typical LSL script.

```

// This turns the prim red the first time it's touched
// green the second, and so on

// colors are represented by vectors, which are a fundamental type
vector GREEN = <0.0, 1.0, 0.0>;  
vector RED = <1.0, 0.0, 0.0>;

// we start in default state - note the irregular syntax
default {
// on state change all queued events are flushed, the exit handler
// is called on the old state, then the entry handler is called on
// the new state
      state_entry()
      {
           llSetColor(GREEN, ALL_SIDES);
      }

      // handle stopping the 'touch; (left click) action
      // the in world equivilent of mouse up
      touch_end(integer n)
      {
           state red;  // change state
          // code here never executes
      }
}

state red
{
      state_entry()
      {
           llSetColor(RED, ALL_SIDES);
      }

      // handle stopping the 'touch; (left click) action
      // the in world equivilent of mouse up
      touch_end(integer n)
      {
           state default;  // change state
          // code here never executes
      }
}

```

Each script is single threaded and has it's own thread. All communication is by messaging.

### Communication ###

SL has many ways to communicate. Objects can participate in all but voice. Avatars can participate in chat, IM, OwnerSay, and voice.

Chat

Chat is typing at each other.

Chat is done on numbered channels. Every 'chat' message has a channel number and an origin location. Avatars listen only to channel zero and the debug channel, #7FFFFFFF. Avatars my produce chat on any nonnegative channel by starting their message with '/7 this message said on chnnel 7'.

Avatar chat may be _whispered_, _said_, or _shouted_, and will be received by listeners with 10, 20, or 80 meters respectively.

Objects may listen on any channel (including zero), and may whisper, say, or shout. Objects may also 'region say' to the entire region on any non zero channel, or ownersay to their owning avatar only (the normal way to print debug messages).

Conventionally, positive channels are reserved for avatars to control objects, while negative channels are for object to object communication.

IM

Avatars and objects may 'IM' avatars anywhere in the grid. This chat appears in a separate window.

Link Messages

Objects may send messages via the LSL api llMessageLinked to communicate with other scripts within the same prim or same linkset.

This is a broadcast system.

Link messages and multiple scripts are the most common means of encapsulation.

Awkward as stringified messages appear at first, I've found I get a very large amount of code reuse.

Script Injection

Since an object can give an asset in it's inventory to another object, it can give a script. To prevent the obvious security hole of accepting arbitrary scripts, given scripts only run if the giver suppies a secret PIN code (set on the receiver by another LSL API).

Script injection is very useful for implementing 'decorator' type. It's even fairly common for a script to change the behavior of an object by injecting a script that deletes a script, then takes over it's function.

Gasp! Self modifying code!  (wait til the first time you llDie() an object, deleting your only copy of the code along with it).

### Off world communication ###

HTTP

Objects may issue HTTP requests and receive replies. The length is limited to about 1500 chars.

They may also request a temporary (they last a week or so on SL) URL and act as a server.

Notecards

LSL can read, but not write, a notecard in it's inventory. This is the conventional way to store static data.

XML-RPC

Linden Labs implemented an XML-RPC system that never worked well and is deprecated.

Email

Objects can send and receive email. This is quite useful for low rate 'wake up' type communication. For example, a commercial advertising company with hundreds of ads spread throughout SL could update all it's boards by email. At my university employer, we have 'help' objects students can touch to get help. These send an SMS to my phone via SMTP

A note - the Pathways testbed server doesn't have an email gateway set up.

libOMV

Finally, the SL protocol can be implemented. This is what Cogbot does.

A single library, libOMV, partially implements the protocol (it's a complex protocol). This library is in C#. So the core of Cogbot is written in C#.

## User Names ##

User names on grids are

## Final advice ##

Obviously this page barely scratches the surface. Like any other computing environment, the best way to learn is to dive in and make things.

Until we outgrow the ability to support this, contact Annie for an account on the Pathways server, which is our Cogbot testbed server.

Have fun!



