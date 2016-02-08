#Roadmap of Cogbot

# Long Term Roadmap #

This is our long term strategy. We can change things on
the ongoing roadmap fairly easily, but should try to 'stag the course'
with these items

  1. Binary release - roughly May 15. Have a prebuilt binary release of Cogbot available for download from google code.

  1. Afghan Village - roughtly July 1. A simple demo area with bots moving around, doing activities, and responding to each other and to humans in the area - a demo of basic cogbot functionality.

  1. Public Release - roughly Sept 15.  Having all the existing components working well enough, and cogbot well enough packaged, documented, etc. that we can begin to attract users, make public announcements, and be 'alpha'.



## needed for public release ##

#### COMPLETED ####
  * Build system sane
  * multiple AV's
  * pathfinding (to level we've seen)
  * headless mode
  * mono - works fine on Redhat with BUGs on Ubuntu
  * AIML interpreter needs to be working - (need Docs?)
  * prolog API to botvars,sysvars

#### DELAYED ####
  * Complete regression test coverage
  * setup and run pathfinding unit tests (good enough)
  * Bug list reasonably clean
  * Sims working
  * some tale-spinnish thing
  * termified botCmd returns

#### TODO GENERAL ####

  * installer finished - INPROGRESS
  * PR plan
  * website
  * Example files made
  * Some level of documentation of everything
  * Every component currently in the installer's component list must work well enough to be useable.


#### TODO ANNIE ####
  1. move hillpeople to botvars
  1. make hillpeople do various actions (hunt, fish, etc) triggered by some debug method
  1. make hillpeople trigger actions from aiml and be able to answer questions like 'what are you doing?'  'what do you think Otopopo is doing?'

#### TODO DOUG ####
  1. get AIML module ready for installer
  1. easy deployment - Installer needs Cogbot integration
    * Make a publicly downloadable bundles for cogbot (small version )     then an overlay pack that has all the chatbot stuff in it
  1. ABCL interpreter working
  1. document manifesto MORE
  1. Overview of current set of services: like botvar namespaces - INPROGRESS
  1. BotCommand parser using CommandInfo to "parse" the commands separately

# 0.2a release #

  1. fix botconfig architecture to support installs with multiple projects

# The ongoing roadmap (1-year) #

Please edit this only after some consensus on roadmap

#### TODO GENERAL ####
  1. continue the development and support cogbot
  1. Pone file compiler
  1. PDDL executor for cogbot
  1. Look for other improvements to the command language
  1. Look to incorporate Cyc inference if needed for Ontology support
  1. Develop the hillpeople example into a full fledged platform for writing 'real' AI.
  1. Make building afghan village/hillpeople/18th cent london projects easy.
    * It will do this by providing an expressive compiler/interpreter
    * Develop a IDE environment that makes declarative language for relationships, plan fragments, and utility, and an efficient executor that will behave intelligently within the scope of the microenvironment.
    * We will develop a microenvironment in which all significant actions are constrained by the world with realistic constraints (eg you must have the hoe to hoe the cornfield).
  1. publicly announce cogbot and attract a community as a way of forcing what has til now been a research type project into a more streamlined, finisshed project suitable as a building block. Additionally, public announcement will attract other resources (programmers, builders, and institutional users).

### ANNIE TODO ###


### DOUG TODO ###
  1. Progress to ProgramK Prolog AIML bot