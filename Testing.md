#How to use the test framework

# Introduction #

Testing a virtual robot requires unusual techniques when compared to the average web application. However, the theory remains the same. Tests must be:

  * Fast  - many tests require actions by the bot, so we're as fast as we can be
  * Automatic - it's difficult to automatically test for various actions by thebot.
  * Independent - the simulator tends to retain state. Logging the bot on takes 30 seconds or more, so fast and independent tend to conflict. In practice, we leave the bot logged in.
  * Repeatable - Physics related items are not repeatable, and many bugs are due to message failures, so repeatablity is a constant struggle.


# How To #

To run the path finding tests:

load cogbot/test/pathfindplayground.oar into an empty sim

run bin/runcogbot.pl

wait til the bot loads

Move the bot into the sim if necessary

query

tpf.

# What we test #

The test framework checks the time taken and that all the asserts happen.

# Setting up tests #

The bot listens for local chat. It tries to execute this as prolog.

Positive asserts are green blocks named 'needed'
To set up a new positive assert put the name of the test, a comma, and the assert number (starting at 1) like

other\_side\_wall,1

in the object description and reset the scripts in the object.

Negative asserts are named forbidden.
put the name of the test in the description and reset the scripts

Both of these objects are phantom, and hence not 'seen' by the bot

obstacle

is a non-phantom object the bot must not collide with

allowed

is a non-phantom object the bot may collide with

