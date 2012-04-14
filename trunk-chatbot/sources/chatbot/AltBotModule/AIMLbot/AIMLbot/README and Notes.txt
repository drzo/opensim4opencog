Implements an alternative AIML processor to the RTPBot.
Basically the complete console bot is wrapped in the Servitor class. 
This is required since the bot is proactive and needs its own independent threads.
The sevitor is added to RTPBot. Check for "useServitor" to find hook points.

Some thing of note include:
- Behavior trees
        // Kinda based on the idea at ...
        // http://www.garagegames.com/community/blogs/view/21143
- existing AIML template side wrapped inside <task>
- <say> will send results from contents to the sayProcessorDelegate
- FSM's
- Cron/Timer based behavior triggers
- Chemical drive simulator
- Behaviors are sensitive to multiple test conditions
- Can use a SAT solver for determining test conditions

Licencing of modules remain the same as their originals, with BSD otherwise.